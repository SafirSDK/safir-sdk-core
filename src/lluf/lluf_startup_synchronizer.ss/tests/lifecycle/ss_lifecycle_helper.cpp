/******************************************************************************
*
* Copyright Saab AB, 2026 (http://safirsdkcore.com)
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/

/**
 * One process that uses a StartupSynchronizer, with enough knobs for
 * run_lifecycle_tests.py to build the interesting multi process situations out of
 * it. Everything it does is reported on stdout, one event per line, so that the
 * driver can check what happened across all the processes it started.
 *
 * Usage: ss_lifecycle_helper --name <resource> [options]
 *
 *   --hold                 wait for a line on stdin before letting go of the
 *                          resource (the default is to let go immediately)
 *   --create-delay <ms>    spend this long inside the Create callback
 *   --destroy-delay <ms>   spend this long inside the Destroy callback
 *   --exit-in-create <n>   leave the process with _exit(n) from inside Create,
 *                          i.e. behave like a process that dies after it has
 *                          been given the job of creating the resource
 *   --hang-in-create       never return from Create, so that the driver can kill
 *                          the process at the worst possible moment
 *   --resource <path>      pretend the shared resource is this file, and check the
 *                          invariant that matters across all the processes: the
 *                          generation a process is using must not be replaced or
 *                          removed under it while it is using it
 *   --outlive              use the resource and let go of it again, and only then
 *                          (when the driver says so) start a second instance that
 *                          was constructed before any of that happened. Implies
 *                          --hold. This is the situation where the internals of an
 *                          already finished resource get reused.
 */

#include <Safir/Utilities/StartupSynchronizer.h>

#include <boost/filesystem/fstream.hpp>
#include <boost/filesystem/operations.hpp>

#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <exception>
#include <random>
#include <sstream>
#include <string>
#include <thread>

namespace
{
    void Say(const char* const what)
    {
        std::printf("%s\n", what);
        std::fflush(stdout);
    }

    void Sleep(const int milliseconds)
    {
        if (milliseconds > 0)
        {
            std::this_thread::sleep_for(std::chrono::milliseconds(milliseconds));
        }
    }

    struct Options
    {
        std::string name;
        bool hold = false;
        int createDelay = 0;
        int destroyDelay = 0;
        int exitInCreate = -1;
        bool hangInCreate = false;
        bool outlive = false;
        std::string resource;
    };

    /**
     * Stands in for the thing a real Synchronized would be creating, so that the
     * tests can check the one invariant that spans every process involved: while a
     * process is using a generation of the resource, nobody may replace it with a
     * new one or take it away.
     *
     * The generation is just a random number written to a file. Nothing here makes
     * any assumption about timing, so it catches a violation whenever it happens,
     * however narrow the window was.
     */
    class Witness
    {
    public:
        explicit Witness(const std::string& path)
            : m_path(path)
        {
        }

        bool Enabled() const {return !m_path.empty();}

        /** Write a brand new generation, overwriting anything a dead process left. */
        void Created()
        {
            if (!Enabled())
            {
                return;
            }
            std::random_device device;
            std::ostringstream ostr;
            ostr << device() << "-" << device();
            m_generation = ostr.str();

            boost::filesystem::ofstream file(m_path, std::ios::out | std::ios::trunc);
            file << m_generation;
            if (!file.good())
            {
                Complain("could not write the resource file");
            }
        }

        /** Note which generation we are using. */
        void Used()
        {
            if (!Enabled())
            {
                return;
            }
            m_generation = Read();
            if (m_generation.empty())
            {
                Complain("was told to use a resource that does not exist");
            }
        }

        /** Still the generation we were given? */
        void Check(const char* const when)
        {
            if (!Enabled() || m_generation.empty())
            {
                return;
            }
            const std::string actual = Read();
            if (actual.empty())
            {
                Complain(std::string("the resource was destroyed while we were using it (") + when + ")");
            }
            else if (actual != m_generation)
            {
                Complain(std::string("the resource was replaced by another generation while we "
                                     "were using it (") + when + ")");
            }
        }

        void Destroyed()
        {
            if (!Enabled())
            {
                return;
            }
            Check("in Destroy");
            boost::system::error_code ec;
            boost::filesystem::remove(m_path, ec);
            m_generation.clear();
        }

        bool Violated() const {return m_violated;}

    private:
        const std::string Read() const
        {
            boost::filesystem::ifstream file(m_path);
            if (!file.good())
            {
                return std::string();
            }
            std::string content;
            file >> content;
            return content;
        }

        void Complain(const std::string& what)
        {
            m_violated = true;
            std::printf("ERROR resource invariant broken: %s\n", what.c_str());
            std::fflush(stdout);
        }

        const std::string m_path;
        std::string m_generation;
        bool m_violated = false;
    };

    class Instance : public Safir::Utilities::Synchronized
    {
    public:
        explicit Instance(const Options& options)
            : m_options(options)
            , m_witness(options.resource)
        {
        }

        int creates = 0;
        int uses = 0;
        int destroys = 0;

        /** Check that our generation is still the live one. */
        void CheckWitness(const char* const when) {m_witness.Check(when);}
        bool WitnessViolated() const {return m_witness.Violated();}

    private:
        void Create() override
        {
            Say("EVENT CREATE");
            Sleep(m_options.createDelay);

            //Written before we get the chance to die below, so that the process
            //that recovers has to cope with a resource that a dead creator left
            //half finished.
            m_witness.Created();

            if (m_options.hangInCreate)
            {
                Say("EVENT HANGING_IN_CREATE");
                for (;;)
                {
                    Sleep(1000);
                }
            }
            if (m_options.exitInCreate >= 0)
            {
                Say("EVENT EXITING_IN_CREATE");
                //Deliberately not a clean exit: no destructors, nothing cleaned
                //up. This is what a process that is killed while it is creating
                //the resource looks like to everybody else.
                std::_Exit(m_options.exitInCreate);
            }
            ++creates;
        }

        void Use() override
        {
            Say("EVENT USE");
            m_witness.Used();
            ++uses;
        }

        void Destroy() override
        {
            Say("EVENT DESTROY");
            Sleep(m_options.destroyDelay);
            m_witness.Destroyed();
            Say("EVENT DESTROY_DONE");
            ++destroys;
        }

        const Options& m_options;
        Witness m_witness;
    };

    const Options ParseArguments(int argc, char** argv)
    {
        Options options;
        for (int i = 1; i < argc; ++i)
        {
            const std::string arg(argv[i]);
            const char* const next = (i + 1 < argc) ? argv[i + 1] : nullptr;

            if (arg == "--name" && next != nullptr)
            {
                options.name = next;
                ++i;
            }
            else if (arg == "--hold")
            {
                options.hold = true;
            }
            else if (arg == "--create-delay" && next != nullptr)
            {
                options.createDelay = std::atoi(next);
                ++i;
            }
            else if (arg == "--destroy-delay" && next != nullptr)
            {
                options.destroyDelay = std::atoi(next);
                ++i;
            }
            else if (arg == "--exit-in-create" && next != nullptr)
            {
                options.exitInCreate = std::atoi(next);
                ++i;
            }
            else if (arg == "--hang-in-create")
            {
                options.hangInCreate = true;
            }
            else if (arg == "--resource" && next != nullptr)
            {
                options.resource = next;
                ++i;
            }
            else if (arg == "--outlive")
            {
                options.outlive = true;
                options.hold = true;
            }
            else
            {
                std::printf("ERROR unknown argument '%s'\n", arg.c_str());
                std::fflush(stdout);
                std::exit(2);
            }
        }

        if (options.name.empty())
        {
            Say("ERROR --name is required");
            std::exit(2);
        }
        return options;
    }

    /** Wait for the driver to tell us to carry on. */
    void WaitForDriver()
    {
        char line[256];
        if (std::fgets(line, sizeof(line), stdin) == nullptr)
        {
            Say("EVENT STDIN_CLOSED");
        }
    }

    /**
     * An instance that was constructed before another one created *and destroyed*
     * the resource, and that only starts afterwards. Both share the same internal
     * state for this resource, so this is what it takes to make a process run the
     * protocol on internals that have already been through a full life cycle.
     */
    int RunOutliving(const Options& options)
    {
        Instance outer(options);
        Instance inner(options);

        try
        {
            Safir::Utilities::StartupSynchronizer ssOuter(options.name.c_str());

            {
                Safir::Utilities::StartupSynchronizer ssInner(options.name.c_str());
                ssInner.Start(&inner);
            }
            Say("EVENT INNER_DONE");

            //The driver now starts another process, which takes over the resource.
            Say("READY_FOR_RESTART");
            WaitForDriver();

            ssOuter.Start(&outer);
            Say("READY");
            WaitForDriver();
            outer.CheckWitness("before letting go");
        }
        catch (const std::exception& exc)
        {
            std::printf("ERROR %s\n", exc.what());
            std::fflush(stdout);
            return 1;
        }

        std::printf("RESULT created=%d used=%d destroyed=%d\n",
                    outer.creates, outer.uses, outer.destroys);
        std::fflush(stdout);
        return (outer.WitnessViolated() || inner.WitnessViolated()) ? 1 : 0;
    }
}

int main(int argc, char** argv)
{
    const Options options = ParseArguments(argc, argv);
    Instance instance(options);

    if (options.outlive)
    {
        return RunOutliving(options);
    }

    try
    {
        //Note the declaration order: the synchronizer has to go out of scope
        //before the instance it is reporting to.
        Safir::Utilities::StartupSynchronizer ss(options.name.c_str());

        //Announced so that the driver can tell the difference between "has not
        //got there yet" and "is waiting inside the protocol".
        Say("EVENT STARTING");
        ss.Start(&instance);

        Say("READY");

        if (options.hold)
        {
            //Wait for the driver to tell us to let go.
            WaitForDriver();
        }

        //Last chance to notice that somebody interfered with the resource while
        //we were holding it.
        instance.CheckWitness("before letting go");
    }
    catch (const std::exception& exc)
    {
        std::printf("ERROR %s\n", exc.what());
        std::fflush(stdout);
        std::printf("RESULT created=%d used=%d destroyed=%d\n",
                    instance.creates, instance.uses, instance.destroys);
        std::fflush(stdout);
        return 1;
    }

    std::printf("RESULT created=%d used=%d destroyed=%d\n",
                instance.creates, instance.uses, instance.destroys);
    std::fflush(stdout);
    return instance.WitnessViolated() ? 1 : 0;
}
