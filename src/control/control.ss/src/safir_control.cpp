/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
*
* Created by: Anders Widén/ anders.widen@consoden.se
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

#include "ControlApp.h"
#include <Safir/Utilities/CrashReporter.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Utilities/Internal/Expansion.h>
#include <Safir/Utilities/StartupSynchronizer.h>

#include <iostream>
#include <atomic>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267 4251 4913 4005)
#endif

#include <boost/regex.hpp>
#include <boost/program_options.hpp>
#include <boost/asio.hpp>
#include <boost/thread.hpp>
#include <boost/process/search_path.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

namespace //anonymous namespace
{
    void DumpFunc(const char* const dumpPath)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "safir_control has generated a dump to:\n"
                        << dumpPath << "\n"
                        << "Please send this file to your nearest Dob developer, along with\n"
                        << "relevant information about what version of Safir SDK Core you are using");
    }

    boost::filesystem::path FindDoseMain(const std::string& doseMainPathOption)
    {
        using namespace boost::filesystem;

        //Boost.Process sucks, so we need this define.
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        std::wstring doseMainName(L"dose_main");
#elif defined(linux) || defined(__linux) || defined(__linux__)
        std::string doseMainName("dose_main");
#endif

        path doseMainPath;

        if (doseMainPathOption.empty())
        {
            doseMainPath = boost::process::search_path(doseMainName);

            if (doseMainPath.empty())
            {
                std::ostringstream os;
                os << "CTRL: Can't find dose_main in PATH";
                std::wcout << os.str().c_str() << std::endl;
                SEND_SYSTEM_LOG(Critical, << os.str().c_str() << std::endl);
                return "";
            }
        }
        else
        {
            doseMainPath = doseMainPathOption;
        }

        if (exists(doseMainPath))
        {
            if (is_directory(doseMainPath) || !is_regular_file(doseMainPath))
            {
                std::ostringstream os;
                os << "CTRL: " << doseMainPath << " is a directory or a non regular file!";
                std::wcout << os.str().c_str() << std::endl;
                SEND_SYSTEM_LOG(Critical, << os.str().c_str() << std::endl);
                return "";
            }
        }
        else
        {
            std::ostringstream os;
            os << "CTRL: Can't find " << doseMainPath;
            std::wcout << os.str().c_str() << std::endl;
            SEND_SYSTEM_LOG(Critical, << os.str().c_str() << std::endl);
            return "";
        }

        return doseMainPath;
    }
}

std::wostream& operator<<(std::wostream& out, const std::string& str)
{
    return out << str.c_str();
}

std::wostream& operator<<(std::wostream& out, const boost::program_options::options_description& opt)
{
    std::ostringstream ostr;
    ostr << opt;
    return out << ostr.str().c_str();
}

class ProgramOptions
{
public:
    ProgramOptions(int argc, char* argv[])
        : parseOk(false)
    {
        using namespace boost::program_options;
        options_description options("Options");
        options.add_options()
                ("help,h", "show help message")
                ("dose-main-path",
                 value<std::string>(&doseMainPath)->default_value(""),
                 "Absolute or relative path to dose_main executable. If not defined PATH will be used.")
                ("force-id",
                 value<std::int64_t>(&id)->default_value(0, ""),
                 "Override the automatically generated node id. WARNING This parameter is for debugging/testing"
                 "purposes only.")
                ("ignore-control-cmd",
                 value<bool>(&ignoreControlCmd)->default_value(false),
                 "Ignore all commands sent to Control via Ipc. WARNING For debugging/testing purposes only.");

        variables_map vm;

        try
        {
            store(command_line_parser(argc, argv).
                  options(options).run(), vm);
            notify(vm);
        }
        catch (const std::exception& exc)
        {
            std::wcerr << "Error parsing command line: " << exc.what() << "\n" << std::endl;
            ShowHelp(options);
            return;
        }

        if (vm.count("help"))
        {
            ShowHelp(options);
            return;
        }

        parseOk = true;
    }
    bool parseOk;
    std::string doseMainPath;
    std::int64_t id;
    bool ignoreControlCmd;

private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                   << "Usage: control [OPTIONS]\n"
                   << desc << std::endl;
    }

};

// Helper class that makes sure only one safir_control can be started with the same SAFIR_INSTANCE
class StartCondition : public Safir::Utilities::Synchronized
{
public:
    StartCondition() : m_synchronizer(("SAFIR_CONTROL" + Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix()).c_str())
    {
    }

    bool IsAllowedToStart()
    {
        m_synchronizer.Start(this);
        return m_startAllowed;
    }

private:
    Safir::Utilities::StartupSynchronizer m_synchronizer;
    bool m_startAllowed = false;
    void Create() override { m_startAllowed = true; }
    void Use() override {}
    void Destroy() override {}
};

int main(int argc, char * argv[])
{
    //ensure call to CrashReporter::Stop at application exit
    //Start is called in DoseApp
    std::shared_ptr<void> crGuard(static_cast<void*>(0),
                                    [](void*){Safir::Utilities::CrashReporter::Stop();});

    StartCondition startCondition;
    std::atomic<bool> success(true);

    try
    {
        ENSURE(startCondition.IsAllowedToStart(),
               << "CTRL: Failed to start. Another instance of safir_control is already started with SAFIR_INSTANCE="
               << Safir::Utilities::Internal::Expansion::GetSafirInstance());

        lllog(1) << "CTRL: Started" << std::endl;

        const ProgramOptions options(argc, argv);

        if (!options.parseOk)
        {
            return 1;
        }

        // Locate dose_main binary
        const boost::filesystem::path doseMainPath = FindDoseMain(options.doseMainPath);

        boost::asio::io_context io;

        Safir::Utilities::CrashReporter::RegisterCallback(DumpFunc);
        Safir::Utilities::CrashReporter::Start();


        auto controlApp = Safir::make_unique<ControlApp>(io, doseMainPath, options.id, options.ignoreControlCmd);

        const auto run = [&io,&controlApp, &success]
        {
            try
            {
                io.run();
                return;
            }
            catch (const std::exception & exc)
            {
                SEND_SYSTEM_LOG(Alert,
                            << "CTRL: Caught 'std::exception' exception from io_service.run(): "
                                << "  '" << exc.what() << "'.");
                success.exchange(false);
            }
            catch (...)
            {
                SEND_SYSTEM_LOG(Alert,
                                << "CTRL: Caught '...' exception from io_service.run().");
                success.exchange(false);
            }

            controlApp->Stop();
        };

        const auto nbrOfThreads = 4;

        boost::thread_group threads;
        for (auto i = 0; i < nbrOfThreads-1; ++i)
        {
            threads.create_thread(run);
        }

        run();

        threads.join_all();

        if (!success)
        {
            controlApp->Stop();
            io.restart();
            io.run();
        }

        if (!controlApp->DoseMainExitedCleanly())
        {
            success = false;
        }
        controlApp.reset();
        crGuard.reset();
    }
    catch (const std::exception & exc)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "CTRL: Caught 'std::exception' exception: "
                        << "  '" << exc.what() << "'.");
        success = false;
    }
    catch (...)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "CTRL: Caught '...' exception.");
        success = false;
    }
    if (success)
    {
        lllog(1) << "CTRL: Exiting..." << std::endl;
        std::wcout << "CTRL: Exiting..." << std::endl;
    }
    else
    {
        SEND_SYSTEM_LOG(Alert,
                        << "CTRL: Exiting due to error!");
        std::wcout << "CTRL: Exiting due to error!" << std::endl;
    }
    return success ? 0 : 1;
}
