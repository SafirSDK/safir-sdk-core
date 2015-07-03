/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/Id.h>
#include <iostream>
#include <boost/regex.hpp>
#include <boost/atomic.hpp>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267 4251 4913)
#endif

#include <boost/program_options.hpp>
#include <boost/asio.hpp>
#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

namespace //anonymous namespace
{
    void DumpFunc(const char* const dumpPath)
    {
        std::wostringstream ostr;
        SEND_SYSTEM_LOG(Alert,
                        << "safir_control has generated a dump to:\n"
                        << dumpPath << "\n"
                        << "Please send this file to your nearest Dob developer, along with\n"
                        << "relevant information about what version of Safir SDK you are using");
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
                 value<boost::int64_t>(&id)->default_value(LlufId_GenerateRandom64(), ""),
                 "Override the automatically generated node id. For debugging/testing purposes only.");

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
    boost::int64_t id;

private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                   << "Usage: control [OPTIONS]\n"
                   << desc << std::endl;
    }

};


int main(int argc, char * argv[])
{
    //ensure call to CrashReporter::Stop at application exit
    //Start is called in DoseApp
    boost::shared_ptr<void> crGuard(static_cast<void*>(0),
                                    [](void*){Safir::Utilities::CrashReporter::Stop();});

    boost::atomic<bool> success(true);

    try
    {
        lllog(1) << "CTRL: Started" << std::endl;

        const ProgramOptions options(argc, argv);

        if (!options.parseOk)
        {
            return 1;
        }

        // Locate dose_main binary

        namespace fs = boost::filesystem;

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        std::wstring doseMainName(L"dose_main");
#elif defined(linux) || defined(__linux) || defined(__linux__)
        std::string doseMainName("dose_main");
#endif

        fs::path doseMainPath;

        if (options.doseMainPath.empty())
        {
            doseMainPath = boost::process::search_path(doseMainName);

            if (doseMainPath.empty())
            {
                std::ostringstream os;
                os << "CTRL: Can't find dose_main in PATH";
                std::wcout << os.str().c_str() << std::endl;
                SEND_SYSTEM_LOG(Critical, << os.str().c_str() << std::endl);
                return 1;
            }
        }
        else
        {
            doseMainPath =  options.doseMainPath;
        }

        if (fs::exists(doseMainPath))
        {
            if (fs::is_directory(doseMainPath) || !fs::is_regular_file(doseMainPath))
            {
                std::ostringstream os;
                os << "CTRL: " << doseMainPath << " is a directory or a non regular file!";
                std::wcout << os.str().c_str() << std::endl;
                SEND_SYSTEM_LOG(Critical, << os.str().c_str() << std::endl);
                return 1;
            }
        }
        else
        {
            std::ostringstream os;
            os << "CTRL: Can't find " << doseMainPath;
            std::wcout << os.str().c_str() << std::endl;
            SEND_SYSTEM_LOG(Critical, << os.str().c_str() << std::endl);
            return 1;
        }

        boost::asio::io_service ioService;

        Safir::Utilities::CrashReporter::RegisterCallback(DumpFunc);
        Safir::Utilities::CrashReporter::Start();

        //Set number of threads to at least 2, or the number of cpu kernels
        auto nbrOfThreads = std::max<size_t>(2, boost::thread::hardware_concurrency());

        const auto run = [&ioService, &success]
        {
            try
            {
                ioService.run();
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
        };

        ControlApp controlApp(ioService, doseMainPath, options.id);

        (void)controlApp;  // to keep compilers from warning about unused variable

        boost::thread_group threads;
        for (unsigned int i = 0; i < nbrOfThreads-1; ++i)
        {
            threads.create_thread(run);
        }

        run();

        threads.join_all();

        crGuard.reset();
    }
    catch (const std::exception & exc)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "CTRL: Caught 'std::exception' exception: "
                        << "  '" << exc.what() << "'.");
        success.exchange(false);
    }
    catch (...)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "CTRL: Caught '...' exception.");
        success.exchange(false);
    }
    if (success)
    {
        std::wcout << "CTRL: Exiting..." << std::endl;
    }
    else
    {
        std::wcout << "CTRL: Exiting due to error..." << std::endl;
    }
    return success ? 0 : 1;
}
