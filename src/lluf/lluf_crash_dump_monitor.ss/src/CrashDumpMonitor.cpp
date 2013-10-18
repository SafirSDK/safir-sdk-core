/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
*
* Created by: Anders Widén <anders.widen@consoden.se>
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
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/StringEncoding.h>
#include <boost/bind.hpp>
#include <boost/thread/thread.hpp>
#include <iostream>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable:4100)
#pragma warning (disable:4702)
#endif

#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

class ProgramOptions
{
public:
    ProgramOptions(int argc, char* argv[])
        : parseOk(false),
          maxDumpFiles(0),
          checkInterval(0),
          runOnce(false)
    {
        using namespace boost::program_options;
        Safir::Utilities::Internal::ConfigReader reader;
        
        options_description desc("Allowed options");
        desc.add_options()
            ("help,h", "show help message")
            ("max-dump-files,i", value<size_t>(&maxDumpFiles)->default_value(1000), "Max number of crash dumpfiles.")
            ("check-interval,c", value<int>(&checkInterval)->default_value(30), "Check interval in seconds.")
            ("run-once,o", value<bool>(&runOnce)->zero_tokens(), "Run the program once and then return.")
            ;
        
        variables_map vm;

        try
        {
            store(command_line_parser(argc, argv).
                  options(desc).run(), vm);
            notify(vm);
        }
        catch (const std::exception& exc)
        {
            std::cout << "Error parsing command line: " << exc.what() << "\n" << std::endl;
            ShowHelp(desc);
            return;
        }

        if (vm.count("help"))
        {
            ShowHelp(desc);
            return;
        }

        parseOk = true;
    }
    bool    parseOk;
    size_t  maxDumpFiles;
    int     checkInterval;
    bool    runOnce;

private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::cout << std::boolalpha
                   << "Monitors the crash dump directory and removes dump files if the maximum number of files is reached.\n"
                   << desc << "\n"
                   << std::endl;
    }

};

const boost::filesystem::path GetDumpDirectory()
{
    Safir::Utilities::Internal::ConfigReader config;
    return boost::filesystem::path(config.Locations().get<std::string>("crash_dump_directory"));
}

int main(int argc, char * argv[])
{
    const ProgramOptions options(argc,argv);

    if (!options.parseOk)
    {
        return 1;
    }

    for (;;)
    {
        try
        {
            namespace bfs = boost::filesystem;

            using Safir::Utilities::Internal::ToUtf16;

            const std::vector<bfs::path> dumpFiles =
                    std::vector<bfs::path>(bfs::directory_iterator(GetDumpDirectory()),
                                           bfs::directory_iterator());

            if (dumpFiles.size() > options.maxDumpFiles)
            {
                std::multimap<std::time_t, bfs::path> sorted;
                for (std::vector<bfs::path>::const_iterator it = dumpFiles.begin();
                     it != dumpFiles.end(); ++it)
                {
                    sorted.insert(std::make_pair(bfs::last_write_time(*it),*it));
                }

                const size_t tooMany = (dumpFiles.size() - options.maxDumpFiles) + 10; //remove a few more...

                std::multimap<std::time_t, bfs::path>::iterator it = sorted.begin();
                for (size_t i = 0; i < tooMany; ++i)
                {
                    bfs::remove(it->second);
                    SEND_SYSTEM_LOG(Notice,
                                    << L"Removed file " << ToUtf16((it->second).string()));
                    ++it;
                }
            }
        }
        catch (const boost::filesystem::filesystem_error& )
        {

        }

        if (options.runOnce)
        {
            return 0;
        }
        boost::this_thread::sleep(boost::posix_time::seconds(options.checkInterval));
    }

#ifndef _MSC_VER
    return 0;
#endif

}
