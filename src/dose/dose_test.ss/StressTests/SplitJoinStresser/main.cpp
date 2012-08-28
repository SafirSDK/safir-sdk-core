/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
*
* Created by: Anders Widén/ stawi
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

#include <iostream>
#include <string>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4127 4512 4702 4267 4251 4275)
#endif
#include <boost/program_options.hpp>
#if defined _MSC_VER
  #pragma warning (pop)
#endif

#include "app.h"

namespace po = boost::program_options;

struct CommandLineResults
{
    int activeDuration;
};

const CommandLineResults & HandleCommandLine(int argc, char* argv[])
{
    try
    {
        static CommandLineResults results;

        po::options_description desc("Allowed options");
        desc.add_options()
            ("help,h", "show help message")
            ("active-duration,d", po::value<int>(&results.activeDuration)->default_value(0), "duration of the active updates (seconds), 0 = forever");
            
        po::variables_map vm;
        po::store(po::parse_command_line(argc, argv, desc), vm);
        po::notify(vm);

        if (vm.count("help"))
        {
            std::ostringstream ostr;
            ostr << desc;
            std::wcout << ostr.str().c_str() << std::endl;
            exit(0);
        }
  
        return results;
    }
    catch (const std::exception & e)
    {
        std::wcout << "Got exception while parsing command line: "<< std::endl
            <<e.what() <<std::endl;
        exit(1);
    }

}

int main(int argc, char* argv[])
{
    std::wcout << "Starting" << std::endl;

    const CommandLineResults& commandLine = HandleCommandLine(argc,argv);
    
    try
    {
        App app;
        app.Run(commandLine.activeDuration);
    }
    catch(std::exception & e)
    {
        std::wcout << "Caught std::exception! Contents of exception is:" << std::endl
                   << e.what()<<std::endl;
    }
    catch (...)
    {
        std::wcout << "Caught ... exception!" << std::endl;
    }

    std::wcout << "Exiting" << std::endl;

    return 0;
}
