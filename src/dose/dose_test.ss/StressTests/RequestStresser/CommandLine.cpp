/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Hannah Myerscough / sthamy
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

#include "CommandLine.h"

#include <iostream>
#include <Safir/Dob/Typesystem/Operations.h>

CommandLine & CommandLine::Instance()
{
    static CommandLine obj;
    return obj;
}

bool CommandLine::Parse(int argc, char* argv[])
{
    namespace po = boost::program_options;
    // Declare the supported options.
    std::wcout << std::boolalpha;
    try
    {
        po::options_description general("General options");
        general.add_options()
            ("help,h", "produce help message")
            ("requestor,s", "Be a requestor (default is to be a receiver)")
            ;

        po::options_description requestor("Requestor options");
        requestor.add_options()
            ("large,l", "Use a large message (default is small)")
            ;

        po::options_description receiver("Receiver options");
        receiver.add_options()
            ;

        //merge options into one.
        po::options_description all ("Allowed options");
        all.add(general).add(requestor).add(receiver);

        po::store(po::parse_command_line(argc,argv,all),m_variablesMap);

        if (m_variablesMap.count("help"))
        {
            std::wcout << all << std::endl;
            return false;
        }

        m_requestor = 0 != m_variablesMap.count("requestor");
        m_large = 0 != m_variablesMap.count("large");
    }
    catch (const std::exception & e)
    {
        std::wcerr << "Parse of command line failed: " << std::endl
            << e.what() << std::endl;
        return false;
    }
    return true;
}

