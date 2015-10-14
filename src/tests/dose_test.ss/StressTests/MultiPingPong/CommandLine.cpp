/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / stlrha
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
            ("no-pinger", "Dont be a pinger (default is to be a pinger and a ponger)")
            ("no-ponger", "Dont be a ponger (default is to be a pinger and a ponger)")
            ;

        po::options_description pinger("Pinger options");
        pinger.add_options()
            ("num-instances,i", po::value<int>(&m_numInstances)->default_value(1), "The number of instances to use")
            ("payload,p", "Add payload of random size to Ping")
            ("timeout,t", po::value<int>(&m_timeout)->default_value(20),"Timeout before missing pongs should generate error report")
            ;


        //merge options into one.
        po::options_description all ("Allowed options");
        all.add(general).add(pinger);

        po::store(po::parse_command_line(argc,argv,all),m_variablesMap);
        po::notify(m_variablesMap);

        if (m_variablesMap.count("help"))
        {
            std::wcout << all << std::endl;
            return false;
        }

        m_noPinger = 0 != m_variablesMap.count("no-pinger");
        m_noPonger = 0 != m_variablesMap.count("no-ponger");
        m_payload = 0 != m_variablesMap.count("payload");
    }
    catch (const std::exception & e)
    {
        std::wcerr << "Parse of command line failed: " << std::endl
            << e.what() << std::endl;
        return false;
    }
    return true;
}

