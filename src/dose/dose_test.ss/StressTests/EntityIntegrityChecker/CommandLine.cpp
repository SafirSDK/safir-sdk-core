/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

#include <boost/program_options/cmdline.hpp>
#include <boost/program_options/parsers.hpp>
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
    std::cout << std::boolalpha;
    try
    {
        po::options_description general("General options");
        general.add_options()
            ("help,h", "produce help message")
            ("owner,o", "Be an owner")
            ("subscriber,s", "Be a subscriber (this is the default)")
            ;

        po::options_description owner("Owner and Reader options");
        owner.add_options()
            ("no-ack,n", "Use an entity without ack (default is with ack)")
            ("sleep-time,t", po::value<int>(&m_sleepTime)->default_value(0), "The time to sleep between each Set/Read \n(in milliseconds)")
            ("num-instances,N", po::value<int>(&m_numInstances)->default_value(1000), "The number of instances to use")
            ;


        //merge options into one.
        po::options_description all ("Allowed options");
        all.add(general).add(owner);

        po::store(po::parse_command_line(argc,argv,all),m_variablesMap);
        po::notify(m_variablesMap);
        if (m_variablesMap.count("help"))
        {
            std::cout << all << std::endl;
            return false;
        }

        m_owner = 0 != m_variablesMap.count("owner");
        m_subscriber = 0 != m_variablesMap.count("subscriber");

        if (m_variablesMap.count("owner") +
            m_variablesMap.count("subscriber")> 1)
        {
            std::wcerr << "Only one of the options --owner, --subscriber and --reader may be set" << std::endl;
            return false;
        }
        else if (m_variablesMap.count("owner") +
                 m_variablesMap.count("subscriber") == 0)
        {
            m_subscriber = true;
        }

        if (m_owner)
        {
            std::wcout << "Sleeping " << m_sleepTime << " ms between each set" << std::endl;
        }
    }
    catch (const std::exception & e)
    {
        std::wcerr << "Parse of command line failed: " << std::endl
            << e.what() << std::endl;
        return false;
    }
    return true;
}

