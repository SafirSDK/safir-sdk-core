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
            ("reader,r", "Be a reader")
            ;

        po::options_description owner("Owner and Reader options");
        owner.add_options()
            ("no-ack,n", "Use an entity without ack (default is with ack)")
            ("large,l", "Use a large message (default is small)")
            ("no-sleep,N", "Don't sleep between Sets/Reads")
            ("num-instances,i", po::value<int>()->default_value(1),"Number of instances to use")
            ("sleep-time,t", po::value<int>()->default_value(0), "The time to sleep between each Set/Read \n(in milliseconds)")
            ("batch-size,b", po::value<int>()->default_value(1), "The number of entities to set between \neach sleep")
            ("attach-payload,p","Put the DefaultPayload defined in the\nRootEntity dou file into the Payload member.");
            ;

        po::options_description receiver("Subscriber options");
        receiver.add_options()
            ("change-info,c", "Subscribe with change information\n enabled (default is not enabled)")
            ;

        //merge options into one.
        po::options_description all ("Allowed options");
        all.add(general).add(owner).add(receiver);

        po::store(po::parse_command_line(argc,argv,all),m_variablesMap);
        m_sleepTime = m_variablesMap["sleep-time"].as<int>();
        m_batchSize = m_variablesMap["batch-size"].as<int>();
        m_numInstances = m_variablesMap["num-instances"].as<int>();
        if (m_variablesMap.count("help"))
        {
            std::cout << all << std::endl;
            return false;
        }

        m_owner = 0 != m_variablesMap.count("owner");
        m_subscriber = 0 != m_variablesMap.count("subscriber");
        m_reader = 0 != m_variablesMap.count("reader");

        if (m_variablesMap.count("owner") +
            m_variablesMap.count("subscriber") +
            m_variablesMap.count("reader") > 1)
        {
            std::wcerr << "Only one of the options --owner, --subscriber and --reader may be set" << std::endl;
            return false;
        }
        else if (m_variablesMap.count("owner") +
                 m_variablesMap.count("subscriber") +
                 m_variablesMap.count("reader") == 0)
        {
            m_subscriber = true;
        }

        m_ack = 0 == m_variablesMap.count("no-ack");
        m_large = 0 != m_variablesMap.count("large");
        m_noSleep = 0 != m_variablesMap.count("no-sleep");

        m_changeInfo = 0 != m_variablesMap.count("change-info");
    }
    catch (const std::exception & e)
    {
        std::wcerr << "Parse of command line failed: " << std::endl
            << e.what() << std::endl;
        return false;
    }
    return true;
}

