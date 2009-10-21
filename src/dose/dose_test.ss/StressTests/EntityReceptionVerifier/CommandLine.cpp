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
    std::cout << std::boolalpha;
    try
    {
        po::options_description general("General options");
        general.add_options()
            ("help,h", "produce help message")
            ("owner,o", "Be an owner")
            ("subscriber,s", "Be a subscriber (this is the default)")
            ("num-sets,n",po::value<int>(&m_numSets)->default_value(100000),"Number of sets to perform")
            ;

        po::options_description owner("Owner options");
        owner.add_options()
            ("no-sleep,N", "Don't sleep between Sets")
            ("num-instances,i",po::value<int>()->default_value(1), "Number of instances to use")
            ("sleep-time,t", po::value<int>()->default_value(0), "The time to sleep between each Set/Read \n(in milliseconds)")
            ("batch-size,b", po::value<int>()->default_value(1), "The number of entities to set between \neach sleep")
            ("attach-payload,p","Put the DefaultPayload defined in the\nRootEntity dou file into the Payload member.");
            ;

        po::options_description receiver("Subscriber options");
        receiver.add_options()
            ("change-info,c", "Subscribe with change information\n enabled (default is not enabled)")
            ("print-misses,m", "Print the missed updates")
            ("extra-dispatch,e", "Perform an extra dispatch before exiting")
            ;

        //merge options into one.
        po::options_description all ("Allowed options");
        all.add(general).add(owner).add(receiver);

        po::store(po::parse_command_line(argc,argv,all),m_variablesMap);
        po::notify(m_variablesMap);
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

        if (m_variablesMap.count("owner") +
            m_variablesMap.count("subscriber") > 1)
        {
            std::wcerr << "Only one of the options --owner and --subscriber may be set" << std::endl;
            return false;
        }
        else if (m_variablesMap.count("owner") +
                 m_variablesMap.count("subscriber") == 0)
        {
            m_subscriber = true;
        }

        m_noSleep = 0 != m_variablesMap.count("no-sleep");


    }
    catch (const std::exception & e)
    {
        std::wcerr << "Parse of command line failed: " << std::endl
            << e.what() << std::endl;
        return false;
    }
    return true;
}

