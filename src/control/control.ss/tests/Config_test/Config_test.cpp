/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
*
* Created by: Anders Widén
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
#include "../../src/Config.h"

#include <iostream>
#include <math.h>

#define CHECK(expr) {if (!(expr)) { std::cout << "Test failed! Line: " <<__LINE__<< ", expr: " << #expr << std::endl; exit(1);}}

namespace ctrl = Safir::Dob::Internal::Control;

int main(int argc, char* argv[])
{
    if (argc < 2 || argc > 3)
    {
        std::cout << "Invalid number of arguments" << std::endl;
        return 1;
    }

    std::string test(argv[1]);

    if (test == "tc1")
    {
        ctrl::Config config;

        auto nodeTypes = config.GetNodeTypes();

        CHECK(nodeTypes.size() == 3);

        // Check first index which is Server
        CHECK(nodeTypes[0].name == "Server");
        CHECK(nodeTypes[0].isLight == false);
        CHECK(nodeTypes[0].talksTo.size() == 3);
        CHECK(nodeTypes[0].talksTo.find("Server") != nodeTypes[0].talksTo.end());
        CHECK(nodeTypes[0].talksTo.find("Client") != nodeTypes[0].talksTo.end());
        CHECK(nodeTypes[0].talksTo.find("RemoteClient") != nodeTypes[0].talksTo.end());
        //TODO CHECK(nodeTypes[0].multicastAddress == "192.0.0.1");
        //TODO CHECK(nodeTypes[0].controlPort == 9500);
        //TODO CHECK(nodeTypes[0].dataPort == 9501);
        CHECK(nodeTypes[0].heartbeatInterval == 1000);
        CHECK(nodeTypes[0].maxLostHeartbeats == 5);
        CHECK(nodeTypes[0].slidingWindowSize == 10);
        CHECK(nodeTypes[0].retryTimeout == 200);
        CHECK(nodeTypes[0].wantedTypes.size() == 1);
        CHECK(nodeTypes[0].wantedTypes[0] == ".*");
        CHECK(nodeTypes[0].unwantedTypes.size() == 0);

        // Check second index which is Client
        CHECK(nodeTypes[1].name == "Client");
        CHECK(nodeTypes[1].isLight == true);
        CHECK(nodeTypes[1].talksTo.size() == 1);
        CHECK(nodeTypes[1].talksTo.find("Server") != nodeTypes[1].talksTo.end());
        //TODO CHECK(nodeTypes[1].multicastAddress == "192.0.0.15");
        //TODO CHECK(nodeTypes[1].controlPort == 9500);
        //TODO CHECK(nodeTypes[1].dataPort == 9501);
        CHECK(nodeTypes[1].heartbeatInterval == 5000);
        CHECK(nodeTypes[1].maxLostHeartbeats == 5);
        CHECK(nodeTypes[1].slidingWindowSize == 10);
        CHECK(nodeTypes[1].retryTimeout == 2000);
        CHECK(nodeTypes[1].wantedTypes.size() == 1);
        CHECK(nodeTypes[1].wantedTypes[0] == ".*");
        CHECK(nodeTypes[1].unwantedTypes.size() == 0);

        // Check third index which is RemoteClient
        CHECK(nodeTypes[2].name == "RemoteClient");
        CHECK(nodeTypes[2].isLight == true);
        CHECK(nodeTypes[2].talksTo.size() == 1);
        CHECK(nodeTypes[2].talksTo.find("Server") != nodeTypes[2].talksTo.end());
        //TODO CHECK(nodeTypes[2].multicastAddress == "192.0.0.16");
        //TODO CHECK(nodeTypes[2].controlPort == 9500);
        //TODO CHECK(nodeTypes[2].dataPort == 9501);
        CHECK(nodeTypes[2].heartbeatInterval == 30000);
        CHECK(nodeTypes[2].maxLostHeartbeats == 5);
        CHECK(nodeTypes[2].slidingWindowSize == 10);
        CHECK(nodeTypes[2].retryTimeout == 5000);
        CHECK(nodeTypes[2].wantedTypes.size() == 1);
        CHECK(nodeTypes[2].wantedTypes[0] == R"(Capabilities\.Vehicles\.)");
        CHECK(nodeTypes[2].unwantedTypes.size() == 1);
        CHECK(nodeTypes[2].unwantedTypes[0] == R"(Capabilities\.Vehicles\.Ambulance.*)");

    }
    else if (test == "tc2")
    {
        try
        {
            ctrl::Config config;
            config.GetNodeTypes();

            CHECK(!"Expected an exception!");
        }
        catch (const std::exception& e)
        {
           CHECK(std::string(e.what()).find("IsLight is mandatory") != std::string::npos)
        }
    }
    else if (test == "tc3")
    {
        try
        {
            ctrl::Config config;
            config.GetNodeTypes();

            CHECK(!"Expected an exception!");
        }
        catch (const std::exception& e)
        {
           CHECK(std::string(e.what()).find("WantedTypes can only be .* for non Light nodes!") != std::string::npos)
        }
    }
    else
    {
        std::cout << "Valid test case args are: Normal" << std::endl;
        return 1;
    }

    return 0;
}


