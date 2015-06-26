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
#include <Safir/Dob/Internal/ControlConfig.h>

#include <iostream>
#include <math.h>

int failures = 0;
#define CHECK(expr) {if (!(expr)) { std::wcout << "Test failed! Line: " <<__LINE__<< ", expr: " << #expr << std::endl; ++failures;}}

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
        ctrl::Config conf;

        CHECK(conf.nodeTypesParam.size() == 3);

        // Check first index which is Server
        CHECK(conf.nodeTypesParam[0].name == "Server");
        CHECK(conf.nodeTypesParam[0].isLight == false);
        CHECK(conf.nodeTypesParam[0].talksTo.size() == 3);
        CHECK(conf.nodeTypesParam[0].talksTo.find("Server") != conf.nodeTypesParam[0].talksTo.end());
        CHECK(conf.nodeTypesParam[0].talksTo.find("Client") != conf.nodeTypesParam[0].talksTo.end());
        CHECK(conf.nodeTypesParam[0].talksTo.find("RemoteClient") != conf.nodeTypesParam[0].talksTo.end());
        CHECK(conf.nodeTypesParam[0].multicastAddressControl == "192.0.0.1:9500");
        CHECK(conf.nodeTypesParam[0].multicastAddressData == "192.0.0.1:9501");
        CHECK(conf.nodeTypesParam[0].heartbeatInterval == 1000);
        CHECK(conf.nodeTypesParam[0].maxLostHeartbeats == 5);
        CHECK(conf.nodeTypesParam[0].slidingWindowSize == 10);
        CHECK(conf.nodeTypesParam[0].retryTimeout == 200);
        CHECK(conf.nodeTypesParam[0].wantedTypes.size() == 1);
        CHECK(conf.nodeTypesParam[0].wantedTypes[0] == ".*");
        CHECK(conf.nodeTypesParam[0].unwantedTypes.size() == 0);

        // Check second index which is Client
        CHECK(conf.nodeTypesParam[1].name == "Client");
        CHECK(conf.nodeTypesParam[1].isLight == true);
        CHECK(conf.nodeTypesParam[1].talksTo.size() == 1);
        CHECK(conf.nodeTypesParam[1].talksTo.find("Server") != conf.nodeTypesParam[1].talksTo.end());
        CHECK(conf.nodeTypesParam[1].multicastAddressControl == "");
        CHECK(conf.nodeTypesParam[1].multicastAddressData == "192.0.0.15:9501");
        CHECK(conf.nodeTypesParam[1].heartbeatInterval == 5000);
        CHECK(conf.nodeTypesParam[1].maxLostHeartbeats == 5);
        CHECK(conf.nodeTypesParam[1].slidingWindowSize == 10);
        CHECK(conf.nodeTypesParam[1].retryTimeout == 2000);
        CHECK(conf.nodeTypesParam[1].wantedTypes.size() == 1);
        CHECK(conf.nodeTypesParam[1].wantedTypes[0] == ".*");
        CHECK(conf.nodeTypesParam[1].unwantedTypes.size() == 0);

        // Check third index which is RemoteClient
        CHECK(conf.nodeTypesParam[2].name == "RemoteClient");
        CHECK(conf.nodeTypesParam[2].isLight == true);
        CHECK(conf.nodeTypesParam[2].talksTo.size() == 1);
        CHECK(conf.nodeTypesParam[2].talksTo.find("Server") != conf.nodeTypesParam[2].talksTo.end());
        CHECK(conf.nodeTypesParam[2].multicastAddressControl == "192.0.0.16:9500");
        CHECK(conf.nodeTypesParam[2].multicastAddressData == "");
        CHECK(conf.nodeTypesParam[2].heartbeatInterval == 30000);
        CHECK(conf.nodeTypesParam[2].maxLostHeartbeats == 5);
        CHECK(conf.nodeTypesParam[2].slidingWindowSize == 10);
        CHECK(conf.nodeTypesParam[2].retryTimeout == 5000);
        CHECK(conf.nodeTypesParam[2].wantedTypes.size() == 1);
        CHECK(conf.nodeTypesParam[2].wantedTypes[0] == "Capabilities\\.Vehicles\\.");
        CHECK(conf.nodeTypesParam[2].unwantedTypes.size() == 1);
        CHECK(conf.nodeTypesParam[2].unwantedTypes[0] == "Capabilities\\.Vehicles\\.Ambulance.*");

        // Check ThisNode parameters
        CHECK(conf.thisNodeParam.controlAddress == "0.0.0.0:30000");
        CHECK(conf.thisNodeParam.dataAddress == "0.0.0.0:40000");
        CHECK(conf.thisNodeParam.seeds[0] == "192.168.1.11:40000");
        CHECK(conf.thisNodeParam.seeds[1] == "192.168.1.15:30000");
        CHECK(conf.thisNodeParam.name == "MyNode");
        CHECK(conf.thisNodeParam.nodeType == "Server");
        CHECK(conf.thisNodeParam.nodeTypeId == LlufId_Generate64("Server"));
    }
    else if (test == "tc2")
    {
        try
        {           
            ctrl::Config conf;

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
            ctrl::Config conf;

            CHECK(!"Expected an exception!");
        }
        catch (const std::exception& e)
        {
           CHECK(std::string(e.what()).find("WantedTypes can only be .* for non Light nodes!") != std::string::npos)
        }
    }
    else if (test == "tc4")
    {
        try
        {
            ctrl::Config conf;

            CHECK(!"Expected an exception!");
        }
        catch (const std::exception& e)
        {
            std::cout << e.what() << std::endl;

           CHECK(std::string(e.what()).find("Duplicated ip addresses") != std::string::npos)
        }
    }
    else
    {
        std::cout << "Valid test case args are: Normal" << std::endl;
        return 1;
    }

    return failures;
}


