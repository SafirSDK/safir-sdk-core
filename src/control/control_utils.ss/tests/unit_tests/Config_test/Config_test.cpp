/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
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
#define LOGERROR(expr) { std::wcout << "Test failed! Line: " <<__LINE__<< ", expr: " << #expr << std::endl; ++failures;}

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

        CHECK(conf.nodeTypesParam.size() == 4);

        // Check first index which is Server
        CHECK(conf.nodeTypesParam[0].name == "Server");
        CHECK(conf.nodeTypesParam[0].multicastAddressControl == "192.0.0.1:9500");
        CHECK(conf.nodeTypesParam[0].multicastAddressData == "192.0.0.1:9501");
        CHECK(conf.nodeTypesParam[0].heartbeatInterval == 1000);
        CHECK(conf.nodeTypesParam[0].maxLostHeartbeats == 5);
        CHECK(conf.nodeTypesParam[0].slidingWindowSize == 20);
        CHECK(conf.nodeTypesParam[0].retryTimeout.front() == 200);
        CHECK(conf.nodeTypesParam[0].requiredForStart == true);
        CHECK(conf.nodeTypesParam[0].isLightNode == false);
        CHECK(conf.nodeTypesParam[0].keepStateWhileDetached == false);

        CHECK(conf.nodeTypesParam[1].name == "Server2");
        CHECK(conf.nodeTypesParam[1].multicastAddressControl == "");
        CHECK(conf.nodeTypesParam[1].multicastAddressData == "");
        CHECK(conf.nodeTypesParam[1].heartbeatInterval == 1000);
        CHECK(conf.nodeTypesParam[1].maxLostHeartbeats == 5);
        CHECK(conf.nodeTypesParam[1].slidingWindowSize == 20);
        CHECK(conf.nodeTypesParam[1].retryTimeout.front() == 200);
        CHECK(conf.nodeTypesParam[1].requiredForStart == false);
        CHECK(conf.nodeTypesParam[1].isLightNode == false);
        CHECK(conf.nodeTypesParam[1].keepStateWhileDetached == false);

        // Check second index which is Client
        CHECK(conf.nodeTypesParam[2].name == "Client");
        CHECK(conf.nodeTypesParam[2].multicastAddressControl == "");
        CHECK(conf.nodeTypesParam[2].multicastAddressData == "");
        CHECK(conf.nodeTypesParam[2].heartbeatInterval == 5000);
        CHECK(conf.nodeTypesParam[2].maxLostHeartbeats == 5);
        CHECK(conf.nodeTypesParam[2].slidingWindowSize == 20);
        CHECK(conf.nodeTypesParam[2].retryTimeout.front() == 2000);
        CHECK(conf.nodeTypesParam[2].requiredForStart == false);
        CHECK(conf.nodeTypesParam[2].isLightNode == true);
        CHECK(conf.nodeTypesParam[2].keepStateWhileDetached == true);

        // Check third index which is RemoteClient
        CHECK(conf.nodeTypesParam[3].name == "RemoteClient");
        CHECK(conf.nodeTypesParam[3].multicastAddressControl == "");
        CHECK(conf.nodeTypesParam[3].multicastAddressData == "");
        CHECK(conf.nodeTypesParam[3].heartbeatInterval == 30000);
        CHECK(conf.nodeTypesParam[3].maxLostHeartbeats == 5);
        CHECK(conf.nodeTypesParam[3].slidingWindowSize == 20);
        CHECK(conf.nodeTypesParam[3].retryTimeout.front() == 5000);
        CHECK(conf.nodeTypesParam[3].requiredForStart == false);
        CHECK(conf.nodeTypesParam[3].isLightNode == true);
        CHECK(conf.nodeTypesParam[3].keepStateWhileDetached == false);

        // Check ThisNode parameters
        CHECK(conf.thisNodeParam.controlAddress == "0.0.0.0:30000");
        CHECK(conf.thisNodeParam.dataAddress == "0.0.0.0:40000");
        CHECK(conf.thisNodeParam.seeds[0] == "192.168.1.11:40000");
        CHECK(conf.thisNodeParam.seeds[1] == "192.168.1.15:30000");
        CHECK(conf.thisNodeParam.name == "MyNode");
        CHECK(conf.thisNodeParam.nodeType == "Server");
        CHECK(conf.thisNodeParam.nodeTypeId == LlufId_Generate64("Server"));

        CHECK(conf.aloneTimeout == boost::chrono::seconds(123));
        CHECK(conf.localInterfaceTimeout == boost::chrono::seconds(33));

        CHECK(&conf.GetThisNodeType() == &conf.nodeTypesParam[0]);
    }
    else if (test == "tc2")
    {
        try
        {
            ctrl::Config conf;
            LOGERROR("Expected an exception!");
        }
        catch (const std::exception& e)
        {
           CHECK(std::string(e.what()).find("RequiredForStart is mandatory") != std::string::npos)
        }
    }
    else if (test == "tc3")
    {
        try
        {
            ctrl::Config conf;
            LOGERROR("Expected an exception!");
        }
        catch (const std::exception& e)
        {
           CHECK(std::string(e.what()).find("RequiredForStart and IsLightNode cannot both be true") != std::string::npos)
        }
    }
    else if (test == "tc4")
    {
        try
        {
            ctrl::Config conf;

            LOGERROR("Expected an exception!");
        }
        catch (const std::exception& e)
        {
            std::cout << e.what() << std::endl;

           CHECK(std::string(e.what()).find("Duplicated ip addresses") != std::string::npos)
        }
    }
    else if (test == "tc5")
    {
        try
        {
            ctrl::Config conf;
            LOGERROR("Expected an exception!");
        }
        catch (const std::exception& e)
        {
           CHECK(std::string(e.what()).find("KeepStateWhileDetached is mandatory") != std::string::npos)
        }
    }
    else if (test == "tc6")
    {
        try
        {
            ctrl::Config conf;
            LOGERROR("Expected an exception!");
        }
        catch (const std::exception& e)
        {
           CHECK(std::string(e.what()).find("HeartbeatInterval") != std::string::npos)
        }
    }
    else if (test == "tc7")
    {
        try
        {
            ctrl::Config conf;
            LOGERROR("Expected an exception!");
        }
        catch (const std::exception& e)
        {
           CHECK(std::string(e.what()).find("SlidingWindowsSize") != std::string::npos)
        }
    }
    else if (test == "tc8")
    {
        try
        {
            ctrl::Config conf;
            LOGERROR("Expected an exception!");
        }
        catch (const std::exception& e)
        {
           CHECK(std::string(e.what()).find("AckRequestThreshold") != std::string::npos)
        }
    }
    else if (test == "tc9")
    {
        try
        {
            ctrl::Config conf;
            LOGERROR("Expected an exception!");
        }
        catch (const std::exception& e)
        {
           CHECK(std::string(e.what()).find("RetryTimeout is mandatory") != std::string::npos)
        }
    }
    else if (test == "tc10")
    {
        try
        {
            ctrl::Config conf;
            LOGERROR("Expected an exception!");
        }
        catch (const std::exception& e)
        {
           CHECK(std::string(e.what()).find("RetryTimeout values must all be greater than 0") != std::string::npos)
        }
    }

    else
    {
        std::cout << "Valid test case args are: Normal" << std::endl;
        return 1;
    }

    return failures;
}
