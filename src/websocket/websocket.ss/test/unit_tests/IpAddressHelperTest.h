/******************************************************************************
*
* Copyright Saab AB, 2016 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include "../../src/IpAddressHelper.h"

#define CHECK(expr) {if (!(expr)) { std::cout<<"Test failed! Line: "<<__LINE__<<", expr: "<< #expr <<std::endl; exit(1);}}

inline void IpAddressHelperTest()
{
    std::string ip;
    unsigned short port;

    std::string address="127.0.0.1:10000";
    bool ok=IpAddressHelper::SplitAddress(address, ip, port);
    CHECK(ok);
    CHECK(ip=="127.0.0.1");
    CHECK(port==10000);

    address=":123";
    ok=IpAddressHelper::SplitAddress(address, ip, port);
    CHECK(ok);
    CHECK(ip=="");
    CHECK(port==123);

    address="192.168.100.100 - 12345";
    ok=IpAddressHelper::SplitAddress(address, ip, port);
    CHECK(!ok);

    try
    {
        ip="127.0.0.1";
        port=10000;
        IpAddressHelper::CreateEndpoint(ip, port);
    }
    catch (const std::logic_error&)
    {
        CHECK(false);
    }

    try
    {
        ip="";
        port=10000;
        IpAddressHelper::CreateEndpoint(ip, port);
    }
    catch (const std::logic_error&)
    {
        CHECK(false);
    }

    try
    {
        ip="192.0.0";
        port=10000;
        IpAddressHelper::CreateEndpoint(ip, port);
        CHECK(false);
    }
    catch (const std::logic_error&)
    {
    }
}
