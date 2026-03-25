/******************************************************************************
*
* Copyright Saab AB, 2026 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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

#ifdef _MSC_VER
#pragma warning(disable: 4127)
#endif

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

    // IPv6 SplitAddress: [::1]:9000
    address="[::1]:9000";
    ok=IpAddressHelper::SplitAddress(address, ip, port);
    CHECK(ok);
    CHECK(ip=="[::1]");
    CHECK(port==9000);

    // IPv6 SplitAddress: full address
    address="[2001:db8::1]:8080";
    ok=IpAddressHelper::SplitAddress(address, ip, port);
    CHECK(ok);
    CHECK(ip=="[2001:db8::1]");
    CHECK(port==8080);

    // IPv6 SplitAddress: no port (no colon after bracket) — returns false
    address="[::1]";
    ok=IpAddressHelper::SplitAddress(address, ip, port);
    CHECK(!ok);

    // CreateEndpoint with bare IPv6 address (no brackets) — must succeed
    try
    {
        ip="::1";
        port=9000;
        auto ep = IpAddressHelper::CreateEndpoint(ip, port);
        CHECK(ep.port()==9000);
    }
    catch (const std::logic_error&)
    {
        CHECK(false);
    }

    // CreateEndpoint with invalid IP string — must throw std::logic_error
    try
    {
        ip="not_an_ip";
        port=1234;
        IpAddressHelper::CreateEndpoint(ip, port);
        CHECK(false);
    }
    catch (const std::logic_error&)
    {
        // expected
    }

}
