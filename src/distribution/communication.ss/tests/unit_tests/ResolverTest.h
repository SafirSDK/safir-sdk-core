/******************************************************************************
*
* Copyright Saab AB, 2013-2015 (http://safirsdkcore.com)
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
#pragma once

#include "fwd.h"

class ResolverTest
{
public:
    static void Run()
    {
        std::wcout<<"ResolverTest started"<<std::endl;
        boost::asio::io_context io;
        auto work=boost::asio::make_work_guard(io);

        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        bool success=true;
        auto resolveLocal=[&](const std::string& expr)
        {
            try
            {
                auto ep=Com::Resolver::ResolveLocalEndpoint(expr);
                std::wcout<<"ResolvedLocal "<<expr.c_str()<<" to '"<<ep.c_str()<<"'"<<std::endl;
            }
            catch(const std::exception& e)
            {
                success=false;
                std::wcout<<"Got Exception in ResolvedLocal: "<<e.what()<<std::endl;
            }
        };

        Com::Resolver resolver(io,true);
        auto resolveRemote=[&](const std::string& expr, int protocol)
        {
            try
            {
                auto ep=resolver.ResolveRemoteEndpoint(expr, protocol);
                std::wcout<<"ResolvedRemote "<<expr.c_str()<<" to '"<<ep.c_str()<<"'"<<std::endl;
            }
            catch(const std::exception& e)
            {
                success=false;
                std::wcout<<"Got Exception in ResolvedRemote: "<<e.what()<<std::endl;
            }
        };

        std::vector<std::string> v;

        v.push_back("192.168.0.100");
        v.push_back("192.168.66.100");
        v.push_back("192.0.0.0");
        v.push_back("192.168.0.0");
        v.push_back("192.255.255.255");

        CHECK(Com::Resolver::FindBestMatch("192.168.66.*", v, true)=="192.168.66.100");
        CHECK(Com::Resolver::FindBestMatch("192.0.*.*", v, true)=="192.0.0.0");
        CHECK(Com::Resolver::FindBestMatch("192.168.*.*", v, true)=="192.168.0.100");
        CHECK(Com::Resolver::FindBestMatch("*.*.*.*", v, true)=="192.168.0.100");
        CHECK(Com::Resolver::FindBestMatch("192.168.0.101", v, true)=="");
        CHECK(Com::Resolver::FindBestMatch("asdfasdf", v, true)=="");
        CHECK(Com::Resolver::FindBestMatch("", v, true)=="");

        CHECK(Com::Resolver::ResolveLocalEndpoint("127.0.0.1:11111", true) == "127.0.0.1:11111");
        CHECK(Com::Resolver::ResolveLocalEndpoint("127.0.0.*:11111", true) == "127.0.0.1:11111");
        CHECK(Com::Resolver::ResolveLocalEndpoint("127.0.*.*:11111", true) == "127.0.0.1:11111");
        CHECK(Com::Resolver::ResolveLocalEndpoint("whut:11111", true) == "");
        CHECK(Com::Resolver::ResolveLocalEndpoint("0.0.0.0:11111", true) == "0.0.0.0:11111");

#ifndef _MSC_VER
        CHECK(Com::Resolver::ResolveLocalEndpoint("lo:123",true) == "127.0.0.1:123");

        //GetAdapters must enumerate every interface that carries an IP address,
        //including layer-3-only interfaces with no link-layer (AF_PACKET) entry.
        //We cannot create such an interface in a unit test, but we can verify the
        //enumeration is sane: every entry has a name and address, and the loopback
        //IPv4 address is present (it is preferred over the IPv6 ::1 for name "lo").
        {
            const auto adapters = Com::Resolver::GetAdapters();
            CHECK(!adapters.empty());
            bool foundLoopbackV4 = false;
            for (const auto& a : adapters)
            {
                CHECK(!a.name.empty());
                CHECK(!a.ipAddress.empty());
                CHECK(a.ipVersion == 4 || a.ipVersion == 6);
                if (a.name == "lo" && a.ipAddress == "127.0.0.1")
                {
                    foundLoopbackV4 = true;
                }
            }
            CHECK(foundLoopbackV4);
        }
#endif

        //GetAdapters must enumerate every interface that carries an IP address,
        //including layer-3-only tunnel interfaces (WireGuard, OpenVPN TUN, etc.).
        //We cannot create such an interface in a unit test, but we can verify the
        //enumeration is sane: every entry has a name and address, and the loopback
        //IPv4 address is present (it is preferred over the IPv6 loopback because
        //IPv4 addresses are collected first).
        {
            const auto adapters = Com::Resolver::GetAdapters();
            CHECK(!adapters.empty());
            bool foundLoopbackV4 = false;
            for (const auto& a : adapters)
            {
                CHECK(!a.name.empty());
                CHECK(!a.ipAddress.empty());
                CHECK(a.ipVersion == 4 || a.ipVersion == 6);
                if (a.ipAddress == "127.0.0.1")
                {
                    foundLoopbackV4 = true;
                }
            }
            CHECK(foundLoopbackV4);
        }

        //Name-based local resolution: an interface name must resolve to that
        //interface's address. We cannot hardcode a name that exists on every
        //platform (it is "lo" on Linux but a localized FriendlyName like
        //"Ethernet" on Windows), so we pick a real IPv4 interface from the
        //enumeration and verify it resolves back to its own address. This
        //exercises the Windows name path enabled by FriendlyName (#607), which
        //previously could not match a name since name equalled the IP address.
        {
            const auto adapters = Com::Resolver::GetAdapters();
            const Com::Resolver::AdapterInfo* v4 = nullptr;
            for (const auto& a : adapters)
            {
                //Skip names containing ':' - ResolveLocalEndpoint splits the
                //port off at the first colon, so a name like "eth0:0" would be
                //mis-parsed. (This is the pre-existing name/port ambiguity, not
                //what we are testing here.)
                if (a.ipVersion == 4 && a.name.find(':') == std::string::npos)
                {
                    v4 = &a;
                    break;
                }
            }
            CHECK(v4 != nullptr);
            if (v4 != nullptr)
            {
                const auto resolved = Com::Resolver::ResolveLocalEndpoint(v4->name + ":4242", true);
                CHECK(resolved == v4->ipAddress + ":4242");
            }
        }

        std::wcout<<"Testing resolve local endpoint"<<std::endl;
        resolveLocal("192.168.*.*:12345");
        resolveLocal("eth0:10000");

        std::wcout<<"Testing resolve remote endpoint"<<std::endl;

        CHECK(resolver.ResolveRemoteEndpoint("localhost:100",4) == "127.0.0.1:100");
        resolveRemote("safir-salt-router:10000", 4);
        resolveRemote("192.168.211.157:10000", 4);
        resolveRemote("google.com:10000", 4);
        TRACELINE

        work.reset();
        threads.join_all();
        std::wcout<<"Resolver tests passed"<<std::endl;
    }

private:
};
