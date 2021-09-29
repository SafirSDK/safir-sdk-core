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
        boost::asio::io_service io;
        auto work=std::make_shared<boost::asio::io_service::work>(io);

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
#endif

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
