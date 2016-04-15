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
        std::cout<<"ResolverTest started"<<std::endl;
        boost::asio::io_service io;
        auto work=boost::make_shared<boost::asio::io_service::work>(io);

        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        bool success=true;
        Com::Resolver resolver(io);
        auto resolveLocal=[&](const std::string& expr)
        {
            try
            {
                auto ep=resolver.ResolveLocalEndpoint(expr);
                std::cout<<"ResolvedLocal "<<expr<<" to '"<<ep<<"'"<<std::endl;
            }
            catch(const std::exception& e)
            {
                success=false;
                std::cout<<"Got Exception in ResolvedLocal: "<<e.what()<<std::endl;
            }
        };

        auto resolveRemote=[&](const std::string& expr, int protocol)
        {
            try
            {
                auto ep=resolver.ResolveRemoteEndpoint(expr, protocol);
                std::cout<<"ResolvedRemote "<<expr<<" to '"<<ep<<"'"<<std::endl;
            }
            catch(const std::exception& e)
            {
                success=false;
                std::cout<<"Got Exception in ResolvedRemote: "<<e.what()<<std::endl;
            }
        };

        CHECK(resolver.DiffIndex("abcdefg", "abdcefg")==2);
        CHECK(resolver.DiffIndex("bcdefg", "abdcefg")==0);
        CHECK(resolver.DiffIndex("abcdefg", "abcdefg")==7);

        std::vector<std::string> v;

        v.push_back("192.168.0.100");
        v.push_back("192.168.66.100");
        v.push_back("192.0.0.0");
        v.push_back("192.168.0.0");
        v.push_back("192.255.255.255");

        CHECK(resolver.FindBestMatch("192.168.66.*", v)=="192.168.66.100");
        CHECK(resolver.FindBestMatch("192.0.*.*", v)=="192.0.0.0");
        CHECK(resolver.FindBestMatch("192.168.*.*", v)=="192.168.0.100");
        CHECK(resolver.FindBestMatch("*.*.*.*", v)=="192.168.0.100");
        CHECK(resolver.FindBestMatch("192.168.0.101", v)=="");
        CHECK(resolver.FindBestMatch("asdfasdf", v)=="");
        CHECK(resolver.FindBestMatch("", v)=="");

        CHECK(resolver.ResolveLocalEndpoint("127.0.0.1:11111") == "127.0.0.1:11111");
        CHECK(resolver.ResolveLocalEndpoint("127.0.0.*:11111") == "127.0.0.1:11111");
        CHECK(resolver.ResolveLocalEndpoint("127.0.*.*:11111") == "127.0.0.1:11111");
        try
        {
            resolver.ResolveLocalEndpoint("whut:11111");
            CHECK(false);
        }
        catch(...)
        {
            CHECK(true);
        }
#ifndef _MSC_VER
        CHECK(resolver.ResolveLocalEndpoint("lo:123") == "127.0.0.1:123");

#endif
        std::cout<<"Testing resolve local endpoint"<<std::endl;
        resolveLocal("192.168.*.*:12345");
        resolveLocal("eth0:10000");
        resolveLocal("lo:12000");

        std::cout<<"Testing resolve remote endpoint"<<std::endl;
        resolveRemote("safir-salt-router:10000", 4);
        resolveRemote("192.168.211.157:10000", 4);
        resolveRemote("google.com:10000", 4);
        TRACELINE

        work.reset();
        threads.join_all();
        std::cout<<"Resolver tests passed"<<std::endl;
    }

private:
};
