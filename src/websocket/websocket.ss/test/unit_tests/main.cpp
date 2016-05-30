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
#include "JsonRpcTest.h"
#include "PingHandlerTest.h"
#include "RequestIdMapperTest.h"
#include "ResponseSenderStoreTest.h"
#include "ProxyToJsonTest.h"
#include "IpAddressHelperTest.h"

int main(int /*argc*/, const char** /*argv*/)
{
    std::cout<<"===== IpAddressHelperTest ====="<<std::endl;
    IpAddressHelperTest();
    std::cout<<"Test passed!"<<std::endl;

    std::cout<<"===== RequestIdMapperTest ====="<<std::endl;
    RequestIdMapperTest();
    std::cout<<"Test passed!"<<std::endl;

    std::cout<<"===== ResponseSenderStoreTest ====="<<std::endl;
    ResponseSenderStoreTest();
    std::cout<<"Test passed!"<<std::endl;

    std::cout<<"===== JsonRpcTest ====="<<std::endl;
    JsonRpcTest();
    std::cout<<"Test passed!"<<std::endl;

    std::cout<<"===== ProxyToJsonTest ====="<<std::endl;
    ProxyToJsonTest();
    std::cout<<"Test passed!"<<std::endl;

    std::cout<<"===== PingHandlerTest ====="<<std::endl;
    PingHandlerTest();
    std::cout<<"Test passed!"<<std::endl;

    return 0;
}

