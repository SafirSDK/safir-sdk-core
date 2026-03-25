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
#include "JsonRpcTest.h"
#include "PingHandlerTest.h"
#include "RequestIdMapperTest.h"
#include "ResponseSenderStoreTest.h"
#include "IpAddressHelperTest.h"
#include "CommandValidatorTest.h"
#include "RestRoutingTest.h"

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

    std::cout<<"===== CommandValidatorTest ====="<<std::endl;
    CommandValidatorTest();
    std::cout<<"Test passed!"<<std::endl;
    std::cout<<"===== RestRoutingTest =====" <<std::endl;
    RestRoutingTest();
    std::cout<<"Test passed!"<<std::endl;
    std::cout<<"===== PingHandlerTest ====="<<std::endl;
    PingHandlerTest();
    std::cout<<"Test passed!"<<std::endl;

    return 0;
}

