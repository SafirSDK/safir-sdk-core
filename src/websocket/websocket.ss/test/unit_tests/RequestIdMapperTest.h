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
#include "../../src/RequestIdMapper.h"

#define CHECK(expr) {if (!(expr)) { std::cout<<"Test failed! Line: "<<__LINE__<<", expr: "<< #expr <<std::endl; exit(1);}}

inline void RequestIdMapperTest()
{
    RequestIdMapper rm;
    CHECK(rm.Count()==0);
    auto id=rm.Get(123);
    CHECK(id.IsNull());
    rm.Add(1, JsonRpcId("strVal"));
    rm.Add(2, JsonRpcId(123));
    CHECK(rm.Count()==2);
    id=rm.Get(1);
    CHECK(rm.Count()==1);
    CHECK(id.HasInt()==false);
    CHECK(id.HasStr()==true);
    CHECK(id.String()=="strVal");
    id=rm.Get(1);
    CHECK(id.IsNull());
    CHECK(rm.Count()==1);
    id=rm.Get(2);
    CHECK(rm.Count()==0);
    CHECK(id.HasInt()==true);
    CHECK(id.HasStr()==false);
    CHECK(id.Int()==123);
    CHECK(rm.Count()==0);
    id=rm.Get(2);
    CHECK(id.IsNull());
    CHECK(rm.Count()==0);
}
