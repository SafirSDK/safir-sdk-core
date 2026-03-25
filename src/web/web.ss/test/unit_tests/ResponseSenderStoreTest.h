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
#include "../../src/ResponseSenderStore.h"

#define CHECK(expr) {if (!(expr)) { std::cout<<"Test failed! Line: "<<__LINE__<<", expr: "<< #expr <<std::endl; exit(1);}}

class ResSnd : public sd::ResponseSender
{
public:
    static std::shared_ptr<ResSnd> Create(sd::RequestId id)
    {
        auto p=std::shared_ptr<ResSnd>(new ResSnd(id));
        return p;
    }

    ResSnd(sd::RequestId id): m_id(id) {}
    void Send(const Safir::Dob::ResponsePtr&) override {}
    bool IsDone() override {return false;}
    void Discard() override {}
    sd::RequestId Id() const {return m_id;}
private:
    sd::RequestId m_id;
};

inline void ResponseSenderStoreTest()
{
    const ts::TypeId typeA = 1;
    const ts::TypeId typeB = 2;
    const ts::HandlerId handlerA(1);
    const ts::HandlerId handlerB(2);

    ResponseSenderStore rs(2);
    CHECK(rs.Count()==0);
    auto s=rs.Get(1);
    CHECK(s==nullptr);

    auto id=rs.Add(ResSnd::Create(1), typeA, handlerA);
    CHECK(id==1);
    CHECK(rs.Count()==1);
    id=rs.Add(ResSnd::Create(2), typeA, handlerA);
    CHECK(id==2);
    CHECK(rs.Count()==2);

    s=rs.Get(3);
    CHECK(s==nullptr);
    CHECK(rs.Count()==2);

    s=rs.Get(1);
    CHECK(std::dynamic_pointer_cast<ResSnd>(s)->Id()==1);
    CHECK(rs.Count()==1);

    s=rs.Get(2);
    CHECK(std::dynamic_pointer_cast<ResSnd>(s)->Id()==2);
    CHECK(rs.Count()==0);

    id=rs.Add(ResSnd::Create(3), typeA, handlerA);
    CHECK(id==3);
    id=rs.Add(ResSnd::Create(4), typeA, handlerA);
    CHECK(id==4);
    CHECK(rs.Count()==2);
    id=rs.Add(ResSnd::Create(5), typeA, handlerA);
    CHECK(id==5);
    CHECK(rs.Count()==2);

    s=rs.Get(3);
    CHECK(s==nullptr);
    CHECK(rs.Count()==2);

    s=rs.Get(4);
    CHECK(std::dynamic_pointer_cast<ResSnd>(s)->Id()==4);
    CHECK(rs.Count()==1);

    s=rs.Get(5);
    CHECK(std::dynamic_pointer_cast<ResSnd>(s)->Id()==5);
    CHECK(rs.Count()==0);

    id=rs.Add(ResSnd::Create(6), typeA, handlerA);
    CHECK(id==6);
    id=rs.Add(ResSnd::Create(7), typeA, handlerA);
    CHECK(id==7);
    id=rs.Add(ResSnd::Create(8), typeA, handlerA);
    CHECK(id==8);
    id=rs.Add(ResSnd::Create(9), typeA, handlerA);
    CHECK(id==9);

    s=rs.Get(6);
    CHECK(s==nullptr);

    s=rs.Get(7);
    CHECK(s==nullptr);

    CHECK(rs.Count()==2);

    s=rs.Get(8);
    CHECK(std::dynamic_pointer_cast<ResSnd>(s)->Id()==8);
    CHECK(rs.Count()==1);

    s=rs.Get(9);
    CHECK(std::dynamic_pointer_cast<ResSnd>(s)->Id()==9);
    CHECK(rs.Count()==0);

    s=rs.Get(10);
    CHECK(s==nullptr);
    CHECK(rs.Count()==0);

    // Test DiscardForHandler: only matching handler entries are discarded
    rs.Add(ResSnd::Create(11), typeA, handlerA);
    auto idB = rs.Add(ResSnd::Create(12), typeB, handlerB);
    CHECK(rs.Count()==2);
    rs.DiscardForHandler(typeA, handlerA);
    CHECK(rs.Count()==1);
    s=rs.Get(idB);
    CHECK(std::dynamic_pointer_cast<ResSnd>(s)->Id()==12);
    CHECK(rs.Count()==0);

    // Test DiscardAll
    rs.Add(ResSnd::Create(13), typeA, handlerA);
    rs.Add(ResSnd::Create(14), typeB, handlerB);
    CHECK(rs.Count()==2);
    rs.DiscardAll();
    CHECK(rs.Count()==0);
}
