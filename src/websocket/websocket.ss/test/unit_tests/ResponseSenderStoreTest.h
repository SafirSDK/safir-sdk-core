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
#include "../../src/ResponseSenderStore.h"

#define CHECK(expr) {if (!(expr)) { std::cout<<"Test failed! Line: "<<__LINE__<<", expr: "<< #expr <<std::endl; exit(1);}}

class ResSnd : public sd::ResponseSender
{
public:
    static boost::shared_ptr<ResSnd> Create(sd::RequestId id)
    {
        auto p=boost::shared_ptr<ResSnd>(new ResSnd(id));
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
    ResponseSenderStore rs(2);
    CHECK(rs.Count()==0);
    auto s=rs.Get(1);
    CHECK(s==nullptr);

    auto id=rs.Add(ResSnd::Create(1));
    CHECK(id==1);
    CHECK(rs.Count()==1);
    id=rs.Add(ResSnd::Create(2));
    CHECK(id==2);
    CHECK(rs.Count()==2);

    s=rs.Get(3);
    CHECK(s==nullptr);
    CHECK(rs.Count()==2);

    s=rs.Get(1);
    CHECK(boost::dynamic_pointer_cast<ResSnd>(s)->Id()==1);
    CHECK(rs.Count()==1);

    s=rs.Get(2);
    CHECK(boost::dynamic_pointer_cast<ResSnd>(s)->Id()==2);
    CHECK(rs.Count()==0);

    id=rs.Add(ResSnd::Create(3));
    CHECK(id==3);
    id=rs.Add(ResSnd::Create(4));
    CHECK(id==4);
    CHECK(rs.Count()==2);
    id=rs.Add(ResSnd::Create(5));
    CHECK(id==5);
    CHECK(rs.Count()==2);

    s=rs.Get(3);
    CHECK(s==nullptr);
    CHECK(rs.Count()==2);

    s=rs.Get(4);
    CHECK(boost::dynamic_pointer_cast<ResSnd>(s)->Id()==4);
    CHECK(rs.Count()==1);

    s=rs.Get(5);
    CHECK(boost::dynamic_pointer_cast<ResSnd>(s)->Id()==5);
    CHECK(rs.Count()==0);

    id=rs.Add(ResSnd::Create(6));
    CHECK(id==6);
    id=rs.Add(ResSnd::Create(7));
    CHECK(id==7);
    id=rs.Add(ResSnd::Create(8));
    CHECK(id==8);
    id=rs.Add(ResSnd::Create(9));
    CHECK(id==9);

    s=rs.Get(6);
    CHECK(s==nullptr);

    s=rs.Get(7);
    CHECK(s==nullptr);

    CHECK(rs.Count()==2);

    s=rs.Get(8);
    CHECK(boost::dynamic_pointer_cast<ResSnd>(s)->Id()==8);
    CHECK(rs.Count()==1);

    s=rs.Get(9);
    CHECK(boost::dynamic_pointer_cast<ResSnd>(s)->Id()==9);
    CHECK(rs.Count()==0);

    s=rs.Get(10);
    CHECK(s==nullptr);
    CHECK(rs.Count()==0);
}
