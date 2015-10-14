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

class MessageQueueTest
{
public:
    static void Run()
    {
        std::cout<<"MessageQueue started"<<std::endl;

        typedef Com::MessageQueue<std::string> MQ;
        MQ mq(5);

        CHECK(!mq.has_unhandled());

        mq.enqueue("a");
        mq.enqueue("b");
        mq.enqueue("c");
        mq.enqueue("d");

        TRACELINE

        CHECK(mq.size()==4);
        CHECK(mq.has_unhandled());
        CHECK(mq.first_unhandled_index()==0);
        //mq.DumpInfo(std::cout);

        mq.step_unhandled();
        mq.step_unhandled();
        CHECK(mq.size()==4);
        CHECK(mq.first_unhandled_index()==2);
        //mq.DumpInfo(std::cout);

        mq.enqueue("e");
        CHECK(mq.first_unhandled_index()==2);
        CHECK(mq.size()==5);
        CHECK(mq.full());
        //mq.DumpInfo(std::cout);

        TRACELINE

        mq.dequeue();

        CHECK(mq.has_unhandled());
        CHECK(mq.first_unhandled_index()==1);
        CHECK(mq.size()==4);
        CHECK(!mq.full());
        //mq.DumpInfo(std::cout);

        CHECK(mq[0]=="b");
        CHECK(mq[1]=="c");
        CHECK(mq[2]=="d");
        CHECK(mq[3]=="e");

        mq.enqueue("f");
        mq.enqueue("g");

        TRACELINE

        CHECK(mq.first_unhandled_index()==1);
        CHECK(mq.size()==5);
        CHECK(mq.full());
        //mq.DumpInfo(std::cout);

        mq.enqueue("h");

        CHECK(mq.first_unhandled_index()==1);
        CHECK(mq.size()==5);
        CHECK(mq.full());
        //mq.DumpInfo(std::cout);

        mq.dequeue();
        CHECK(mq.has_unhandled());
        CHECK(mq.first_unhandled_index()==0);
        mq.dequeue();

        CHECK(mq.has_unhandled());
        CHECK(mq.first_unhandled_index()==0);
        CHECK(mq.size()==5);
        CHECK(mq.full());
        //mq.DumpInfo(std::cout);

        CHECK(mq[0]=="d");
        CHECK(mq[1]=="e");
        CHECK(mq[2]=="f");
        CHECK(mq[3]=="g");
        CHECK(mq[4]=="h");

        mq.step_unhandled();
        mq.step_unhandled();
        mq.step_unhandled();
        CHECK(mq.first_unhandled_index()==3);
        mq.dequeue();
        mq.dequeue();

        CHECK(mq.has_unhandled());
        CHECK(mq.first_unhandled_index()==1);
        CHECK(mq.size()==3);
        CHECK(!mq.full());
        //mq.DumpInfo(std::cout);

        CHECK(mq[0]=="f");
        CHECK(mq[1]=="g");
        CHECK(mq[2]=="h");

        mq.dequeue();
        mq.dequeue();

        CHECK(mq.has_unhandled());
        CHECK(mq.first_unhandled_index()==0);
        CHECK(mq.size()==1);
        CHECK(!mq.full());
        //mq.DumpInfo(std::cout);

        CHECK(mq[0]=="h");

        mq.dequeue();

        CHECK(!mq.has_unhandled());
        CHECK(mq.first_unhandled_index()==mq.size());
        CHECK(mq.size()==0);
        CHECK(!mq.full());
        CHECK(mq.empty());

        std::cout<<"MessageQueue tests passed"<<std::endl;

        //--------------------------------
        // Circular array
        //--------------------------------
        std::cout<<"CircularArray started"<<std::endl;

        Com::CircularArray<std::string> arr(5);
        arr[0]="Kalle";
        arr[3]="Pelle";
        arr.Step(2);
        CHECK(arr[1]=="Pelle");
        arr[3]="Biffen";
        arr.Step();
        CHECK(arr[2]=="Biffen");

        std::cout<<"CircularArray tests passed"<<std::endl;
    }
};
