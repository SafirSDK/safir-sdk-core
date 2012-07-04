/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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

#include <iostream>
#include <Safir/Dob/Internal/Connections.h>
#include <boost/lexical_cast.hpp>
#include <boost/thread.hpp>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Typesystem/Serialization.h>

using namespace Safir::Dob::Internal;

const int NUM_MSG = 1000;

long dispatched = 0;

void Dispatch(const DistributionData &, bool & exitDispatch, bool & dontRemove)
{
    ++dispatched;
    exitDispatch = false;
    dontRemove = false;
}

MessageQueue queue(10);

volatile long sent = 0;
void Sender()
{
    Safir::Dob::MessagePtr m = Safir::Dob::Message::Create();
    Safir::Dob::Typesystem::BinarySerialization ser;
    Safir::Dob::Typesystem::Serialization::ToBinary(m,ser);

    DistributionData d(message_tag,ConnectionId(100,0,100),Safir::Dob::Typesystem::ChannelId(),&ser[0]);

    for (;;)
    {
        if (sent == NUM_MSG)
        {
            std::wcout << NUM_MSG << " sent" << std::endl;
            return;
        }

        const bool res = queue.push(d);

        if (res)
        {
            ++sent;
        }
        else
        {
            boost::this_thread::yield();
        }
    }
}

int main(int, char**)
{
    boost::thread t(Sender);
    for(;;)
    {
        queue.Dispatch(Dispatch,NULL);
        if (sent == NUM_MSG && dispatched == NUM_MSG)
        {
            std::wcout << "Dispatched " << dispatched << std::endl;
            break;
        }
    }
    t.join();
    
    return 0;
}


