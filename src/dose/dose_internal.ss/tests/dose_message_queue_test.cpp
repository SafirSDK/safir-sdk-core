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
#include <Safir/Dob/Internal/ConnectionId.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/MessageQueue.h>
#include <Safir/Dob/Internal/StateDeleter.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
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
    
    if (dispatched % 100 == 0)
    {
        lllerr << "Dispatched " << dispatched << std::endl;
    }

    exitDispatch = false;
    dontRemove = false;
}

MessageQueue queue(10);

void Sender(long& sent)
{
    Safir::Dob::MessagePtr m = Safir::Dob::Message::Create();
    Safir::Dob::Typesystem::BinarySerialization ser;
    Safir::Dob::Typesystem::Serialization::ToBinary(m,ser);

    DistributionData d(message_tag,ConnectionId(100,0,100),Safir::Dob::Typesystem::ChannelId(),&ser[0]);
    lllerr << "Push loop starting (in thread)" << std::endl;
    for (;;)
    {
        if (sent == NUM_MSG)
        {
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
            continue;
        }

        if (sent % 100 == 0)
        {
            lllerr << sent << " sent" << std::endl;
        }
    }
}

int main(int, char**)
{
    lllerr << "Starting thread" << std::endl;
    long sent = 0;
    boost::thread t(boost::bind(Sender,boost::ref(sent)));

    lllerr << "Dispatch loop starting" << std::endl;
    for(;;)
    {
        const size_t res = queue.Dispatch(Dispatch,NULL);
        
        if (res == 0)
        {
            boost::this_thread::yield();
            continue;
        }

        if (dispatched == NUM_MSG)
        {
            break;
        }
    }
    lllerr << "Joining thread" << std::endl;
    t.join();
    if (sent != NUM_MSG)
    {
        lllerr << "unexpected number of sent " << sent << std::endl;
        return 1;
    }
    lllerr << "all seems good" << std::endl;
    return 0;
}


