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
#include <cstddef>
#include <atomic>
#include <string>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    namespace Parameters
    {
        //Size of the send queue, number of outstanding messages
        static const size_t SendQueueSize = 100;

        //Max number of messages that can be sent in sequence before waiting for ack.
        //Max number of messages out of order that are saved
        static const size_t MaxSlidingWindowSize=20;

        //Max number of undelivered messages to application allowed before slowing down receiver
        static const size_t MaxNumberOfUndelivered=20;

        //Receive buffer size, must be at least FragmentSize
        static const size_t ReceiveBufferSize = 66000;

        //This is the socket send and receive buffer. On Windows the default is 8192 wich is far too low.
        static const int SocketBufferSize = 106496;

        //If no acked data has been sent for this time, the system will send a ping message to all other nodes.
        static const int SendPingThreshold = 7000; //millisec

        //Simulate network up/down. Global variable that must be initiated one time at startup.
        extern std::atomic<bool> NetworkEnabled;

        extern std::string LogPrefix;
    }
}
}
}
}
