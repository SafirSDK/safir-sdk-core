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

#include "Receiver.h"

namespace Com = Safir::Dob::Internal::Com;

class Sender : public Receiver
{
public:

    Sender(Com::ControlModeTag tag, boost::asio::io_service& ioService, int64_t nodeId, int64_t nodeType);
    Sender(Com::DataModeTag tag, boost::asio::io_service& ioService, int64_t nodeId, int64_t nodeType);
    void Stop();
    void Print() const;

private:
    boost::asio::steady_timer m_timerSend;
    uint64_t m_msgCount;

    void Send();
};
