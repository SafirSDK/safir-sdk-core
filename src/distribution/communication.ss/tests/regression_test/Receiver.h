/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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

#include <queue>
#include "Utilities.h"

namespace Com = Safir::Dob::Internal::Com;

class Receiver
{
public:

    Receiver(Com::ControlModeTag tag, boost::asio::io_service& ioService, int64_t nodeId, int64_t nodeType);
    Receiver(Com::DataModeTag tag, boost::asio::io_service& ioService, int64_t nodeId, int64_t nodeType);

    void InjectNode(int64_t nodeId, int64_t nodeType);
    void Seed(int64_t nodeId);
    void Stop();
    void Print() const;

protected:
    boost::asio::steady_timer m_timerInclude;
    boost::asio::io_service::strand m_strand;
    Com::Communication m_com;

    std::queue<int64_t> m_newNodes;
    std::map<int64_t, std::pair<uint64_t /*expected*/, uint64_t /*lastRecvVal*/> > m_recvCount;

    bool m_running;

    void NewNode(const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& controlAddress, const std::string& dataAddress);
    void GotReceiveFrom(int64_t fromNodeId);
    void RetransmitTo(int64_t toNodeId);
    void ReceiveData(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size);
    void QueueNotFull(int64_t nodeTypeId);
    void IncludeNode();

};
