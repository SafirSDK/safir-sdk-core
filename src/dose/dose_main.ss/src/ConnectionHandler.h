/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
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
#include <unordered_set>
#include <unordered_map>
#include <boost/noncopyable.hpp>
#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif
#include <boost/asio.hpp>
#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Internal/Connections.h>
#include "ProcessInfoHandler.h"
#include "PoolHandler.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
    //forward declarations:
    namespace Com
    {
        class Communication;
    }

    class ConnectionHandler:
        private boost::noncopyable
    {
    public:
        ConnectionHandler(boost::asio::io_service& ioService,
                          Distribution& distribution,
                          const std::function<void(const ConnectionPtr& connection, bool disconnecting)>& onAppEvent,
                          const std::function<void(int64_t)>& checkPendingReg,
                          const std::function<void(const std::string& str)>& logStatus);

        void Start();
        void Stop();

    private:
        std::atomic<bool> m_running = false;
        std::atomic<bool> m_connectionThreadRunning = false;
        boost::asio::io_service::strand m_strand;
        Com::Communication&             m_communication;
        std::function<void(const ConnectionPtr& connection, bool disconnecting)> m_onAppEvent;
        typedef std::queue< std::pair<Safir::Utilities::Internal::SharedConstCharArray, size_t>> SendQueue;//vector of pair<data, size>
        std::unordered_map<int64_t, SendQueue> m_sendQueues; //<nodeType, SendQueue>
        boost::thread m_connectionThread;

        std::atomic<bool> m_connectEvent;
        std::atomic<bool> m_connectionOutEvent;
        std::atomic<bool> m_handleEventsNotified;

        PoolHandler m_poolHandler;
        ProcessInfoHandler m_processInfoHandler;

        const bool m_keepStateWhileDetached;

        void SendAll(const std::pair<Safir::Utilities::Internal::SharedConstCharArray, size_t>& data);
        void HandleSendQueues();

        void ConnectionThread();
        void HandleEvents();

        void HandleConnect(const ConnectionPtr& connection);
        void HandleDisconnect(const ConnectionPtr& connection);
        void HandleConnectionOutEvent(const ConnectionPtr& connection, std::vector<ConnectionPtr>& deadConnections);

        void StopConnectionThread();

        static inline std::pair<Safir::Utilities::Internal::SharedConstCharArray, size_t> ConnectDataPtr(const Safir::Dob::Internal::ConnectionId& id,
                                                                                        const char* nameWithoutCounter,
                                                                                        Typesystem::Int32 counter)
        {
            DistributionData d(connect_message_tag, id, nameWithoutCounter, counter);
            Safir::Utilities::Internal::SharedConstCharArray p(d.GetReference(), [=](const char* ptr){DistributionData::DropReference(ptr);});
            return std::make_pair(std::move(p), d.Size());
        }

        static inline std::pair<Safir::Utilities::Internal::SharedConstCharArray, size_t> DisconnectDataPtr(const Safir::Dob::Internal::ConnectionId& id)
        {
            DistributionData d(disconnect_message_tag, id);
            Safir::Utilities::Internal::SharedConstCharArray p(d.GetReference(), [=](const char* ptr){DistributionData::DropReference(ptr);});
            return std::make_pair(std::move(p), d.Size());
        }
    };
}
}
}


