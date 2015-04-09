/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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
    class RequestHandler;
    class PendingRegistrationHandler;
    class NodeHandler;

    class ConnectionHandler:
        private boost::noncopyable
    {
    public:
        ConnectionHandler(boost::asio::io_service& ioService,
                          Distribution& distribution,
                          const std::unordered_set<int64_t>& nodeTypeIds,
                          const std::function<void(const ConnectionPtr& connection, bool disconnecting)>& onAppEvent);

        void Stop();

    private:
        boost::asio::io_service::strand m_strand;
        Com::Communication&             m_communication;
        std::function<void(const ConnectionPtr& connection, bool disconnecting)> m_onAppEvent;
        using SendQueue=std::queue< std::pair< boost::shared_ptr<const char[]>, size_t> >;//vector of pair<data, size>
        std::unordered_map<int64_t, SendQueue> m_sendQueues; //<nodeType, SendQueue>
        boost::thread m_connectionThread;

        std::atomic_bool m_connectEvent;
        std::atomic_bool m_connectionOutEvent;
        std::atomic_bool m_handleEventsNotified;

        // Process info
        ProcessInfoHandler m_processInfoHandler;

        void SendAll(const std::pair<boost::shared_ptr<const char[]>, size_t>& data);
        void HandleSendQueues();

        void ConnectionThread();
        void HandleEvents();

        void HandleConnect(const ConnectionPtr& connection);
        void HandleDisconnect(const ConnectionPtr& connection);
        void HandleConnectionOutEvent(const ConnectionPtr& connection, std::vector<ConnectionPtr>& deadConnections);

        static inline std::pair<boost::shared_ptr<const char[]>, size_t> ConnectDataPtr(const Safir::Dob::Internal::ConnectionId& id,
                                                                                        const char* nameWithoutCounter,
                                                                                        Typesystem::Int32 counter)
        {
            DistributionData d(connect_message_tag, id, nameWithoutCounter, counter);
            boost::shared_ptr<const char[]> p(d.GetReference(), [=](const char* ptr){DistributionData::DropReference(ptr);});
            return std::make_pair(std::move(p), d.Size());
        }

        static inline std::pair<boost::shared_ptr<const char[]>, size_t> DisconnectDataPtr(const Safir::Dob::Internal::ConnectionId& id)
        {
            DistributionData d(disconnect_message_tag, id);
            boost::shared_ptr<const char[]> p(d.GetReference(), [=](const char* ptr){DistributionData::DropReference(ptr);});
            return std::make_pair(std::move(p), d.Size());
        }
    };
}
}
}


