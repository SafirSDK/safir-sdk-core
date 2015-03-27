/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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
#pragma once

#include <queue>
#include <unordered_set>
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
                          Com::Communication& communication,
                          const std::unordered_set<int64_t>& nodeTypeIds,
                          RequestHandler& requesthandler,
                          PendingRegistrationHandler& prh);

        void OnPoolDistributionComplete();

        void HandleConnect(const ConnectionPtr & connection);
        void HandleDisconnect(const ConnectionPtr & connection);
        bool HandleUnsent();

    private:
        boost::asio::io_service::strand m_strand;
        Com::Communication&             m_communication;
        const std::unordered_set<int64_t> m_nodeTypeIds;
        RequestHandler&                 m_requestHandler;
        PendingRegistrationHandler&     m_pendingRegistrationHandler;

        std::queue< std::pair< boost::shared_ptr<const char[]>, size_t> > m_unsent; //vector of pair<data, size>

        bool m_poolDistributionComplete = false;

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


