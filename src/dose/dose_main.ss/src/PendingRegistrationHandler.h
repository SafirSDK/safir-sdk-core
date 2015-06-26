/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/ConnectionId.h>
#include <map>
#include <set>
#include <atomic>
#include "Distribution.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class PendingRegistrationHandler :
        public boost::noncopyable
    {
    public:
        PendingRegistrationHandler(boost::asio::io_service& ioService,
                                   Distribution& distribution);

        void Stop();

        void CheckForNewOrRemovedPendingRegistration(const ConnectionPtr & connection);

        void CheckForPending(const Safir::Dob::Typesystem::TypeId typeId);

        void RemovePendingRegistrations(const ConnectionId & id);

        void CheckForPending();
    private:
        //check if the request is completed, and if so signal the application
        //returns false if the request is not completed
        bool HandleCompletion(const long requestId);

        void SendRequest(const long requestId);

        void TryPendingRegistration(const long requestId);

        //handle remote incoming request
        void HandleRequest(const DistributionData & msg,
                           const int64_t fromNodeId,
                           const int64_t fromNodeType);

        struct PendingRegistrationInfo
        {
            PendingRegistrationInfo(boost::asio::io_service& ioService,
                                    const ConnectionId connId,
                                    const Dob::Typesystem::TypeId type,
                                    const Dob::Typesystem::HandlerId&  handler):
                connectionId(connId),
                typeId(type),
                handlerId(handler),
                timer(ioService),
                nbrOfSentRequests(0),
                lastRequestTimestamp(),
                acceptedNodes(),
                rejected(false){}

            ConnectionId connectionId;
            Dob::Typesystem::TypeId typeId;
            Dob::Typesystem::HandlerId handlerId;
            boost::asio::steady_timer timer;
            unsigned int nbrOfSentRequests;
            LamportTimestamp lastRequestTimestamp;
            std::set<int64_t> acceptedNodes;
            bool rejected;
        };
        typedef std::map<long,std::unique_ptr<PendingRegistrationInfo>> PendingRegistrations;

        std::atomic<bool> m_stopped;

        boost::asio::strand m_strand;

        Distribution& m_distribution;
        const int64_t m_dataTypeIdentifier;
        std::map<int64_t,int64_t> m_liveNodes;

        PendingRegistrations m_pendingRegistrations;
        long m_nextId;

        LamportClock m_pendingRegistrationClock;
    };
}
}
}
