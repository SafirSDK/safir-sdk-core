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

#ifndef __DOSE_PENDING_REGISTRATION_HANDLER_H__
#define __DOSE_PENDING_REGISTRATION_HANDLER_H__

#include <Safir/Dob/Internal/InternalFwd.h>
#include "dose_main_timers.h"
#include "dose_main_defs.h"
#include <map>
#include "dose_main_communication.h"
#include "dose_main_node_handler.h"
#include <bitset>
namespace Safir
{
namespace Dob
{
namespace Internal
{
    typedef TimerInfo<long> ResendPendingTimerInfo;

    class PendingRegistrationHandler :
        public TimeoutHandler,
        public boost::noncopyable
    {
    public:
        PendingRegistrationHandler(ExternNodeCommunication & ecom, NodeHandler & nodeHandler);

        void CheckForNewOrRemovedPendingRegistration(const ConnectionPtr & connection);


        void CheckForPending(const Safir::Dob::Typesystem::TypeId typeId);
        void CheckForPending();

        void RemovePendingRegistrations(const ConnectionId & id);

        void HandleMessageFromDoseCom(const DistributionData & msg);
    private:
        virtual void HandleTimeout(const TimerInfoPtr & timer);

        //check if the request is completed, and if so signal the application
        //returns false if the request is not completed
        bool HandleCompletion(const long requestId);

        void SendRequest(const long requestId);


        void TryPendingRegistration(const long requestId);

        struct PendingRegistrationInfo
        {
            PendingRegistrationInfo(const ConnectionId connId,
                                    const Dob::Typesystem::TypeId type,
                                    const Dob::Typesystem::HandlerId& handler):
                connectionId(connId),
                typeId(type),
                handlerId(handler),
                nextRequestTime(),
                nbrOfSentRequests(0),
                lastRequestTimestamp(),
                acceptedNodes(false),
                rejected(false){}

            ConnectionId connectionId;
            Dob::Typesystem::TypeId typeId;
            Dob::Typesystem::HandlerId handlerId;
            double nextRequestTime;
            unsigned int nbrOfSentRequests;
            LamportTimestamp lastRequestTimestamp;
            std::bitset<NUM_NODES> acceptedNodes;
            bool rejected;
        };
        typedef std::map<long,PendingRegistrationInfo> PendingRegistrations;

        PendingRegistrations m_pendingRegistrations;
        long m_nextId;
        ExternNodeCommunication & m_ecom;
        NodeHandler & m_nodeHandler;

        LamportClock m_pendingRegistrationClock;

        TimerId m_timerId;
    };
}
}
}

#endif

