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

#ifndef __DOSE_MAIN_PERSIST_HANDLER_H__
#define __DOSE_MAIN_PERSIST_HANDLER_H__

#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Connection.h>
#include "dose_main_timers.h"
#include <boost/noncopyable.hpp>
#include <deque>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class ExternNodeCommunication;
    class ConnectionHandler;
    class NodeHandler;

    class PersistHandler:
        public Safir::Dob::ServiceHandler,
        public TimeoutHandler,
        private boost::noncopyable
    {
    public:
        PersistHandler();

        void Init(ExternNodeCommunication& ecom, 
                  ConnectionHandler& connectionHandler,
                  NodeHandler& nodeHandler,
                  const bool otherNodesExistAtStartup);

        //only to be called when the service persistencedataready is called
        //or when we have fulfilled other criteria for receiving persistence data
        void SetPersistentDataReady();
        bool IsPersistentDataReady() const;

        void HandleMessageFromDoseCom(const DistributionData& data);
    private:
        virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    typeId,
                                           const Safir::Dob::Typesystem::HandlerId& handlerId);

        virtual void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                      Safir::Dob::ResponseSenderPtr   responseSender);

        
        virtual void HandleTimeout(const TimerInfoPtr & timer);
        
        void RequestPersistenceInfo();

        ExternNodeCommunication * m_ecom;
        ConnectionHandler * m_connectionHandler;
        NodeHandler * m_nodeHandler;

        Safir::Dob::SecondaryConnection m_connection;
        
        std::set<Safir::Dob::Typesystem::Int32> m_waitingForResponsesFromNodes;

        TimerId m_timerId;

        bool m_persistDataReady;
    };
}
}
}

#endif
