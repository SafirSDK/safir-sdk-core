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

#ifndef _dose_main_node_handler_h
#define _dose_main_node_handler_h

#include <Safir/Dob/NodeStatus.h>
#include <Safir/Dob/Internal/ConnectionId.h>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Connection.h>
#include <boost/noncopyable.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class ExternNodeCommunication;
    class RequestHandler;
    class PoolHandler;

    class NodeHandler:
        public Safir::Dob::EntityHandler,
        private boost::noncopyable
    {
    public:

        NodeHandler();
        ~NodeHandler();
        void Init (ExternNodeCommunication & ecom,
                   RequestHandler & requestHandler,
                   PoolHandler& poolHandler);

        //Get and handle node status changes from dosecom.
        void HandleNodeStatusChanges();

    private:

        virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    typeId,
                                           const Safir::Dob::Typesystem::HandlerId& handlerId);

        virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                     Safir::Dob::ResponseSenderPtr    responseSender);

        virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                     Safir::Dob::ResponseSenderPtr    responseSender);

        virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                     Safir::Dob::ResponseSenderPtr    responseSender);

        void HandleDisconnect(const ConnectionPtr & connection, const NodeNumber node);
        void DeleteConnections(const NodeNumber node);

        void KickConnection(const ConnectionPtr& connection);

        ExternNodeCommunication* m_ecom;
        RequestHandler * m_requestHandler;
        PoolHandler * m_poolHandler;

        Safir::Dob::SecondaryConnection m_connection;
    };
}
}
}

#endif
