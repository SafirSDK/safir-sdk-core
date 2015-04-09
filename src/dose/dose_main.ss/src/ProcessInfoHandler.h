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

#include <Safir/Dob/Connection.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Utilities/ProcessMonitor.h>
#include <atomic>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    /**
     * This class handles the ProcessInfo entity, creating and deleting instances
     * when connections come and go.
     * It also instantiates the ProcessMonitor class and uses it to detect when
     * a process dies to be able to mark its connections as dead.
     *
     * The only request that is allowed on the entity is DeleteRequest, which
     * will cause a stop order to be sent to the application.
     *
     * All methods in this class are thread safe.
     */
    class ProcessInfoHandler:
        public Safir::Dob::EntityHandler,
        private boost::noncopyable
    {
    public:
        // Constructor and Destructor
        explicit ProcessInfoHandler(boost::asio::io_service& ioService);

        void Stop();

        void ConnectionAdded(const ConnectionPtr & connection);
        void ConnectionRemoved(const ConnectionPtr & connection);

    private:
        void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId,
                                   const Safir::Dob::Typesystem::HandlerId& handlerId) override;

        void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                             Safir::Dob::ResponseSenderPtr responseSender) override;

        void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                             Safir::Dob::ResponseSenderPtr responseSender) override;

        void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                             Safir::Dob::ResponseSenderPtr responseSender) override;

        void AddOwnConnection();

        boost::asio::io_service::strand m_strand;
        Safir::Dob::Connection m_connection;
        Utilities::AsioDispatcher m_dispatcher;
        std::atomic<bool> m_stopped{false};

        Safir::Utilities::ProcessMonitor m_processMonitor;
    };
}
}
}

