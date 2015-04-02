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
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/ProcessInfo.h>
#include <Safir/Utilities/ProcessMonitor.h>
#include <vector>

namespace Safir
{
namespace Dob
{
namespace Internal
{
#if 0 //stewart
    class ExternNodeCommunication;
#endif

    class ProcessInfoHandler:
        public Safir::Dob::EntityHandler,
        private boost::noncopyable
    {
    public:
        // Constructor and Destructor
        ProcessInfoHandler();
        ~ProcessInfoHandler();

        void Init(
#if 0 //stewart
                  const ExternNodeCommunication & ecom,
#endif
                  Safir::Utilities::ProcessMonitor& processMonitor);

        void ConnectionAdded(const ConnectionPtr & connection);
        void ConnectionRemoved(const ConnectionPtr & connection);

        void HandleProcessInfoEntityDelete(const DistributionData & request);

        //returns Success if it is possible to add a new connection to the given process,
        //otherwise an error code.
        ConnectResult CanAddConnectionFromProcess(const pid_t pid) const;

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

        Safir::Dob::SecondaryConnection m_connection;

        Safir::Utilities::ProcessMonitor * m_processMonitor;
    };
}
}
}

