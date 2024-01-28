/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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

#include <Safir/Dob/SecondaryConnection.h>
#include <Safir/Control/GetConnectionStatistics.h>
#include <Safir/Control/GetConnectionStatisticsAllNodes.h>
#include <Safir/Control/ConnectionStatisticsAllNodesResponse.h>

#include <boost/asio.hpp>
#include <boost/noncopyable.hpp>


namespace Safir
{
namespace Control
{
    class NodeInfoMirrorer :
        public Safir::Dob::EntitySubscriber,
        public Safir::Dob::EntityHandlerPending,
        private boost::noncopyable
    {
    public:
        NodeInfoMirrorer(boost::asio::io_context& io);

        void Start();
        void Stop();

        void OnNewEntity(const Safir::Dob::EntityProxy entityProxy) override;

        void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy) override;

        void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy,
                             const bool) override;

        void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                   const Safir::Dob::Typesystem::HandlerId& handlerId) override;

        void OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                     const Safir::Dob::Typesystem::HandlerId& handlerId) override;

        void OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy) override;

        void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId,
                                     const Safir::Dob::Typesystem::HandlerId&     handlerId) override;

        void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                             Safir::Dob::ResponseSenderPtr        responseSender) override;

        void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                             Safir::Dob::ResponseSenderPtr        responseSender) override;

        void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                             Safir::Dob::ResponseSenderPtr        responseSender) override;

    private:
        void OnNewOrUpdatedEntity(const Safir::Dob::EntityProxy& entityProxy);
        
        boost::asio::io_context& m_io;
        Safir::Dob::SecondaryConnection m_connection;
    };
}
}
