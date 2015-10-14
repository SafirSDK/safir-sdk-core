/******************************************************************************
*
* Copyright Saab AB, 2007-2013,2015 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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

#include <Safir/Control/Command.h>
#include <Safir/Dob/SecondaryConnection.h>
#include <Safir/Dob/Internal/ControlCmd.h>
#include <boost/noncopyable.hpp>
#include <boost/asio.hpp>

namespace Safir
{
namespace Control
{
    /**
     * This class handles the Safir.Control.Command service.
     *
     * When a request is received the command is passed on via IPC to Safir_control
     */
    class CommandRequestHandler:
        public Safir::Dob::ServiceHandler,
        private boost::noncopyable
    {
    public:
        CommandRequestHandler(boost::asio::io_service& ioService);

        void Start();
        void Stop();

    private:
        void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    typeId,
                                   const Safir::Dob::Typesystem::HandlerId& handlerId) override;

        void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                       Safir::Dob::ResponseSenderPtr       responseSender) override;

        Safir::Dob::Internal::Control::CommandAction GetCommandActionFromOperation(
                Safir::Control::Operation::Enumeration operation);

        bool m_connectedToIPC;
        Safir::Dob::SecondaryConnection m_dobConnection;
        std::unique_ptr<Safir::Dob::Internal::Control::ControlCmdSender> m_controlCommandSender;
    };
}
}
