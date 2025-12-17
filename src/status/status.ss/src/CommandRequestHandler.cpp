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
#include "CommandRequestHandler.h"
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/LowMemoryException.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/Typesystem/Convenience.h>
#include <Safir/Logging/Log.h>
#include <Safir/Utilities/Internal/SystemLog.h>

namespace Safir
{
namespace Control
{
    CommandRequestHandler::CommandRequestHandler(boost::asio::io_context& ioContext)
    {
        m_controlCommandSender.reset(new Safir::Dob::Internal::Control::ControlCmdSender(ioContext));
    }

    void CommandRequestHandler::Start()
    {
        m_dobConnection.Attach();

        m_dobConnection.RegisterServiceHandler(Command::ClassTypeId,
                                               Safir::Dob::Typesystem::HandlerId(),
                                               this);
    }

    void CommandRequestHandler::Stop()
    {

    }

    void CommandRequestHandler::OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                                 Safir::Dob::ResponseSenderPtr responseSender)
    {
        Safir::Dob::ResponsePtr response;

        Safir::Control::CommandPtr command
                = std::dynamic_pointer_cast<Safir::Control::Command>(serviceRequestProxy.GetRequest());

        const auto completionHandler = [responseSender](const std::error_code& err)
        {
            Safir::Dob::ResponsePtr response;
            if (err)
            {
                if (err == std::make_error_code(std::errc::timed_out))
                {
                    response = Safir::Dob::ErrorResponse::CreateErrorResponse(
                        Safir::Dob::ResponseGeneralErrorCodes::SafirTimeout(),
                        L"Underlying IPC request timed out, try again.");
                }
                else
                {
                    response = Safir::Dob::ErrorResponse::CreateErrorResponse(
                        Safir::Dob::ResponseGeneralErrorCodes::SafirInternalErr(),
                        L"Failed to send IPC request to safir_control. Internal error: " + Wstr(err.message()));
                }
            }
            else
            {
                response = Safir::Dob::SuccessResponse::Create();
            }

            try
            {
                if (response != nullptr)
                {
                    responseSender->Send(response);
                }
            }
            catch (const Safir::Dob::LowMemoryException&)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"Failed to send response due to low shared memory. Skipping.");
                responseSender->Discard();
            }
        };

        if (command->Operation().IsNull())
        {
            response = Safir::Dob::ErrorResponse::CreateErrorResponse(
                        Safir::Dob::ResponseGeneralErrorCodes::SafirMissingMember(),
                        L"Operation cannot be NULL");
        }
        else if (command->NodeId().IsNull())
        {
            //NodeId is null, send "system wide" command
            m_controlCommandSender->SendCmd(GetCommandActionFromOperation(command->Operation().GetVal()),
                                            0,
                                            std::chrono::seconds(5),
                                            completionHandler);
        }
        else
        {
            //Send node command
            m_controlCommandSender->SendCmd(GetCommandActionFromOperation(command->Operation().GetVal()),
                                            command->NodeId().GetVal(),
                                            std::chrono::seconds(5),
                                            completionHandler);
        }

        try
        {
            if (response != nullptr)
            {
                responseSender->Send(response);
            }
        }
        catch (const Safir::Dob::LowMemoryException&)
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Failed to send response due to low shared memory. Skipping.");
            responseSender->Discard();
        }
    }

    void CommandRequestHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId /*typeId*/,
                                                const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {
        SEND_SYSTEM_LOG(Error,
                        << "Someone overregistered Safir::Control::Command, so I'm not "
                        << "going to be able to handle this service any longer!");
    }

    Safir::Dob::Internal::Control::CommandAction
    CommandRequestHandler::GetCommandActionFromOperation(Safir::Control::Operation::Enumeration operation)
    {
        switch (operation) {
        case Safir::Control::Operation::Reboot:
        {
            return Safir::Dob::Internal::Control::REBOOT;
            break;
        }
        case Safir::Control::Operation::Shutdown:
        {
            return Safir::Dob::Internal::Control::SHUTDOWN;
            break;
        }
        case Safir::Control::Operation::Stop:
        {
            return Safir::Dob::Internal::Control::STOP;
            break;
        }
        default:
            break;
        }

        return Safir::Dob::Internal::Control::STOP;
    }
}
}
