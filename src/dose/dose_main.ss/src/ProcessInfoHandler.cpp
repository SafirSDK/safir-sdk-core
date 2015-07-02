/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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

#include "ProcessInfoHandler.h"

#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/ErrorListResponse.h>
#include <Safir/Dob/ResponseErrorInfo.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Dob/ProcessInfo.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <Safir/Utilities/Internal/SystemLog.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    ProcessInfoHandler::ProcessInfoHandler(boost::asio::io_service& ioService,
                                           const Distribution& distribution)
        : m_strand(ioService)
        , m_dispatcher(m_connection, m_strand)
        , m_stopped(false)
    {
        m_processMonitor.reset(new Safir::Utilities::ProcessMonitor(ioService,
                                                                       [] (const pid_t pid)
                                                                       {
                                                                           //This marks all connections belonging to the process that died
                                                                           //as dead.
                                                                           Connections::Instance().ForEachConnectionPtr
                                                                               ([pid](const Safir::Dob::Internal::ConnectionPtr& connection)
                                                                                {
                                                                                    if (connection->Pid() == pid)
                                                                                    {
                                                                                        connection->Died();
                                                                                    }
                                                                                });
                                                                       },
                                                                       boost::chrono::seconds(1)));

        m_strand.dispatch([this,&distribution]
        {
            m_connection.Open(L"dose_main",L"ProcessInfoHandler",0,nullptr,&m_dispatcher);

            if (!distribution.IsLocal(Dob::ProcessInfo::ClassTypeId))
            {
                throw Dob::Typesystem::ConfigurationErrorException
                    (L"Entity ProcessInfo must have DistributionChannelProperty (or Override) set to Local",__WFILE__,__LINE__);
            }

            // Register ProcessInfo class
            m_connection.RegisterEntityHandler(Dob::ProcessInfo::ClassTypeId,
                                               Typesystem::HandlerId(),
                                               Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                               this);

            AddOwnConnection();
        });
    }


    void ProcessInfoHandler::Stop()
    {
        const bool was_stopped = m_stopped.exchange(true);
        if (!was_stopped)
        {
            m_processMonitor->Stop();
            m_strand.dispatch([this]
                              {
                                  m_connection.Close();
                              });
        }
    }

    void ProcessInfoHandler::ConnectionAdded(const ConnectionPtr & connection)
    {
        if (m_stopped)
        {
            return;
        }

        m_strand.dispatch([this,connection]
        {
            const Typesystem::EntityId eid(ProcessInfo::ClassTypeId,Typesystem::InstanceId(connection->Pid()));
            try
            {
                if (!m_connection.IsCreated(eid))
                {
                    m_processMonitor->StartMonitorPid(connection->Pid());

                    //pid does not exist, need a new instance
                    ProcessInfoPtr processInfo = ProcessInfo::Create();

                    Safir::Utilities::ProcessInfo pi(connection->Pid());
                    processInfo->Name().SetVal(Typesystem::Utilities::ToWstring(pi.GetProcessName()));
                    processInfo->Pid().SetVal(connection->Pid());
                    processInfo->ConnectionNames().push_back
                        (Typesystem::Utilities::ToWstring(connection->NameWithCounter()));

                    m_connection.SetAll(processInfo, eid.GetInstanceId(), Typesystem::HandlerId());
                }
                else
                {
                    //we have an instance for the pid, add the connection to it.
                    ProcessInfoPtr processInfo = boost::static_pointer_cast<ProcessInfo>
                        (m_connection.Read(eid).GetEntity());
                    processInfo->ConnectionNames().push_back
                        (Typesystem::Utilities::ToWstring(connection->NameWithCounter()));

                    m_connection.SetAll(processInfo, eid.GetInstanceId(), Typesystem::HandlerId());
                }
            }
            catch (const Safir::Dob::AccessDeniedException &)
            {
                SEND_SYSTEM_LOG(Error,
                                << "Unable to set ProcessInfo entity " << eid
                                << ". Has anyone overregistered Safir::Dob::ProcessInfo?");
            }
        });
    }


    void ProcessInfoHandler::AddOwnConnection()
    {
        const Typesystem::EntityId eid(ProcessInfo::ClassTypeId,
                                       Typesystem::InstanceId(Safir::Utilities::ProcessInfo::GetPid()));
        try
        {
            ProcessInfoPtr processInfo = ProcessInfo::Create();
            ENSURE(!m_connection.IsCreated(eid),
                   << "Failed to create ProcessInfo object for dose_main! entityId = " << eid);

            processInfo->Name().SetVal(L"dose_main");
            processInfo->Pid().SetVal(Safir::Utilities::ProcessInfo::GetPid());

            // We don't publish any of dose_mains connections.

            m_connection.SetAll(processInfo,eid.GetInstanceId(),Typesystem::HandlerId());
        }
        catch (const Safir::Dob::AccessDeniedException &)
        {
            SEND_SYSTEM_LOG(Error,
                            << "Unable to set ProcessInfo entity " << eid
                            << "Has someone overregistered Safir::Dob::ProcessInfo?");
        }
    }


    void ProcessInfoHandler::ConnectionRemoved(const ConnectionPtr & connection)
    {
        if (m_stopped)
        {
            return;
        }

        m_strand.dispatch([this,connection]
        {
            const Typesystem::EntityId eid(ProcessInfo::ClassTypeId,Typesystem::InstanceId(connection->Pid()));
            try
            {
                bool processInfoUpdated = false;

                auto processInfo = boost::static_pointer_cast<ProcessInfo>(m_connection.Read(eid).GetEntity());

                for (size_t i = 0; i < processInfo->ConnectionNames().size(); ++i)
                {
                    // Found a connection. Is it the one we are looking for ...?
                    if (connection->NameWithCounter() == Typesystem::Utilities::ToUtf8
                        (processInfo->ConnectionNames()[i]))
                    {
                        // Yes, it was. Set it to NULL.
                        processInfo->ConnectionNames().EraseAt(i);
                        processInfoUpdated = true;
                        break;
                    }
                }
                if (processInfoUpdated)
                {
                    if (processInfo->ConnectionNames().empty())
                    {
                        m_connection.Delete(eid,Typesystem::HandlerId());
                        m_processMonitor->StopMonitorPid(connection->Pid());
                    }
                    else
                    {
                        m_connection.SetAll(processInfo,eid.GetInstanceId(),Typesystem::HandlerId());
                    }
                }
                else
                {
                    SEND_SYSTEM_LOG(Warning,
                                    << "Could not remove connection '" << connection->NameWithCounter()
                                    << "' from process with pid = " << connection->Pid()
                                    << " in the Safir.Dob.ProcessInfo entity since it wasn't in there!")
                }
            }
            catch (const Safir::Dob::AccessDeniedException &)
            {
                SEND_SYSTEM_LOG(Error,
                                << "Unable to set ProcessInfo entity " << eid
                                << "Has someone overregistered Safir::Dob::ProcessInfo?");
            }
        });
    }

    void ProcessInfoHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    /*typeId*/,
                                                   const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {
        SEND_SYSTEM_LOG(Critical,
                        << "Someone overregistered Safir::Dob::ProcessInfo, so I'm not "
                        << "going to be able to update this entity any longer!");
    }

    void ProcessInfoHandler::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                             Safir::Dob::ResponseSenderPtr    responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
            (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
             L"It is not possible to send create requests on Safir::Dob::ProcessInfo"));
    }

    void ProcessInfoHandler::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                             Safir::Dob::ResponseSenderPtr    responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
            (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
             L"It is not possible to send update requests on Safir::Dob::ProcessInfo"));
    }

    void ProcessInfoHandler::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                             Safir::Dob::ResponseSenderPtr    responseSender)
    {
        if (m_connection.IsCreated(entityRequestProxy.GetEntityId()))
        {
            ProcessInfoPtr procInfo = boost::static_pointer_cast<ProcessInfo>
                (m_connection.Read(entityRequestProxy.GetEntityId()).GetEntity());

            for (auto name = procInfo->ConnectionNames().begin(); name != procInfo->ConnectionNames().end(); ++name)
            {
                const ConnectionId id(Connections::Instance().NodeId(),
                    -1,  // dummy context (context is part of the connection name)
                    (Connection::CalculateIdentifier
                    (Typesystem::Utilities::ToUtf8(*name))));
                ConnectionPtr connection = Connections::Instance().GetConnection(id);

                connection->SendStopOrder();
            }
        }

        responseSender->Send(Safir::Dob::SuccessResponse::Create());
    }

}
}
}
