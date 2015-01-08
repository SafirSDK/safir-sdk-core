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

#include "dose_main_process_info_handler.h"
#include "dose_main_communication.h"

#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/DistributionChannelProperty.h>
#include <Safir/Dob/ErrorListResponse.h>
#include <Safir/Dob/ResponseErrorInfo.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
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
    ProcessInfoHandler::ProcessInfoHandler():
        m_processMonitor(NULL)
    {

    }

    ProcessInfoHandler::~ProcessInfoHandler()
    {
    }

    void ProcessInfoHandler::Init(
#if 0 //stewart
                                  const ExternNodeCommunication & ecom,
#endif
                                  Safir::Utilities::ProcessMonitor& processMonitor)
    {
        m_processMonitor = &processMonitor;
        m_connection.Attach();

#if 0 //stewart
        if (!ecom.IsLocal(Dob::ProcessInfo::ClassTypeId))
        {
            throw Dob::Typesystem::ConfigurationErrorException
                (L"Entity ProcessInfo must have DistributionChannelProperty (or Override) set to Local",__WFILE__,__LINE__);
        }
#endif

        // Register ProcessInfo class
        m_connection.RegisterEntityHandler(Dob::ProcessInfo::ClassTypeId,
                                           Typesystem::HandlerId(),
                                           Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                           this);

        AddOwnConnection();
    }



    ConnectResult
    ProcessInfoHandler::CanAddConnectionFromProcess(const pid_t pid) const
    {
        const Typesystem::EntityId eid(ProcessInfo::ClassTypeId,Typesystem::InstanceId(pid));

        if (m_connection.IsCreated(eid))
        {
            ProcessInfoPtr processInfo = boost::static_pointer_cast<ProcessInfo>
                (m_connection.Read(eid).GetEntity());

            bool slotFound = false;
            for (int i = 0; i< ProcessInfo::ConnectionNamesArraySize(); ++i)
            {
                if (processInfo->ConnectionNames()[i].IsNull())
                {
                    slotFound = true;
                    break;
                }
            }
            if (!slotFound)
            {
                return TooManyConnectionsInProcess;
            }
        }
        else
        {
            const size_t numProcessInfo = std::distance(m_connection.GetEntityIterator(ProcessInfo::ClassTypeId,false),
                                                        EntityIterator());
            if (numProcessInfo > static_cast<size_t>(ProcessInfo::MaxNumberOfInstances()))
            {
                return TooManyProcesses;
            }
        }
        return Success;
    }

    void ProcessInfoHandler::ConnectionAdded(const ConnectionPtr & connection)
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
                processInfo->ConnectionNames()[0].SetVal(Typesystem::Utilities::ToWstring(connection->NameWithCounter()));

                m_connection.SetAll(processInfo, eid.GetInstanceId(), Typesystem::HandlerId());
            }
            else
            {
                //we have an instance for the pid, add the connection to it.
                ProcessInfoPtr processInfo = boost::static_pointer_cast<ProcessInfo>
                    (m_connection.Read(eid).GetEntity());

                bool connectionAdded = false;
                for (int i = 0; i< ProcessInfo::ConnectionNamesArraySize(); ++i)
                {
                    if (processInfo->ConnectionNames()[i].IsNull())
                    {
                        processInfo->ConnectionNames()[i].SetVal(Typesystem::Utilities::ToWstring(connection->NameWithCounter()));
                        connectionAdded = true;
                        break;
                    }
                }
                ENSURE(connectionAdded, << "Unable to add connection to ProcessInfo object. PID: " << connection->Pid()
                       << " has reached max number of connections");

                m_connection.SetAll(processInfo, eid.GetInstanceId(), Typesystem::HandlerId());
            }
        }
        catch (const Safir::Dob::AccessDeniedException &)
        {
            SEND_SYSTEM_LOG(Error,
                            << "Unable to set ProcessInfo entity " << eid 
                            << ". Has anyone overregistered Safir::Dob::ProcessInfo?");
        }
    }


    void ProcessInfoHandler::AddOwnConnection()
    {
        const Typesystem::EntityId eid(ProcessInfo::ClassTypeId,Typesystem::InstanceId(Safir::Utilities::ProcessInfo::GetPid()));
        try
        {
            ProcessInfoPtr processInfo = ProcessInfo::Create();
            ENSURE(!m_connection.IsCreated(eid),
                   << "Failed to create ProcessInfo object for dose_main! entityId = " << eid);

            processInfo->Name().SetVal(L"dose_main");
            processInfo->Pid().SetVal(Safir::Utilities::ProcessInfo::GetPid());
            processInfo->ConnectionNames()[0].SetVal(ConnectionAspectMisc(m_connection).GetConnectionName());

            m_connection.SetAll(processInfo,eid.GetInstanceId(),Typesystem::HandlerId());
        }
        catch (const Safir::Dob::AccessDeniedException &)
        {
            SEND_SYSTEM_LOG(Error,
                            << "Unable to set ProcessInfo entity " << eid
                            << "Has anyone overregistered Safir::Dob::ProcessInfo?");
        }
    }

    void ProcessInfoHandler::ConnectionRemoved(const ConnectionPtr & connection)
    {
        const Typesystem::EntityId eid(ProcessInfo::ClassTypeId,Typesystem::InstanceId(connection->Pid()));
        try
        {
            bool processHasConnection = false;
            bool processInfoUpdated = false;

            ProcessInfoPtr processInfo =
                boost::static_pointer_cast<ProcessInfo>(m_connection.Read(eid).GetEntity());

            for (int i = 0; i< ProcessInfo::ConnectionNamesArraySize(); ++i)
            {
                if (!processInfo->ConnectionNames()[i].IsNull())
                {
                    // Found a connection. Is it the one we are looking for ...?
                    if (connection->NameWithCounter() == Typesystem::Utilities::ToUtf8
                        (processInfo->ConnectionNames()[i].GetVal()))
                    {
                        // Yes, it was. Set it to NULL.
                        processInfo->ConnectionNames()[i].SetNull();
                        processInfoUpdated = true;
                    }
                    else
                    {
                        processHasConnection = true;
                        // No, it wasn't. Indicate that this process has a connection.
                    }
                }
            }

            ENSURE(processInfoUpdated, << "No process info was changed in call to ConnectionRemoved with connection " << connection->NameWithCounter());

            if (!processHasConnection)
            {
                m_connection.Delete(eid,Typesystem::HandlerId());
                m_processMonitor->StopMonitorPid(connection->Pid());
            }
            else
            {
                m_connection.SetAll(processInfo,eid.GetInstanceId(),Typesystem::HandlerId());
            }
        }
        catch (const Safir::Dob::AccessDeniedException &)
        {

        }
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

            for (int i = 0; i < ProcessInfo::ConnectionNamesArraySize(); ++i)
            {
                if (!procInfo->ConnectionNames()[i].IsNull())
                {
                    const ConnectionId id(Safir::Dob::ThisNodeParameters::NodeNumber(),
                                          -1,  // dummy context (context is part of the connection name)
                                          (Connection::CalculateIdentifier
                                           (Typesystem::Utilities::ToUtf8(procInfo->ConnectionNames()[i].GetVal()))));
                    ConnectionPtr connection = Connections::Instance().GetConnection(id);

                    connection->SendStopOrder();
                }
            }
        }

        responseSender->Send(Safir::Dob::SuccessResponse::Create());
    }

}
}
}
