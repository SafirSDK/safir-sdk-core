/******************************************************************************
*
* Copyright Saab AB, 2016 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/ToolSupport/Serialization.h>
#include <Safir/Websocket/Parameters.h>
#include "RemoteClient.h"
#include "CommandValidator.h"
#include "Typesystem.h"
#include "Methods.h"
#include "JsonRpcRequest.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable:4355)
#endif

RemoteClient::RemoteClient(WsServer& server,
                           boost::asio::io_service& ioService,
                           websocketpp::connection_hdl& connectionHandle,
                           std::function<void(const RemoteClient*)> onClose)
    :m_server(server)
    ,m_strand(ioService)
    ,m_connectionHandle(connectionHandle)
    ,m_connection(m_server.get_con_from_hdl(connectionHandle))
    ,m_onConnectionClosed(onClose)
    ,m_dob(m_strand, [this](const std::string& msg){SendToClient(msg);})
    ,m_pingHandler(std::make_shared<PingHandler>(m_strand, static_cast<int>(Safir::Websocket::Parameters::PingInterval()), [this]{m_connection->ping("");}))
    ,m_enableTypeSystem(Safir::Websocket::Parameters::EnableTypesystemCommands())
{
    m_connection->set_close_handler([this](websocketpp::connection_hdl)
    {
        m_strand.post([this]{OnClose();});
    });
    m_connection->set_fail_handler([this](websocketpp::connection_hdl)
    {
        m_strand.post([this]{OnError();});
    });
    m_connection->set_message_handler([this](websocketpp::connection_hdl, WsMessage msg)
    {
        m_strand.post([this,msg]{OnMessage(msg);});
    });

    m_pingHandler->Start();
}


#ifdef _MSC_VER
#pragma warning(pop)
#endif

void RemoteClient::Close()
{
    //called from websocket server, usually means we have got a stop order from the DOB
    m_strand.post([this]
    {
        m_pingHandler->Stop();
        m_dob.Close();
        m_server.close(m_connectionHandle, websocketpp::close::status::normal, "onStopOrder");
        //callback OnClose will be triggered an then cause this instance to be deleted.
    });
}

std::string RemoteClient::ToString() const
{
    return m_connection->get_remote_endpoint();
}

//------------------------------------------------------
// Websocket events
//------------------------------------------------------
void RemoteClient::OnClose()
{
    //client closed connection
    lllog(5)<<"WS: RemoteClient.OnClose"<<std::endl;
    m_pingHandler->Stop();
    m_dob.Close();

    //since m_onConnectionClosed will destruct this object, we must wrap it in a post to let all
    //already queued events execute first. For example its likely that there is a dispatch waiting
    //that will crash the entire application if it gets executed after this instance is destroyed.
    m_strand.post([this]{m_onConnectionClosed(this);});
}

void RemoteClient::OnError()
{
    //client::connection_ptr con = m_client.get_con_from_hdl(hdl);
    std::ostringstream os;
    os<<"RemoteClient.OnError "<<ToString()<<std::endl;
    os<<"  State: "<<m_connection->get_state()<<std::endl;
    os<<"  LocalCloseCode: "<<m_connection->get_local_close_code()<<std::endl;
    os<<"  LocalCloseReason: "<<m_connection->get_local_close_reason()<<std::endl;
    os<<"  RemoteCloseCode: "<<m_connection->get_remote_close_code()<<std::endl;
    os<<"  RemoteCloseReason: "<<m_connection->get_remote_close_reason()<<std::endl;
    os<<"  Error: "<<m_connection->get_ec() << " - " <<m_connection->get_ec().message()<<std::endl;
    auto errorMsg=os.str();
    SEND_SYSTEM_LOG(Error, <<errorMsg.c_str()<<std::endl);
    lllog(5)<<errorMsg.c_str()<<std::endl;
}

void RemoteClient::OnMessage(WsMessage msg)
{
    try
    {
        auto payload=msg->get_payload();
        lllog(5)<<"WS: RemoteClient.OnMessage "<<payload.c_str()<<std::endl;

        JsonRpcRequest req(payload);
        try
        {
            req.Validate();
        }
        catch (const RequestErrorException& e)
        {
            SendToClient(JsonRpcResponse::Error(req.Id(), e.Code(), e.Message(), e.Data()));
            return;
        }

        WsDispatch(req);
    }
    catch (const RequestErrorException& e)
    {
        SendToClient(JsonRpcResponse::Error(JsonRpcId(), e.Code(), e.Message(), e.Data()));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::Error(JsonRpcId(), JsonRpcErrorCodes::InternalError, "Unexpected exception", e.what()));
    }
    catch (...)
    {
        SendToClient(JsonRpcResponse::Error(JsonRpcId(), JsonRpcErrorCodes::InternalError, JsonRpcErrorCodes::CodeToString(JsonRpcErrorCodes::InternalError), "Unexpected error in safir_websocket"));
    }
}

//------------------------------------------------------
// Websocket client commands
//------------------------------------------------------
void RemoteClient::WsDispatch(const JsonRpcRequest& req)
{
    try
    {
        if (req.IsResponse())
        {
            //if the received data is a JsonRpc-response, is must
            //be a response to a Safir-request (Service, or Entity CRUD)
           WsResponse(req);
        }
        else if (req.Method()==Methods::SetEntity)
        {
            WsSetEntity(req);
        }
        else if (req.Method()==Methods::SetEntityChanges)
        {
            WsSetEntityChanges(req);
        }
        else if (req.Method()==Methods::CreateRequest)
        {
            WsCreateRequest(req);
        }
        else if (req.Method()==Methods::UpdateRequest)
        {
            WsUpdateRequest(req);
        }
        else if (req.Method()==Methods::DeleteRequest)
        {
            WsDeleteRequest(req);
        }
        else if (req.Method()==Methods::ServiceRequest)
        {
            WsServiceRequest(req);
        }
        else if (req.Method()==Methods::SendMessage)
        {
            WsSendMessage(req);
        }
        else if (req.Method()==Methods::ReadEntity)
        {
            WsReadEntity(req);
        }
        else if (req.Method()==Methods::DeleteEntity)
        {
            WsDeleteEntity(req);
        }
        else if (req.Method()==Methods::DeleteAllInstances)
        {
            WsDeleteAllInstances(req);
        }
        else if (req.Method()==Methods::SubscribeMessage)
        {
            WsSubscribeMessage(req);
        }
        else if (req.Method()==Methods::UnsubscribeMessage)
        {
            WsUnsubscribeMessage(req);
        }
        else if (req.Method()==Methods::SubscribeEntity)
        {
            WsSubscribeEntity(req);
        }
        else if (req.Method()==Methods::RegisterEntityHandler)
        {
            WsRegisterEntityHandler(req);
        }
        else if (req.Method()==Methods::RegisterServiceHandler)
        {
            WsRegisterServiceHandler(req);
        }
        else if (req.Method()==Methods::UnregisterHandler)
        {
            WsUnregisterHandler(req);
        }
        else if (req.Method()==Methods::SubscribeRegistration)
        {
            WsSubscribeRegistration(req);
        }
        else if (req.Method()==Methods::UnsubscribeRegistration)
        {
            WsUnsubscribeRegistration(req);
        }
        else if (req.Method()==Methods::IsCreated)
        {
            WsIsCreated(req);
        }
        else if (req.Method()==Methods::GetNumberOfInstances)
        {
            WsGetNumberOfInstances(req);
        }
        else if (req.Method()==Methods::GetAllInstanceIds)
        {
            WsGetAllInstanceIds(req);
        }
        else if (req.Method()==Methods::Ping)
        {
            WsPing(req);
        }
        else if (req.Method()==Methods::Open)
        {
            WsOpen(req);
        }
        else if (req.Method()==Methods::Close)
        {
            WsClose(req);
        }
        else if (req.Method()==Methods::IsOpen)
        {
            WsIsOpen(req);
        }
        else if (req.Method()==Methods::GetInstanceIdPolicy)
        {
            WsGetInstanceIdPolicy(req);
        }
        else if (req.Method()==Methods::GetTypeHierarchy && m_enableTypeSystem)
        {
            WsGetTypeHierarchy(req);
        }
        else
        {
            throw RequestErrorException(JsonRpcErrorCodes::MethodNotFound, "Command is not supported. "+req.Method());
        }
    }
    catch (const RequestErrorException& e)
    {
        SendToClient(JsonRpcResponse::Error(req.Id(), e.Code(), e.Message(), e.Data()));
    }
    catch (const Safir::Dob::NotOpenException& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::SafirNotOpen, e.what());
        SendToClient(JsonRpcResponse::Error(req.Id(), err.Code(), err.Message(), err.Data()));
    }
    catch (const Safir::Dob::OverflowException& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::SafirOverflow, e.what());
        SendToClient(JsonRpcResponse::Error(req.Id(), err.Code(), err.Message(), err.Data()));
    }
    catch (const Safir::Dob::NotFoundException& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::SafirNotFound, e.what());
        SendToClient(JsonRpcResponse::Error(req.Id(), err.Code(), err.Message(), err.Data()));
    }
    catch (const Safir::Dob::AccessDeniedException& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::SafirAccessDenied, e.what());
        SendToClient(JsonRpcResponse::Error(req.Id(), err.Code(), err.Message(), err.Data()));
    }
    catch (const Safir::Dob::GhostExistsException& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::SafirGhostExists, e.what());
        SendToClient(JsonRpcResponse::Error(req.Id(), err.Code(), err.Message(), err.Data()));
    }
    catch (const Safir::Dob::Typesystem::Exception& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::SafirUnexpectedException, e.what());
        auto error=JsonRpcResponse::Error(req.Id(), err.Code(), err.Message(), err.Data());
        SendToClient(error);
        SEND_SYSTEM_LOG(Error, <<"WS: Got unexpected Safir exception: "<<error.c_str()<<std::endl);
        lllog(5)<<"WS: Got unexpected Safir exception: "<<error.c_str()<<std::endl;
    }
    catch (const std::exception& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::ServerError, e.what());
        SendToClient(JsonRpcResponse::Error(req.Id(), err.Code(), err.Message(), err.Data()));
        SEND_SYSTEM_LOG(Error, <<"WS: Unexpected exception: "<<e.what()<<std::endl);
        lllog(5)<<"WS: Unexpected exception: "<<e.what()<<std::endl;
    }
}

void RemoteClient::WsResponse(const JsonRpcRequest &req)
{
    CommandValidator::ValidateResponse(req);
    sd::ResponsePtr response=ToObject<sd::Response>(req.Result());
    m_dob.SendResponse(response, req.Id().Int());
}

void RemoteClient::WsPing(const JsonRpcRequest &req)
{
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "pong"));
}

void RemoteClient::WsOpen(const JsonRpcRequest& req)
{
    CommandValidator::ValidateOpen(req);
    auto context=req.HasContext() ?  req.Context() : 0;
    m_dob.Open(Wstr(req.ConnectionName()), context);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsClose(const JsonRpcRequest& req)
{
    m_dob.Close();
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsIsOpen(const JsonRpcRequest &req)
{
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::Bool(req.Id(), m_dob.IsOpen()));
}

void RemoteClient::WsGetTypeHierarchy(const JsonRpcRequest& req)
{
    try
    {
        if (!req.Id().IsNull())
            SendToClient(JsonRpcResponse::Json(req.Id(), Typesystem::GetTypeHierarchy()));
    }
    catch (const std::exception& e)
    {
        SendToClient(JsonRpcResponse::Error(req.Id(), JsonRpcErrorCodes::InternalError, "Failed to construct type hierarchy", e.what()));
    }
}

void RemoteClient::WsSubscribeMessage(const JsonRpcRequest& req)
{
    CommandValidator::ValidateSubscribeMessage(req);
    auto channel=req.HasChannelId() ? req.ChannelId() : ts::ChannelId::ALL_CHANNELS;
    auto includeSub=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
    m_dob.SubscribeMessage(req.TypeId(), channel, includeSub);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsSendMessage(const JsonRpcRequest& req)
{
    CommandValidator::ValidateSendMessage(req);
    auto channel=req.HasChannelId() ? req.ChannelId() : ts::ChannelId();
    sd::MessagePtr message=ToObject<sd::Message>(req.Message());
    m_dob.SendMessage(message, channel);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsUnsubscribeMessage(const JsonRpcRequest& req)
{
    CommandValidator::ValidateUnsubscribeMessage(req);
    auto channel=req.HasChannelId() ? req.ChannelId() : ts::ChannelId::ALL_CHANNELS;
    auto includeUpdates=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
    m_dob.UnsubscribeMessage(req.TypeId(), channel, includeUpdates);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsSubscribeEntity(const JsonRpcRequest& req)
{
    CommandValidator::ValidateSubscribeEntity(req);
    auto includeUpdates=req.HasIncludeUpdates() ? req.IncludeUpdates() : true;
    auto restartSub=req.HasRestartSubscription() ? req.RestartSubscription() : true;

    if (req.HasInstanceId())
    {
        auto entityId=ts::EntityId(req.TypeId(), req.InstanceId());
        m_dob.SubscribeEntity(entityId, includeUpdates, restartSub);
    }
    else
    {
        auto includeSubclasses=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
        m_dob.SubscribeEntity(req.TypeId(), includeUpdates, includeSubclasses, restartSub);
    }

    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsUnsubscribeEntity(const JsonRpcRequest& req)
{
    CommandValidator::ValidateUnsubscribeEntity(req);

    if (req.HasInstanceId())
    {
        auto entityId=ts::EntityId(req.TypeId(), req.InstanceId());
        m_dob.UnsubscribeEntity(entityId);
    }
    else
    {
        auto includeSubclasses=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
        m_dob.UnsubscribeEntity(req.TypeId(), includeSubclasses);
    }

    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsRegisterEntityHandler(const JsonRpcRequest& req)
{
    CommandValidator::ValidateRegisterEntityHandler(req);
    auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    auto instPolicy=req.HasInstanceIdPolicy() ? req.InstanceIdPolicy() : sd::InstanceIdPolicy::RequestorDecidesInstanceId;
    auto injectionHandler=req.HasInjectionHandler() ? req.InjectionHandler() : false;
    auto pendingReg=req.HasPending() ? req.Pending() : false;
    m_dob.RegisterEntity(req.TypeId(), handler, instPolicy, injectionHandler, pendingReg);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsRegisterServiceHandler(const JsonRpcRequest &req)
{
    CommandValidator::ValidateRegisterServiceHandler(req);
    auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    auto pendingReg=req.HasPending() ? req.Pending() : false;
    m_dob.RegisterService(req.TypeId(), handler, pendingReg);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsUnregisterHandler(const JsonRpcRequest& req)
{
    CommandValidator::ValidateUnregisterHandler(req);
    auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId::ALL_HANDLERS;
    m_dob.UnregisterHandler(req.TypeId(), handler);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsSubscribeRegistration(const JsonRpcRequest &req)
{
    CommandValidator::ValidateSubscribeRegistration(req);
    auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId::ALL_HANDLERS;
    auto inclSub=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
    auto restartSub=req.HasRestartSubscription() ? req.RestartSubscription() : true;
    m_dob.SubscribeRegistration(req.TypeId(), handler, inclSub, restartSub);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsUnsubscribeRegistration(const JsonRpcRequest& req)
{
    CommandValidator::ValidateUnsubscribeRegistration(req);
    auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId::ALL_HANDLERS;
    auto inclSub=req.IncludeSubclasses() ? req.IncludeSubclasses() : true;
    m_dob.UnsubscribeRegistration(req.TypeId(), handler, inclSub);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsCreateRequest(const JsonRpcRequest& req)
{
    CommandValidator::ValidateCreateRequest(req);
    auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    sd::EntityPtr entity=ToObject<sd::Entity>(req.Entity());
    if (req.HasInstanceId())
    {
        m_dob.CreateRequest(entity, req.InstanceId(), handler, req.Id());
    }
    else
    {
        m_dob.CreateRequest(entity, handler, req.Id());
    }
}

void RemoteClient::WsUpdateRequest(const JsonRpcRequest& req)
{
    CommandValidator::ValidateUpdateRequest(req);
    sd::EntityPtr entity=ToObject<sd::Entity>(req.Entity());
    m_dob.UpdateRequest(entity, req.InstanceId(), req.Id());
}

void RemoteClient::WsDeleteRequest(const JsonRpcRequest& req)
{
    CommandValidator::ValidateDeleteRequest(req);
    m_dob.DeleteRequest(req.TypeId(), req.InstanceId(), req.Id());
}

void RemoteClient::WsServiceRequest(const JsonRpcRequest &req)
{
    CommandValidator::ValidateServiceRequest(req);
    auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    sd::ServicePtr service=ToObject<sd::Service>(req.Request());
    m_dob.ServiceRequest(service, handler, req.Id());
}

void RemoteClient::WsSetEntityChanges(const JsonRpcRequest& req)
{
    CommandValidator::ValidateSetEntityChanges(req);
    sd::EntityPtr entity=ToObject<sd::Entity>(req.Entity());
    auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    m_dob.SetChanges(entity, req.InstanceId(), handler);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsSetEntity(const JsonRpcRequest& req)
{
    CommandValidator::ValidateSetEntity(req);
    sd::EntityPtr entity=ToObject<sd::Entity>(req.Entity());
    auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    m_dob.SetAll(entity, req.InstanceId(), handler);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsDeleteEntity(const JsonRpcRequest& req)
{
    CommandValidator::ValidateDeleteEntity(req);
    auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    m_dob.Delete(req.TypeId(), req.InstanceId(), handler);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsDeleteAllInstances(const JsonRpcRequest& req)
{
    CommandValidator::ValidateDeleteAllInstances(req);
    auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    m_dob.DeleteAllInstances(req.TypeId(), handler);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsReadEntity(const JsonRpcRequest& req)
{
    CommandValidator::ValidateReadEntity(req);
    auto entity=m_dob.Read(req.TypeId(), req.InstanceId());
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::Json(req.Id(), entity));
}

void RemoteClient::WsIsCreated(const JsonRpcRequest& req)
{
    CommandValidator::ValidateIsCreated(req);
    auto isCreated=m_dob.IsCreated(req.TypeId(), req.InstanceId());
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::Bool(req.Id(), isCreated));
}

void RemoteClient::WsGetNumberOfInstances(const JsonRpcRequest& req)
{
    CommandValidator::ValidateGetNumberOfInstances(req);
    auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId::ALL_HANDLERS;
    auto inclSub=req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
    ts::Int64 num=m_dob.GetNumberOfInstances(req.TypeId(), handler, inclSub);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::Int(req.Id(), num));
}

void RemoteClient::WsGetInstanceIdPolicy(const JsonRpcRequest& req)
{
    CommandValidator::ValidateGetInstanceIdPolicy(req);
    auto handler=req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    auto policy=m_dob.GetInstanceIdPolicy(req.TypeId(), handler);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), policy));
}

void RemoteClient::WsGetAllInstanceIds(const JsonRpcRequest& req)
{
    CommandValidator::ValidateGetAllInstanceIds(req);
    auto ids=m_dob.GetAllInstanceIds(req.TypeId());
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::UnquotedArray(req.Id(), ids));
}
