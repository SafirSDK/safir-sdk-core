/******************************************************************************
*
* Copyright Saab AB, 2026 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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
#include <sstream>
#include <boost/beast/core/buffers_to_string.hpp>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/LowMemoryException.h>
#include <Safir/Dob/Typesystem/Convenience.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Web/Parameters.h>
#include "RemoteClient.h"
#include "DobConnectionRegistry.h"
#include "CommandValidator.h"
#include "Typesystem.h"
#include "Methods.h"

namespace http = boost::beast::http;

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable:4355)
#endif

RemoteClient::RemoteClient(boost::asio::io_context& io,
                           TcpSocket socket,
                           std::shared_ptr<DobConnectionRegistry> dobConnectionRegistry,
                           std::function<void(const RemoteClient*)> onClose)
    : m_stream(std::move(socket))
    , m_strand(std::make_shared<boost::asio::io_context::strand>(io))
    , m_dobConnectionRegistry(dobConnectionRegistry)
    , m_onConnectionClosed(onClose)
    , m_dobConnection(nullptr)
    , m_pingHandler(std::make_shared<PingHandler>(*m_strand,
                                                  static_cast<int>(Safir::Web::Parameters::PingInterval()),
                                                  [this]{SendPing();}))
    , m_connectionName()
    , m_enableTypeSystem(Safir::Web::Parameters::EnableTypesystemCommands())
    , m_isWriting(false)
    , m_isClosed(false)
    , m_isOpened(false)
{
}

void RemoteClient::Start(boost::beast::http::request<boost::beast::http::string_body> handshakeRequest,
                         std::function<void(bool)> onStarted)
{
    auto self = shared_from_this();
    boost::asio::post(*m_strand, [this, self, onStarted, req = std::move(handshakeRequest)]() mutable
    {
        m_stream.set_option(boost::beast::websocket::stream_base::timeout::suggested(boost::beast::role_type::server));
        m_stream.async_accept(req,
                              boost::asio::bind_executor(*m_strand, [this, self, onStarted](const boost::system::error_code& acceptEc)
        {
            if (acceptEc)
            {
                LogError("RemoteClient.Accept", acceptEc);
                if (onStarted)
                {
                    onStarted(false);
                }
                return;
            }

            m_dobConnection = std::make_shared<DobConnection>(*m_strand, [this](const std::string& msg){SendToClient(msg);});
            m_isOpened = true;
            // Limit read buffer to 64 MB to guard against oversized payloads
            m_readBuffer.max_size(64 * 1024 * 1024);
            m_pingHandler->Start();
            if (onStarted)
            {
                onStarted(true);
            }
            DoRead();
        }));
    });
}

#ifdef _MSC_VER
#pragma warning(pop)
#endif

void RemoteClient::Close()
{
    auto self = shared_from_this();
    boost::asio::post(*m_strand, [this, self]
    {
        if (m_isClosed)
        {
            return;
        }
        m_isClosed = true;
        m_pingHandler->Stop();
        m_dobConnection->Close();

        if (m_isOpened)
        {
            boost::beast::websocket::close_reason reason;
            reason.code = boost::beast::websocket::close_code::normal;
            reason.reason = "onStopOrder";
            m_stream.async_close(reason,
                                 boost::asio::bind_executor(*m_strand, [this, self](const boost::system::error_code& ec)
            {
                if (ec && ec != boost::beast::websocket::error::closed)
                {
                    LogError("RemoteClient.Close", ec);
                }
                NotifyClosed();
            }));
        }
        else
        {
            NotifyClosed();
        }
    });
}

std::string RemoteClient::ToString() const
{
    boost::system::error_code ec;
    auto endpoint = m_stream.next_layer().remote_endpoint(ec);
    if (ec)
    {
        return "<unknown endpoint>";
    }

    std::ostringstream os;
    os << endpoint.address().to_string() << ":" << endpoint.port();
    return os.str();
}

void RemoteClient::SendToClient(const std::string& msg)
{
    // All callers are already on the strand — no post() needed
    if (m_isClosed)
        return;

    /*if (m_writeQueue.size() >= MaxWriteQueueSize)
    {
        SEND_SYSTEM_LOG(Error, << "WS: RemoteClient write queue full (" << MaxWriteQueueSize << "), closing connection to " << ToString().c_str() << std::endl);
        lllog(5) << "WS: RemoteClient write queue full, closing connection to " << ToString().c_str() << std::endl;
        CloseInternal();
        return;
    }*/

    m_writeQueue.push_back(msg);
    if (!m_isWriting)
    {
        DoWrite();
    }
}

void RemoteClient::SendPing()
{
    // Called from PingHandler which already runs on the strand
    if (m_isClosed || !m_isOpened)
    {
        return;
    }

    auto self = shared_from_this();
    m_stream.async_ping({}, boost::asio::bind_executor(*m_strand, [this, self](const boost::system::error_code& ec)
    {
        if (ec)
        {
            LogError("RemoteClient.Ping", ec);
            CloseInternal();
        }
    }));
}

void RemoteClient::DoRead()
{
    auto self = shared_from_this();
    m_stream.async_read(m_readBuffer, boost::asio::bind_executor(*m_strand, [this, self](const boost::system::error_code &ec, std::size_t /*bytesTransferred*/)
                                                                 {
    if (ec)
    {
        if (ec != boost::beast::websocket::error::closed)
        {
            LogError("RemoteClient.Read", ec);
        }
        CloseInternal();
        return;
    }

    auto payload = boost::beast::buffers_to_string(m_readBuffer.data());
    m_readBuffer.consume(m_readBuffer.size());

    bool valid = true;
    try
    {
        lllog(5)<<"WS: RemoteClient.OnMessage "<<payload.c_str()<<std::endl;

        JsonRpcRequest req(payload);
        valid = true;
        try
        {
            req.Validate();
        }
        catch (const RequestErrorException& e)
        {
            SendToClient(JsonRpcResponse::Error(req.Id(), e.Code(), e.Message(), e.Data()));
            valid = false;
        }

        if (valid)
        {
            WsDispatch(req);
        }
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
        SendToClient(JsonRpcResponse::Error(JsonRpcId(), JsonRpcErrorCodes::InternalError, JsonRpcErrorCodes::CodeToString(JsonRpcErrorCodes::InternalError), "Unexpected error in safir_web"));
    }

    DoRead(); }));
}

void RemoteClient::DoWrite()
{
    if (m_writeQueue.empty() || m_isClosed)
    {
        m_isWriting = false;
        return;
    }

    m_isWriting = true;
    auto self = shared_from_this();
    
    m_stream.async_write(boost::asio::buffer(m_writeQueue.front()),
                         boost::asio::bind_executor(*m_strand, [this, self](const boost::system::error_code& ec, std::size_t /*bytesTransferred*/)
    {
        if (ec)
        {
            LogError("RemoteClient.Write", ec);
            CloseInternal();
            return;
        }

        m_writeQueue.pop_front();
        m_pingHandler->Update();
        DoWrite();
    }));
}

void RemoteClient::NotifyClosed()
{
    if (!m_isOpened)
    {
        return;
    }

    lllog(5) << "WS: RemoteClient.OnClose" << std::endl;
    m_isOpened = false;
    RemoveDobConnectionFromRegistry();

    auto self = shared_from_this();
    boost::asio::post(*m_strand, [this, self]{m_onConnectionClosed(this);});
}

void RemoteClient::LogError(const char* context, const boost::system::error_code& ec)
{
    std::ostringstream os;
    os << context << " " << ToString() << std::endl;
    os << "  Error: " << ec << " - " << ec.message() << std::endl;
    auto errorMsg = os.str();
    SEND_SYSTEM_LOG(Error, << errorMsg.c_str() << std::endl);
    lllog(5) << errorMsg.c_str() << std::endl;
}

void RemoteClient::CloseInternal()
{
    if (m_isClosed)
    {
        return;
    }
    m_isClosed = true;
    m_pingHandler->Stop();
    m_dobConnection->Close();
    NotifyClosed();
}

void RemoteClient::RemoveDobConnectionFromRegistry()
{
    if (!m_connectionName.empty() && m_dobConnectionRegistry)
    {
        m_dobConnectionRegistry->RemoveConnection(m_connectionName);
        m_connectionName.clear();
    }
}

void RemoteClient::WsDispatch(const JsonRpcRequest& req)
{
    try
    {
        if (req.IsResponse())
        {
            WsResponse(req);
        }
        else if (req.Method() == Methods::SetEntity)
        {
            WsSetEntity(req);
        }
        else if (req.Method() == Methods::SetEntityChanges)
        {
            WsSetEntityChanges(req);
        }
        else if (req.Method() == Methods::CreateRequest)
        {
            WsCreateRequest(req);
        }
        else if (req.Method() == Methods::UpdateRequest)
        {
            WsUpdateRequest(req);
        }
        else if (req.Method() == Methods::DeleteRequest)
        {
            WsDeleteRequest(req);
        }
        else if (req.Method() == Methods::ServiceRequest)
        {
            WsServiceRequest(req);
        }
        else if (req.Method() == Methods::SendMessage)
        {
            WsSendMessage(req);
        }
        else if (req.Method() == Methods::ReadEntity)
        {
            WsReadEntity(req);
        }
        else if (req.Method() == Methods::DeleteEntity)
        {
            WsDeleteEntity(req);
        }
        else if (req.Method() == Methods::DeleteAllInstances)
        {
            WsDeleteAllInstances(req);
        }
        else if (req.Method() == Methods::SubscribeMessage)
        {
            WsSubscribeMessage(req);
        }
        else if (req.Method() == Methods::UnsubscribeMessage)
        {
            WsUnsubscribeMessage(req);
        }
        else if (req.Method() == Methods::SubscribeEntity)
        {
            WsSubscribeEntity(req);
        }
        else if (req.Method() == Methods::UnsubscribeEntity)
        {
            WsUnsubscribeEntity(req);
        }
        else if (req.Method() == Methods::RegisterEntityHandler)
        {
            WsRegisterEntityHandler(req);
        }
        else if (req.Method() == Methods::RegisterServiceHandler)
        {
            WsRegisterServiceHandler(req);
        }
        else if (req.Method() == Methods::UnregisterHandler)
        {
            WsUnregisterHandler(req);
        }
        else if (req.Method() == Methods::SubscribeRegistration)
        {
            WsSubscribeRegistration(req);
        }
        else if (req.Method() == Methods::UnsubscribeRegistration)
        {
            WsUnsubscribeRegistration(req);
        }
        else if (req.Method() == Methods::IsCreated)
        {
            WsIsCreated(req);
        }
        else if (req.Method() == Methods::GetNumberOfInstances)
        {
            WsGetNumberOfInstances(req);
        }
        else if (req.Method() == Methods::GetAllInstanceIds)
        {
            WsGetAllInstanceIds(req);
        }
        else if (req.Method() == Methods::Ping)
        {
            WsPing(req);
        }
        else if (req.Method() == Methods::Open)
        {
            WsOpen(req);
        }
        else if (req.Method() == Methods::Close)
        {
            WsClose(req);
        }
        else if (req.Method() == Methods::IsOpen)
        {
            WsIsOpen(req);
        }
        else if (req.Method() == Methods::GetInstanceIdPolicy)
        {
            WsGetInstanceIdPolicy(req);
        }
        else if (req.Method() == Methods::GetTypeHierarchy && m_enableTypeSystem)
        {
            WsGetTypeHierarchy(req);
        }
        else
        {
            throw RequestErrorException(JsonRpcErrorCodes::MethodNotFound, "Command is not supported. " + req.Method());
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
    catch (const Safir::Dob::LowMemoryException& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::SafirLowMemoryException, e.what());
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
    catch (const Safir::Dob::NotFoundException& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::SafirNotFound, e.what());
        SendToClient(JsonRpcResponse::Error(req.Id(), err.Code(), err.Message(), err.Data()));
    }
    catch (const Safir::Dob::Typesystem::IllegalValueException& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::SafirIllegalValue, e.what());
        SendToClient(JsonRpcResponse::Error(req.Id(), err.Code(), err.Message(), err.Data()));
    }
    catch (const Safir::Dob::Typesystem::SoftwareViolationException& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::SafirSoftwareViolation, e.what());
        SendToClient(JsonRpcResponse::Error(req.Id(), err.Code(), err.Message(), err.Data()));
    }
    catch (const Safir::Dob::Typesystem::ReadOnlyException& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::SafirReadOnly, e.what());
        SendToClient(JsonRpcResponse::Error(req.Id(), err.Code(), err.Message(), err.Data()));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::SafirUnexpectedException, e.what());
        auto error = JsonRpcResponse::Error(req.Id(), err.Code(), err.Message(), err.Data());
        SendToClient(error);
        SEND_SYSTEM_LOG(Error, << "WS: Got unexpected Safir exception: " << error.c_str() << std::endl);
        lllog(5) << "WS: Got unexpected Safir exception: " << error.c_str() << std::endl;
    }
    catch (const std::exception& e)
    {
        RequestErrorException err(JsonRpcErrorCodes::ServerError, e.what());
        SendToClient(JsonRpcResponse::Error(req.Id(), err.Code(), err.Message(), err.Data()));
        SEND_SYSTEM_LOG(Error, << "WS: Unexpected exception: " << e.what() << std::endl);
        lllog(5) << "WS: Unexpected exception: " << e.what() << std::endl;
    }
}

void RemoteClient::WsResponse(const JsonRpcRequest& req)
{
    CommandValidator::ValidateResponse(req);
    sd::ResponsePtr response = JsonHelpers::ToObject<sd::Response>(req.Result());
    m_dobConnection->SendResponse(response, req.Id().Int());
}

void RemoteClient::WsPing(const JsonRpcRequest& req)
{
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "pong"));
}

void RemoteClient::WsOpen(const JsonRpcRequest& req)
{
    CommandValidator::ValidateOpen(req);
    RemoveDobConnectionFromRegistry();

    m_connectionName = req.ConnectionName();
    auto context = req.HasContext() ? req.Context() : 0;
    m_dobConnection->Open(Wstr(m_connectionName), context);
    
    if (m_dobConnectionRegistry)
    {
        m_dobConnectionRegistry->InsertConnection(m_connectionName, m_dobConnection, m_strand);
    }
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsClose(const JsonRpcRequest& req)
{
    m_dobConnection->Close();
    RemoveDobConnectionFromRegistry();
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsIsOpen(const JsonRpcRequest& req)
{
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::Bool(req.Id(), m_dobConnection->IsOpen()));
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
    auto channel = req.HasChannelId() ? req.ChannelId() : ts::ChannelId::ALL_CHANNELS;
    auto includeSub = req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
    m_dobConnection->SubscribeMessage(req.TypeId(), channel, includeSub);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsSendMessage(const JsonRpcRequest& req)
{
    CommandValidator::ValidateSendMessage(req);
    auto channel = req.HasChannelId() ? req.ChannelId() : ts::ChannelId();
    sd::MessagePtr message = JsonHelpers::ToObject<sd::Message>(req.Message());
    m_dobConnection->SendMessage(message, channel);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsUnsubscribeMessage(const JsonRpcRequest& req)
{
    CommandValidator::ValidateUnsubscribeMessage(req);
    auto channel = req.HasChannelId() ? req.ChannelId() : ts::ChannelId::ALL_CHANNELS;
    auto includeUpdates = req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
    m_dobConnection->UnsubscribeMessage(req.TypeId(), channel, includeUpdates);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsSubscribeEntity(const JsonRpcRequest& req)
{
    CommandValidator::ValidateSubscribeEntity(req);
    auto includeUpdates = req.HasIncludeUpdates() ? req.IncludeUpdates() : true;
    auto restartSub = req.HasRestartSubscription() ? req.RestartSubscription() : true;

    if (req.HasInstanceId())
    {
        auto entityId = ts::EntityId(req.TypeId(), req.InstanceId());
        m_dobConnection->SubscribeEntity(entityId, includeUpdates, restartSub);
    }
    else
    {
        auto includeSubclasses = req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
        m_dobConnection->SubscribeEntity(req.TypeId(), includeUpdates, includeSubclasses, restartSub);
    }

    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsUnsubscribeEntity(const JsonRpcRequest& req)
{
    CommandValidator::ValidateUnsubscribeEntity(req);

    if (req.HasInstanceId())
    {
        auto entityId = ts::EntityId(req.TypeId(), req.InstanceId());
        m_dobConnection->UnsubscribeEntity(entityId);
    }
    else
    {
        auto includeSubclasses = req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
        m_dobConnection->UnsubscribeEntity(req.TypeId(), includeSubclasses);
    }

    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsRegisterEntityHandler(const JsonRpcRequest& req)
{
    CommandValidator::ValidateRegisterEntityHandler(req);
    auto handler = req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    auto instPolicy = req.HasInstanceIdPolicy() ? req.InstanceIdPolicy() : sd::InstanceIdPolicy::RequestorDecidesInstanceId;
    auto injectionHandler = req.HasInjectionHandler() ? req.InjectionHandler() : false;
    auto pendingReg = req.HasPending() ? req.Pending() : false;
    m_dobConnection->RegisterEntity(req.TypeId(), handler, instPolicy, injectionHandler, pendingReg);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsRegisterServiceHandler(const JsonRpcRequest& req)
{
    CommandValidator::ValidateRegisterServiceHandler(req);
    auto handler = req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    auto pendingReg = req.HasPending() ? req.Pending() : false;
    m_dobConnection->RegisterService(req.TypeId(), handler, pendingReg);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsUnregisterHandler(const JsonRpcRequest& req)
{
    CommandValidator::ValidateUnregisterHandler(req);
    auto handler = req.HasHandlerId() ? req.HandlerId() : ts::HandlerId::ALL_HANDLERS;
    m_dobConnection->UnregisterHandler(req.TypeId(), handler);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsSubscribeRegistration(const JsonRpcRequest& req)
{
    CommandValidator::ValidateSubscribeRegistration(req);
    auto handler = req.HasHandlerId() ? req.HandlerId() : ts::HandlerId::ALL_HANDLERS;
    auto inclSub = req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
    auto restartSub = req.HasRestartSubscription() ? req.RestartSubscription() : true;
    m_dobConnection->SubscribeRegistration(req.TypeId(), handler, inclSub, restartSub);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsUnsubscribeRegistration(const JsonRpcRequest& req)
{
    CommandValidator::ValidateUnsubscribeRegistration(req);
    auto handler = req.HasHandlerId() ? req.HandlerId() : ts::HandlerId::ALL_HANDLERS;
    auto inclSub = req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
    m_dobConnection->UnsubscribeRegistration(req.TypeId(), handler, inclSub);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsCreateRequest(const JsonRpcRequest& req)
{
    CommandValidator::ValidateCreateRequest(req);
    auto handler = req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    sd::EntityPtr entity = JsonHelpers::ToObject<sd::Entity>(req.Entity());
    if (req.HasInstanceId())
    {
        m_dobConnection->CreateRequest(entity, req.InstanceId(), handler, req.Id());
    }
    else
    {
        m_dobConnection->CreateRequest(entity, handler, req.Id());
    }
}

void RemoteClient::WsUpdateRequest(const JsonRpcRequest& req)
{
    CommandValidator::ValidateUpdateRequest(req);
    sd::EntityPtr entity = JsonHelpers::ToObject<sd::Entity>(req.Entity());
    m_dobConnection->UpdateRequest(entity, req.InstanceId(), req.Id());
}

void RemoteClient::WsDeleteRequest(const JsonRpcRequest& req)
{
    CommandValidator::ValidateDeleteRequest(req);
    m_dobConnection->DeleteRequest(req.TypeId(), req.InstanceId(), req.Id());
}

void RemoteClient::WsServiceRequest(const JsonRpcRequest& req)
{
    CommandValidator::ValidateServiceRequest(req);
    auto handler = req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    sd::ServicePtr service = JsonHelpers::ToObject<sd::Service>(req.Request());
    m_dobConnection->ServiceRequest(service, handler, req.Id());
}

void RemoteClient::WsSetEntityChanges(const JsonRpcRequest& req)
{
    CommandValidator::ValidateSetEntityChanges(req);
    sd::EntityPtr entity = JsonHelpers::ToObject<sd::Entity>(req.Entity());
    auto handler = req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    m_dobConnection->SetChanges(entity, req.InstanceId(), handler);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsSetEntity(const JsonRpcRequest& req)
{
    CommandValidator::ValidateSetEntity(req);
    sd::EntityPtr entity = JsonHelpers::ToObject<sd::Entity>(req.Entity());
    auto handler = req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    m_dobConnection->SetAll(entity, req.InstanceId(), handler);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsDeleteEntity(const JsonRpcRequest& req)
{
    CommandValidator::ValidateDeleteEntity(req);
    auto handler = req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    m_dobConnection->Delete(req.TypeId(), req.InstanceId(), handler);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsDeleteAllInstances(const JsonRpcRequest& req)
{
    CommandValidator::ValidateDeleteAllInstances(req);
    auto handler = req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    m_dobConnection->DeleteAllInstances(req.TypeId(), handler);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), "OK"));
}

void RemoteClient::WsReadEntity(const JsonRpcRequest& req)
{
    CommandValidator::ValidateReadEntity(req);
    auto entity = m_dobConnection->Read(req.TypeId(), req.InstanceId());
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::Json(req.Id(), entity));
}

void RemoteClient::WsIsCreated(const JsonRpcRequest& req)
{
    CommandValidator::ValidateIsCreated(req);
    auto isCreated = m_dobConnection->IsCreated(req.TypeId(), req.InstanceId());
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::Bool(req.Id(), isCreated));
}

void RemoteClient::WsGetNumberOfInstances(const JsonRpcRequest& req)
{
    CommandValidator::ValidateGetNumberOfInstances(req);
    auto handler = req.HasHandlerId() ? req.HandlerId() : ts::HandlerId::ALL_HANDLERS;
    auto inclSub = req.HasIncludeSubclasses() ? req.IncludeSubclasses() : true;
    ts::Int64 num = m_dobConnection->GetNumberOfInstances(req.TypeId(), handler, inclSub);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::Int(req.Id(), num));
}

void RemoteClient::WsGetInstanceIdPolicy(const JsonRpcRequest& req)
{
    CommandValidator::ValidateGetInstanceIdPolicy(req);
    auto handler = req.HasHandlerId() ? req.HandlerId() : ts::HandlerId();
    auto policy = m_dobConnection->GetInstanceIdPolicy(req.TypeId(), handler);
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::String(req.Id(), policy));
}

void RemoteClient::WsGetAllInstanceIds(const JsonRpcRequest& req)
{
    CommandValidator::ValidateGetAllInstanceIds(req);
    auto ids = m_dobConnection->GetAllInstanceIds(req.TypeId());
    if (!req.Id().IsNull())
        SendToClient(JsonRpcResponse::UnquotedArray(req.Id(), ids));
}
