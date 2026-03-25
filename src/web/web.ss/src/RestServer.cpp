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
#include <rapidjson/document.h>
#include <rapidjson/writer.h>
#include <rapidjson/stringbuffer.h>
#include <boost/beast.hpp>
#include <boost/beast/http.hpp>
#include <boost/asio/post.hpp>
#include <chrono>
#include <unordered_map>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeUtilities.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Service.h>
#include <Safir/Web/Parameters.h>
#include "RestServer.h"
#include "DobConnectionRegistry.h"
#include "IpAddressHelper.h"
#include "JsonHelpers.h"
#include "Methods.h"
#include "RestRouting.h"
#include "Typesystem.h"
#include "JsonRpcResponse.h"
#include "RequestErrorException.h"
#include "JsonRpcId.h"

namespace ws = Safir::Web;
namespace http = boost::beast::http;
namespace beast = boost::beast;

namespace
{

// ---------------------------------------------------------------------------
// Response builders
// ---------------------------------------------------------------------------

std::string JsonError(const std::string& message)
{
    rapidjson::Document doc(rapidjson::kObjectType);
    auto& allocator = doc.GetAllocator();
    doc.AddMember("error", rapidjson::Value(message.c_str(), allocator), allocator);

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    doc.Accept(writer);
    return buffer.GetString();
}

std::string RestStatusOk()
{
    rapidjson::Document doc(rapidjson::kObjectType);
    auto& allocator = doc.GetAllocator();
    doc.AddMember("status", "OK", allocator);

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    doc.Accept(writer);
    return buffer.GetString();
}

std::string RestResultFromParsedValue(const rapidjson::Value& value)
{
    rapidjson::Document doc(rapidjson::kObjectType);
    auto& allocator = doc.GetAllocator();
    rapidjson::Value result;
    result.CopyFrom(value, allocator);
    doc.AddMember("result", result, allocator);

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    doc.Accept(writer);
    return buffer.GetString();
}

std::string RestResultFromJsonText(const std::string& json)
{
    rapidjson::Document valueDoc;
    valueDoc.Parse(json.c_str());
    if (valueDoc.HasParseError())
    {
        throw std::invalid_argument("Failed to parse JSON result");
    }

    return RestResultFromParsedValue(valueDoc);
}

std::string RestResultFromJsonRpcResult(const std::string& jsonRpc)
{
    rapidjson::Document rpcDoc;
    rpcDoc.Parse(jsonRpc.c_str());
    if (rpcDoc.HasParseError() || !rpcDoc.IsObject() || !rpcDoc.HasMember("result"))
    {
        throw std::invalid_argument("Failed to parse JSON-RPC result");
    }

    return RestResultFromParsedValue(rpcDoc["result"]);
}

std::string RestResultString(const std::string& value)
{
    rapidjson::Document doc(rapidjson::kObjectType);
    auto& allocator = doc.GetAllocator();
    doc.AddMember("result", rapidjson::Value(value.c_str(), allocator), allocator);

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    doc.Accept(writer);
    return buffer.GetString();
}

std::string RestResultBool(bool value)
{
    rapidjson::Document doc(rapidjson::kObjectType);
    auto& allocator = doc.GetAllocator();
    doc.AddMember("result", value, allocator);

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    doc.Accept(writer);
    return buffer.GetString();
}

std::string RestResultInt(std::int64_t value)
{
    rapidjson::Document doc(rapidjson::kObjectType);
    auto& allocator = doc.GetAllocator();
    doc.AddMember("result", value, allocator);

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    doc.Accept(writer);
    return buffer.GetString();
}

std::string RestIsOpenResponse(bool isOpen)
{
    rapidjson::Document doc(rapidjson::kObjectType);
    auto& allocator = doc.GetAllocator();
    doc.AddMember("isOpen", isOpen, allocator);

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    doc.Accept(writer);
    return buffer.GetString();
}

std::string RestReadEntityResponse(const std::string& entityJson)
{
    rapidjson::Document doc(rapidjson::kObjectType);
    auto& allocator = doc.GetAllocator();

    rapidjson::Document entityDoc;
    entityDoc.Parse(entityJson.c_str());
    if (!entityDoc.HasParseError())
    {
        rapidjson::Value entityValue;
        entityValue.CopyFrom(entityDoc, allocator);
        doc.AddMember("entity", entityValue, allocator);
    }
    else
    {
        doc.AddMember("entity", rapidjson::Value().SetObject(), allocator);
    }

    rapidjson::StringBuffer buffer;
    rapidjson::Writer<rapidjson::StringBuffer> writer(buffer);
    doc.Accept(writer);
    return buffer.GetString();
}

// ---------------------------------------------------------------------------
// ID resolution helpers
// ---------------------------------------------------------------------------

DotsC_TypeId ResolveTypeId(const std::string& s)
{
    try { return std::stoll(s); } catch (...) {}
    return Safir::Dob::Typesystem::ToolSupport::TypeUtilities::CalculateTypeId(s);
}

ts::InstanceId ResolveInstanceId(const std::string& s)
{
    try { return ts::InstanceId(std::stoll(s)); } catch (...) {}
    return ts::InstanceId(ts::Utilities::ToWstring(s));
}

ts::HandlerId ResolveHandlerId(const std::string& s)
{
    try { return ts::HandlerId(std::stoll(s)); } catch (...) {}
    return ts::HandlerId(ts::Utilities::ToWstring(s));
}

ts::ChannelId ResolveChannelId(const std::string& s)
{
    try { return ts::ChannelId(std::stoll(s)); } catch (...) {}
    return ts::ChannelId(ts::Utilities::ToWstring(s));
}

// ---------------------------------------------------------------------------
// Execution
// ---------------------------------------------------------------------------

void ExecuteRestCall(std::shared_ptr<DobConnection> dobConnection,
                     const std::string& method,
                     const std::string& typeIdStr,
                     const std::string& instanceIdStr,
                     const std::unordered_map<std::string, std::string>& query,
                     const std::string& body,
                     std::function<void(const std::string&)> onSuccess,
                     std::function<void(const std::string&)> onError)
{
    try
    {
        if (method == Methods::IsOpen)
        {
            onSuccess(RestIsOpenResponse(dobConnection->IsOpen()));
        }
        else if (method == Methods::Ping)
        {
            onSuccess(RestResultString("pong"));
        }
        else if (method == Methods::GetTypeHierarchy)
        {
            onSuccess(RestResultFromJsonText(Typesystem::GetTypeHierarchy()));
        }
        else if (method == Methods::ReadEntity)
        {
            const auto typeId     = ResolveTypeId(typeIdStr);
            const auto instanceId = ResolveInstanceId(instanceIdStr);
            if (!ts::Operations::IsOfType(typeId, sd::Entity::ClassTypeId))
                throw std::invalid_argument("typeId must refer to a subtype of Safir.Dob.Entity");
            auto entity = dobConnection->Read(typeId, instanceId);
            onSuccess(RestReadEntityResponse(entity));
        }
        else if (method == Methods::IsCreated)
        {
            const auto typeId     = ResolveTypeId(typeIdStr);
            const auto instanceId = ResolveInstanceId(instanceIdStr);
            if (!ts::Operations::IsOfType(typeId, sd::Entity::ClassTypeId))
                throw std::invalid_argument("typeId must refer to a subtype of Safir.Dob.Entity");
            onSuccess(RestResultBool(dobConnection->IsCreated(typeId, instanceId)));
        }
        else if (method == Methods::GetAllInstanceIds)
        {
            const auto typeId = ResolveTypeId(typeIdStr);
            if (!ts::Operations::IsOfType(typeId, sd::Entity::ClassTypeId))
                throw std::invalid_argument("typeId must refer to a subtype of Safir.Dob.Entity");
            auto ids = dobConnection->GetAllInstanceIds(typeId);
            onSuccess(RestResultFromJsonRpcResult(JsonRpcResponse::UnquotedArray(JsonRpcId(1), ids)));
        }
        else if (method == Methods::GetInstanceIdPolicy)
        {
            const auto typeId = ResolveTypeId(typeIdStr);
            auto it = query.find("handler");
            auto handler = (it != query.end()) ? ResolveHandlerId(it->second) : ts::HandlerId();
            onSuccess(RestResultString(dobConnection->GetInstanceIdPolicy(typeId, handler)));
        }
        else if (method == Methods::GetNumberOfInstances)
        {
            const auto typeId = ResolveTypeId(typeIdStr);
            auto hit = query.find("handler");
            auto handler = (hit != query.end()) ? ResolveHandlerId(hit->second) : ts::HandlerId::ALL_HANDLERS;
            bool inclSub = ParseBoolParam(query, "includeSubclasses", true);
            ts::Int64 num = dobConnection->GetNumberOfInstances(typeId, handler, inclSub);
            onSuccess(RestResultInt(num));
        }
        else if (method == Methods::SetEntity)
        {
            if (body.empty())
                throw std::invalid_argument("Request body with entity JSON is required");
            const auto instanceId = ResolveInstanceId(instanceIdStr);
            auto hit = query.find("handler");
            auto handler = (hit != query.end()) ? ResolveHandlerId(hit->second) : ts::HandlerId();
            sd::EntityPtr entity = JsonHelpers::ToObject<sd::Entity>(body);
            dobConnection->SetAll(entity, instanceId, handler);
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::SetEntityChanges)
        {
            if (body.empty())
                throw std::invalid_argument("Request body with entity JSON is required");
            const auto instanceId = ResolveInstanceId(instanceIdStr);
            auto hit = query.find("handler");
            auto handler = (hit != query.end()) ? ResolveHandlerId(hit->second) : ts::HandlerId();
            sd::EntityPtr entity = JsonHelpers::ToObject<sd::Entity>(body);
            dobConnection->SetChanges(entity, instanceId, handler);
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::DeleteEntity)
        {
            const auto typeId     = ResolveTypeId(typeIdStr);
            const auto instanceId = ResolveInstanceId(instanceIdStr);
            if (!ts::Operations::IsOfType(typeId, sd::Entity::ClassTypeId))
                throw std::invalid_argument("typeId must refer to a subtype of Safir.Dob.Entity");
            auto hit = query.find("handler");
            auto handler = (hit != query.end()) ? ResolveHandlerId(hit->second) : ts::HandlerId();
            dobConnection->Delete(typeId, instanceId, handler);
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::DeleteAllInstances)
        {
            const auto typeId = ResolveTypeId(typeIdStr);
            if (!ts::Operations::IsOfType(typeId, sd::Entity::ClassTypeId))
                throw std::invalid_argument("typeId must refer to a subtype of Safir.Dob.Entity");
            auto hit = query.find("handler");
            auto handler = (hit != query.end()) ? ResolveHandlerId(hit->second) : ts::HandlerId();
            dobConnection->DeleteAllInstances(typeId, handler);
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::RegisterEntityHandler)
        {
            const auto typeId = ResolveTypeId(typeIdStr);
            if (!ts::Operations::IsOfType(typeId, sd::Entity::ClassTypeId))
                throw std::invalid_argument("typeId must refer to a subtype of Safir.Dob.Entity");
            auto hit = query.find("handler");
            auto handler = (hit != query.end()) ? ResolveHandlerId(hit->second) : ts::HandlerId();
            bool injectionHandler = ParseBoolParam(query, "injectionHandler", false);
            bool pending          = ParseBoolParam(query, "pending", false);
            if (injectionHandler && pending)
                throw std::invalid_argument("Not allowed to specify both pending and injectionHandler");
            auto policyIt = query.find("instanceIdPolicy");
            sd::InstanceIdPolicy::Enumeration instPolicy = sd::InstanceIdPolicy::RequestorDecidesInstanceId;
            if (policyIt != query.end() && policyIt->second == "HandlerDecidesInstanceId")
                instPolicy = sd::InstanceIdPolicy::HandlerDecidesInstanceId;
            dobConnection->RegisterEntity(typeId, handler, instPolicy, injectionHandler, pending);
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::RegisterServiceHandler)
        {
            const auto typeId = ResolveTypeId(typeIdStr);
            if (!ts::Operations::IsOfType(typeId, sd::Service::ClassTypeId))
                throw std::invalid_argument("typeId must refer to a subtype of Safir.Dob.Service");
            auto hit = query.find("handler");
            auto handler = (hit != query.end()) ? ResolveHandlerId(hit->second) : ts::HandlerId();
            bool pending = ParseBoolParam(query, "pending", false);
            dobConnection->RegisterService(typeId, handler, pending);
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::UnregisterHandler)
        {
            const auto typeId = ResolveTypeId(typeIdStr);
            auto hit = query.find("handler");
            auto handler = (hit != query.end()) ? ResolveHandlerId(hit->second) : ts::HandlerId::ALL_HANDLERS;
            dobConnection->UnregisterHandler(typeId, handler);
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::SubscribeEntity)
        {
            const auto typeId    = ResolveTypeId(typeIdStr);
            bool includeUpdates  = ParseBoolParam(query, "includeUpdates", true);
            bool restartSub      = ParseBoolParam(query, "restartSubscription", true);
            auto iidIt = query.find("instanceId");
            if (iidIt != query.end())
            {
                auto inclSubIt = query.find("includeSubclasses");
                if (inclSubIt != query.end())
                    throw std::invalid_argument("Not allowed to specify both instanceId and includeSubclasses");
                dobConnection->SubscribeEntity(ts::EntityId(typeId, ResolveInstanceId(iidIt->second)),
                                               includeUpdates, restartSub);
            }
            else
            {
                bool inclSub = ParseBoolParam(query, "includeSubclasses", true);
                dobConnection->SubscribeEntity(typeId, includeUpdates, inclSub, restartSub);
            }
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::UnsubscribeEntity)
        {
            const auto typeId = ResolveTypeId(typeIdStr);
            auto iidIt = query.find("instanceId");
            if (iidIt != query.end())
            {
                auto inclSubIt = query.find("includeSubclasses");
                if (inclSubIt != query.end())
                    throw std::invalid_argument("Not allowed to specify both instanceId and includeSubclasses");
                dobConnection->UnsubscribeEntity(ts::EntityId(typeId, ResolveInstanceId(iidIt->second)));
            }
            else
            {
                bool inclSub = ParseBoolParam(query, "includeSubclasses", true);
                dobConnection->UnsubscribeEntity(typeId, inclSub);
            }
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::SubscribeMessage)
        {
            const auto typeId = ResolveTypeId(typeIdStr);
            if (!ts::Operations::IsOfType(typeId, sd::Message::ClassTypeId))
                throw std::invalid_argument("typeId must refer to a subtype of Safir.Dob.Message");
            auto cit = query.find("channel");
            auto channel = (cit != query.end()) ? ResolveChannelId(cit->second) : ts::ChannelId::ALL_CHANNELS;
            bool inclSub = ParseBoolParam(query, "includeSubclasses", true);
            dobConnection->SubscribeMessage(typeId, channel, inclSub);
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::UnsubscribeMessage)
        {
            const auto typeId = ResolveTypeId(typeIdStr);
            auto cit = query.find("channel");
            auto channel = (cit != query.end()) ? ResolveChannelId(cit->second) : ts::ChannelId::ALL_CHANNELS;
            bool inclSub = ParseBoolParam(query, "includeSubclasses", true);
            dobConnection->UnsubscribeMessage(typeId, channel, inclSub);
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::SubscribeRegistration)
        {
            const auto typeId = ResolveTypeId(typeIdStr);
            if (!ts::Operations::IsOfType(typeId, sd::Entity::ClassTypeId) &&
                !ts::Operations::IsOfType(typeId, sd::Service::ClassTypeId))
                throw std::invalid_argument("typeId must refer to a subtype of Safir.Dob.Entity or Safir.Dob.Service");
            auto hit = query.find("handler");
            auto handler = (hit != query.end()) ? ResolveHandlerId(hit->second) : ts::HandlerId::ALL_HANDLERS;
            bool inclSub    = ParseBoolParam(query, "includeSubclasses", true);
            bool restartSub = ParseBoolParam(query, "restartSubscription", true);
            dobConnection->SubscribeRegistration(typeId, handler, inclSub, restartSub);
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::UnsubscribeRegistration)
        {
            const auto typeId = ResolveTypeId(typeIdStr);
            if (!ts::Operations::IsOfType(typeId, sd::Entity::ClassTypeId) &&
                !ts::Operations::IsOfType(typeId, sd::Service::ClassTypeId))
                throw std::invalid_argument("typeId must refer to a subtype of Safir.Dob.Entity or Safir.Dob.Service");
            auto hit = query.find("handler");
            auto handler = (hit != query.end()) ? ResolveHandlerId(hit->second) : ts::HandlerId::ALL_HANDLERS;
            bool inclSub = ParseBoolParam(query, "includeSubclasses", true);
            dobConnection->UnsubscribeRegistration(typeId, handler, inclSub);
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::SendMessage)
        {
            if (body.empty())
                throw std::invalid_argument("Request body with message JSON is required");
            auto cit = query.find("channel");
            auto channel = (cit != query.end()) ? ResolveChannelId(cit->second) : ts::ChannelId();
            sd::MessagePtr message = JsonHelpers::ToObject<sd::Message>(body);
            dobConnection->SendMessage(message, channel);
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::CreateRequest)
        {
            if (body.empty())
                throw std::invalid_argument("Request body with entity JSON is required");
            auto hit = query.find("handler");
            auto handler = (hit != query.end()) ? ResolveHandlerId(hit->second) : ts::HandlerId();
            sd::EntityPtr entity = JsonHelpers::ToObject<sd::Entity>(body);
            auto iidIt = query.find("instanceId");
            if (iidIt != query.end())
                dobConnection->CreateRequest(entity, ResolveInstanceId(iidIt->second), handler, JsonRpcId());
            else
                dobConnection->CreateRequest(entity, handler, JsonRpcId());
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::UpdateRequest)
        {
            if (body.empty())
                throw std::invalid_argument("Request body with entity JSON is required");
            const auto instanceId = ResolveInstanceId(instanceIdStr);
            sd::EntityPtr entity = JsonHelpers::ToObject<sd::Entity>(body);
            dobConnection->UpdateRequest(entity, instanceId, JsonRpcId());
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::DeleteRequest)
        {
            const auto typeId     = ResolveTypeId(typeIdStr);
            const auto instanceId = ResolveInstanceId(instanceIdStr);
            if (!ts::Operations::IsOfType(typeId, sd::Entity::ClassTypeId))
                throw std::invalid_argument("typeId must refer to a subtype of Safir.Dob.Entity");
            dobConnection->DeleteRequest(typeId, instanceId, JsonRpcId());
            onSuccess(RestStatusOk());
        }
        else if (method == Methods::ServiceRequest)
        {
            if (body.empty())
                throw std::invalid_argument("Request body with service JSON is required");
            auto hit = query.find("handler");
            auto handler = (hit != query.end()) ? ResolveHandlerId(hit->second) : ts::HandlerId();
            sd::ServicePtr service = JsonHelpers::ToObject<sd::Service>(body);
            dobConnection->ServiceRequest(service, handler, JsonRpcId());
            onSuccess(RestStatusOk());
        }
        else
        {
            onError("Unsupported method: " + method);
        }
    }
    catch (const RequestErrorException& e)
    {
        onError(e.Message() + (e.Data().empty() ? "" : (". " + e.Data())));
    }
    catch (const std::exception& e)
    {
        onError(e.what());
    }
    catch (...)
    {
        onError("Unexpected error while executing REST command");
    }
}

// ---------------------------------------------------------------------------
// HTTP session
// ---------------------------------------------------------------------------

class RestHttpSession : public std::enable_shared_from_this<RestHttpSession>
{
public:
    RestHttpSession(boost::asio::ip::tcp::socket socket,
                    std::function<std::pair<std::shared_ptr<boost::asio::io_context::strand>,
                                            std::shared_ptr<DobConnection>>(const std::string&)> getDobConnectionFunc)
        : m_stream(std::move(socket))
        , m_getDobConnectionFunc(std::move(getDobConnectionFunc))
    {
    }

    void Start()
    {
        DoRead();
    }

private:
    beast::tcp_stream m_stream;
    beast::flat_buffer m_buffer;
    http::request_parser<http::string_body> m_parser;
    std::function<std::pair<std::shared_ptr<boost::asio::io_context::strand>,
                            std::shared_ptr<DobConnection>>(const std::string&)> m_getDobConnectionFunc;

    void DoRead()
    {
        m_parser.body_limit(5 * 1024 * 1024); // 5 MB limit
        m_stream.expires_after(std::chrono::seconds(30));
        auto self = shared_from_this();
        http::async_read(m_stream, m_buffer, m_parser,
                         [self](const boost::system::error_code& ec, std::size_t)
        {
            if (ec)
            {
                if (ec == http::error::body_limit)
                {
                    self->SendResponse(http::status::payload_too_large,
                                       JsonError("Request body too large"));
                }
                return;
            }
            self->HandleRequest();
        });
    }

    void SendResponse(http::status statusCode, const std::string& body,
                      const std::string& allowHeader = {})
    {
        auto response = std::make_shared<http::response<http::string_body>>(statusCode, m_parser.get().version());
        response->set(http::field::server, "safir_websocket_rest");
        response->set(http::field::content_type, "application/json");
        if (!allowHeader.empty())
            response->set(http::field::allow, allowHeader);
        response->keep_alive(false);
        response->body() = body;
        response->prepare_payload();

        m_stream.expires_after(std::chrono::seconds(30));
        auto self = shared_from_this();
        http::async_write(m_stream, *response,
                          [self, response](const boost::system::error_code&, std::size_t)
        {
            boost::system::error_code ec;
            self->m_stream.socket().shutdown(boost::asio::ip::tcp::socket::shutdown_send, ec);
            self->m_stream.socket().close(ec);
        });
    }

    void HandleRequest()
    {
        const std::string target(m_parser.get().target());
        const http::verb verb = m_parser.get().method();

        std::vector<std::string> segments;
        std::unordered_map<std::string, std::string> query;
        ParseUrlTarget(target, segments, query);

        const RestRoute route = RouteRestRequest(verb, segments, query);

        if (route.method.empty())
        {
            SendResponse(http::status::not_found, JsonError("Unknown endpoint"));
            return;
        }

        if (route.method == kWrongVerb)
        {
            SendResponse(http::status::method_not_allowed,
                         JsonError("Method not allowed for this endpoint"),
                         "GET, PUT, PATCH, DELETE, POST");
            return;
        }

        // getTypeHierarchy needs no connection
        if (route.method == Methods::GetTypeHierarchy)
        {
            try
            {
                SendResponse(http::status::ok, RestResultFromJsonText(Typesystem::GetTypeHierarchy()));
            }
            catch (const std::exception& e)
            {
                SendResponse(http::status::internal_server_error, JsonError(e.what()));
            }
            return;
        }

        auto strandAndConnection = m_getDobConnectionFunc(route.connectionId);
        if (strandAndConnection.first == nullptr || strandAndConnection.second == nullptr)
        {
            SendResponse(http::status::not_found, JsonError("Unknown connectionId"));
            return;
        }

        // Only parse body for verbs that carry one
        std::string body;
        if (verb == http::verb::put || verb == http::verb::patch || verb == http::verb::post)
        {
            body = m_parser.get().body();
            // If a body is present it must be valid JSON (or empty for endpoints that don't need it)
            if (!body.empty())
            {
                rapidjson::Document doc;
                doc.Parse(body.c_str());
                if (doc.HasParseError())
                {
                    SendResponse(http::status::bad_request,
                                 JsonError("Request body is not valid JSON"));
                    return;
                }
            }
        }

        auto self = shared_from_this();
        const std::string method      = route.method;
        const std::string typeIdStr   = route.typeIdStr;
        const std::string instanceStr = route.instanceIdStr;

        boost::asio::post(*strandAndConnection.first,
            [self, method, typeIdStr, instanceStr, query = std::move(query),
             body = std::move(body), connection = strandAndConnection.second]()
            {
                ExecuteRestCall(connection, method, typeIdStr, instanceStr, query, body,
                    [self](const std::string& responseBody)
                    {
                        self->SendResponse(http::status::ok, responseBody);
                    },
                    [self](const std::string& error)
                    {
                        self->SendResponse(http::status::bad_request, JsonError(error));
                    });
            });
    }
};
}

RestServer::RestServer(boost::asio::io_context& io,
                       const std::shared_ptr<DobConnectionRegistry>& dobConnectionRegistry)
    : m_dobConnectionRegistry(dobConnectionRegistry)
    , m_acceptor(io)
    , m_isRunning(false)
    , m_isTerminating(false)
{
}

void RestServer::Run()
{
    if (m_isRunning)
    {
        return;
    }

    std::string ip = "";
    unsigned short port = 0;
    if (!IpAddressHelper::SplitAddress(ts::Utilities::ToUtf8(ws::Parameters::RestServerEndpoint()), ip, port))
    {
        SEND_SYSTEM_LOG(Error, << "REST: ServerEndpoint from configuration could not be parsed as <ip>:<port>" << std::endl);
        return;
    }

    const unsigned short restPort = static_cast<unsigned short>(port);

    boost::asio::ip::tcp::endpoint endpoint;
    try
    {
        endpoint = IpAddressHelper::CreateEndpoint(ip, restPort);
    }
    catch (const std::exception& e)
    {
        SEND_SYSTEM_LOG(Error, << "REST: Could not create REST endpoint. " << e.what() << std::endl);
        return;
    }

    boost::system::error_code ec;
    m_acceptor.open(endpoint.protocol(), ec);
    if (ec)
    {
        SEND_SYSTEM_LOG(Error, << "REST: Could not open acceptor. " << ec << std::endl);
        return;
    }

    m_acceptor.set_option(boost::asio::ip::tcp::acceptor::reuse_address(true), ec);
    if (ec)
    {
        SEND_SYSTEM_LOG(Error, << "REST: Could not set reuse_address. " << ec << std::endl);
        return;
    }

    m_acceptor.bind(endpoint, ec);
    if (ec)
    {
        SEND_SYSTEM_LOG(Error, << "REST: Could not bind acceptor. " << ec << std::endl);
        return;
    }

    m_acceptor.listen(boost::asio::socket_base::max_listen_connections, ec);
    if (ec)
    {
        SEND_SYSTEM_LOG(Error, << "REST: Could not listen. " << ec << std::endl);
        return;
    }

    m_isRunning = true;
    StartAccept();
}

void RestServer::Terminate()
{
    if (m_isTerminating)
    {
        return;
    }

    m_isTerminating = true;
    boost::system::error_code ec;
    m_acceptor.cancel(ec);
    m_acceptor.close(ec);
}

void RestServer::StartAccept()
{
    m_acceptor.async_accept([this](const boost::system::error_code& ec, boost::asio::ip::tcp::socket socket)
    {
        if (!ec)
        {
            std::make_shared<RestHttpSession>(std::move(socket), [this](auto connId){return m_dobConnectionRegistry->GetConnection(connId); })->Start();
        }

        if (!m_isTerminating)
        {
            StartAccept();
        }
    });
}
