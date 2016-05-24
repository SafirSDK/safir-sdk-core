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
#pragma once

#include <string>
#include <vector>
#include <functional>
#include <boost/optional.hpp>
#include <Safir/Dob/Connection.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/QueueParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/GhostExistsException.h>
#include <Safir/Dob/NotFoundException.h>
#include "RequestIdMapper.h"
#include "ResponseSenderStore.h"
#include "JsonHelpers.h"

namespace sd = Safir::Dob;
namespace ts = Safir::Dob::Typesystem;

class DobConnection :
        public sd::EntityHandler,
        public sd::EntityHandlerInjection,
        public sd::EntityHandlerPending,
        public sd::EntitySubscriber,
        public sd::RegistrationSubscriber,
        public sd::Requestor,
        public sd::MessageSender,
        public sd::MessageSubscriber,
        public sd::ServiceHandler,
        public sd::ServiceHandlerPending
{
public:

    DobConnection(boost::asio::strand& strand, boost::function<void(const std::string&)> send);
    sd::Connection& Connection() {return m_con;}

    void Open(const std::wstring& name, int context) {m_con.Open(name, L"-ws", context, nullptr, &m_dispatcher);}
    void Close() {if (m_con.IsOpen()) m_con.Close();}
    bool IsOpen() const {return m_con.IsOpen();}
    void SubscribeMessage(ts::TypeId typeId, const ts::ChannelId& ch, bool includeSubclasses) {m_con.SubscribeMessage(typeId, ch, includeSubclasses, this);}
    void UnsubscribeMessage(ts::TypeId typeId, const ts::ChannelId& ch, bool includeSubclasses) {m_con.UnsubscribeMessage(typeId, ch, includeSubclasses, this);}
    void SendMessage(const sd::MessagePtr msg, const ts::ChannelId& ch) {m_con.Send(msg, ch, this);}

    void SubscribeEntity(ts::TypeId typeId, bool inclUpd, bool inclSub, bool restart) {m_con.SubscribeEntity(typeId, inclUpd, inclSub, restart, this);}
    void SubscribeEntity(const ts::EntityId& eid, bool inclUpd, bool restart) {m_con.SubscribeEntity(eid, inclUpd, restart, this);}
    void UnsubscribeEntity(ts::TypeId typeId, bool inclSub) {m_con.UnsubscribeEntity(typeId, inclSub, this);}
    void UnsubscribeEntity(const ts::EntityId& eid) {m_con.UnsubscribeEntity(eid, this);}

    void RegisterEntity(ts::TypeId typeId, const ts::HandlerId& handler, sd::InstanceIdPolicy::Enumeration policy, bool injection, bool pending)
    {
        if (injection)
            m_con.RegisterEntityHandlerInjection(typeId, handler, policy, this);
        else if (pending)
            m_con.RegisterEntityHandlerPending(typeId, handler, policy, this);
        else
            m_con.RegisterEntityHandler(typeId, handler, policy, this);
    }

    void RegisterService(ts::TypeId typeId, const ts::HandlerId& handler, bool pending)
    {
        if (pending)
            m_con.RegisterServiceHandlerPending(typeId, handler, this);
        else
            m_con.RegisterServiceHandler(typeId, handler, this);
    }

    void UnregisterHandler(ts::TypeId typeId, const ts::HandlerId& handler) {m_con.UnregisterHandler(typeId, handler);}

    void SubscribeRegistration(ts::TypeId typeId, const ts::HandlerId& handler, bool inclSub, bool restart) {m_con.SubscribeRegistration(typeId, handler, inclSub, restart, this);}

    void UnsubscribeRegistration(ts::TypeId typeId, const ts::HandlerId& handler, bool inclSub) {m_con.UnsubscribeRegistration(typeId, handler, inclSub, this);}

    void CreateRequest(const sd::EntityPtr& req, const ts::HandlerId& handler, const JsonRpcId& id)
    {
        auto reqId=m_con.CreateRequest(req, handler, this);
        m_reqIdMapper.Add(reqId, id);
    }

    void CreateRequest(const sd::EntityPtr& req, const ts::InstanceId& inst, const ts::HandlerId& handler, const JsonRpcId& id)
    {
        auto reqId=m_con.CreateRequest(req, inst, handler, this);
        m_reqIdMapper.Add(reqId, id);
    }

    void UpdateRequest(const sd::EntityPtr& req, const ts::InstanceId& inst, const JsonRpcId& id)
    {
        auto reqId=m_con.UpdateRequest(req, inst, this);
        m_reqIdMapper.Add(reqId, id);
    }

    void DeleteRequest(ts::TypeId typeId, const ts::InstanceId& inst, const JsonRpcId& id)
    {
        auto reqId=m_con.DeleteRequest(ts::EntityId(typeId, inst), this);
        m_reqIdMapper.Add(reqId, id);
    }

    void ServiceRequest(const sd::ServicePtr& req, const ts::HandlerId& handler, const JsonRpcId& id)
    {
        auto reqId=m_con.ServiceRequest(req, handler, this);
        m_reqIdMapper.Add(reqId, id);
    }

    void SetChanges(const sd::EntityPtr& entity, const ts::InstanceId& inst, const ts::HandlerId& handler) {m_con.SetChanges(entity, inst, handler);}
    void SetAll(const sd::EntityPtr& entity, const ts::InstanceId& inst, const ts::HandlerId& handler) {m_con.SetAll(entity, inst, handler);}

    void Delete(ts::TypeId typeId, const ts::InstanceId& inst, const ts::HandlerId& handler)
    {
        m_con.Delete(ts::EntityId(typeId, inst), handler);
    }

    void DeleteAllInstances(ts::TypeId typeId, const ts::HandlerId& handler) {m_con.DeleteAllInstances(typeId, handler);}

    std::string Read(ts::TypeId typeId, const ts::InstanceId& inst) const
    {
        auto proxy=m_con.Read(ts::EntityId(typeId, inst));
        return std::move(ts::Internal::ToJson(proxy.GetBlob()));
    }

    bool IsCreated(ts::TypeId typeId, const ts::InstanceId& inst) const {return m_con.IsCreated(ts::EntityId(typeId, inst));}

    ts::Int64 GetNumberOfInstances(ts::TypeId typeId, const ts::HandlerId& handler, bool inclSub) const {return m_con.GetNumberOfInstances(typeId, handler, inclSub);}
    std::string GetInstanceIdPolicy(ts::TypeId typeId, const ts::HandlerId& handler) const
    {
        auto policy=m_con.GetInstanceIdPolicy(typeId, handler);
        return std::move(ts::Utilities::ToUtf8(sd::InstanceIdPolicy::ToString(policy)));
    }

    std::vector<ts::InstanceId> GetAllInstanceIds(ts::TypeId typeId) const
    {
        std::vector<ts::InstanceId> instances;
        auto end=sd::EntityIterator();
        for (auto it=m_con.GetEntityIterator(typeId, false); it!=end; ++it)
        {
            instances.emplace_back(it->GetInstanceId());
        }
        return std::move(instances);
    }

    std::string GetName(ts::TypeId typeId) const
    {
        try
        {
            return ts::Utilities::ToUtf8(ts::Operations::GetName(typeId));
        }
        catch (const std::exception&)
        {
            lllog(5)<<"DobConnection::GetName. Type not found. TypeId"<<typeId<<std::endl;
            return "<unknown_type>";
        }
    }

    void SendResponse(const sd::ResponsePtr& response, boost::uint64_t id)
    {
        auto responseSender=m_responseSenderStore.Get(id);
        if (responseSender)
        {
            responseSender->Send(response);
        }
    }

private:
    sd::Connection m_con;
    Safir::Utilities::AsioDispatcher m_dispatcher;
    boost::function<void(const std::string&)> m_wsSend;
    RequestIdMapper m_reqIdMapper;
    ResponseSenderStore m_responseSenderStore;
    ProxyToJson m_proxyToJson;

    //DOB events
    //-----------------
    // EntityHandler interface
    void OnCreateRequest(const sd::EntityRequestProxy entityRequestProxy, sd::ResponseSenderPtr responseSender);
    void OnUpdateRequest(const sd::EntityRequestProxy entityRequestProxy, sd::ResponseSenderPtr responseSender);
    void OnDeleteRequest(const sd::EntityRequestProxy entityRequestProxy, sd::ResponseSenderPtr responseSender);
    void OnRevokedRegistration(const sd::Typesystem::TypeId typeId, const sd::Typesystem::HandlerId &handlerId);
    void OnCompletedRegistration(const sd::Typesystem::TypeId typeId, const sd::Typesystem::HandlerId& handlerId);

    //Injection interface
    void OnInjectedNewEntity(const sd::InjectedEntityProxy injectedEntityProxy);
    void OnInjectedUpdatedEntity(const sd::InjectedEntityProxy injectedEntityProxy);
    void OnInjectedDeletedEntity(const sd::InjectedEntityProxy injectedEntityProxy);
    void OnInitialInjectionsDone(const sd::Typesystem::TypeId typeId, const sd::Typesystem::HandlerId& handlerId);

    //EntitySubscriber interface
    void OnNewEntity(const sd::EntityProxy entityProxy);
    void OnUpdatedEntity(const sd::EntityProxy entityProxy);
    void OnDeletedEntity(const sd::EntityProxy entityProxy, const bool);

    //RegistrationSubscriber interface
    void OnRegistered(const ts::TypeId typeId, const ts::HandlerId&  handlerId);
    void OnUnregistered(const ts::TypeId typeId, const Safir::Dob::Typesystem::HandlerId&  handlerId);

    //Requestor interface
    void OnResponse(const sd::ResponseProxy responseProxy);
    void OnNotRequestOverflow();

    //MessageSender interface
    void OnNotMessageOverflow();

    //MessageSubscriber interface
    void OnMessage(const sd::MessageProxy messageProxy);

    //ServiceHandler interface
    void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy, Safir::Dob::ResponseSenderPtr responseSender);
};
