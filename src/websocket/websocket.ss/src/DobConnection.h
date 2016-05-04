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
#include <functional>
#include <boost/optional.hpp>
#include <Safir/Dob/Connection.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Websocket/Send.h>
#include <Safir/Websocket/Receive.h>

namespace sd = Safir::Dob;
namespace ts = Safir::Dob::Typesystem;

class DobConnection :
        public sd::StopHandler,
        public sd::EntityHandler,
        public sd::EntityHandlerInjection,
        public sd::EntityHandlerPending,
        public sd::EntitySubscriber,
        public sd::Requestor,
        public sd::MessageSender,
        public sd::MessageSubscriber,
        public sd::ServiceHandler
{
public:

    DobConnection(boost::asio::io_service& ioService, boost::function<void(const std::string&)> send, boost::function<void()> onStopOrder);
    sd::Connection& Connection() {return m_con;}

    void Open(const std::wstring& name, int context) {m_con.Open(name, L"-ws", context, this, &m_dispatcher);}
    void Close() {if (m_con.IsOpen()) m_con.Close();}
    void SubscribeMessage(ts::TypeId typeId, const ts::ChannelId& ch, bool includeSubclasses) {m_con.SubscribeMessage(typeId, ch, includeSubclasses, this);}
    void SendMessage(const sd::MessagePtr msg, const ts::ChannelId& ch) {m_con.Send(msg, ch, this);}
    void UnsubscribeMessage(ts::TypeId typeId, const ts::ChannelId& ch, bool includeSubclasses) {m_con.UnsubscribeMessage(typeId, ch, includeSubclasses, this);}

private:
    sd::Connection m_con;
    Safir::Utilities::AsioDispatcher m_dispatcher;
    boost::function<void(const std::string&)> m_wsSend;
    boost::function<void()> m_onStopOrder;

    //DOB events
    //-----------------
    //StopHandler interface
    void OnStopOrder();

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

    //Requestor interface
    void OnResponse(const sd::ResponseProxy responseProxy);
    void OnNotRequestOverflow();

    //MessageSender interface
    void OnNotMessageOverflow();

    //MessageSubscriber interface
    void OnMessage(const sd::MessageProxy messageProxy);

    //ServiceHandler interface
    void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy, Safir::Dob::ResponseSenderPtr responseSender);

    /**
      Open(name, context)
      Close()

      RegHandler
      RegHandlerInj
      RegHandlerPend
      UnregHandler

      SubscribeMessage(type, channel, includeSubclasses)
      UnsubMessage
      SubEnt
      UnsubEnt

      SubReg
      UnsubReg

      Request crud
      SendMessage(message, channel)

      SetAll
      SetChanges
      Delete
      DeleteAll
      Read

      //----------
      Open
      Close

      RegHandler
      UnregHandler

      Subscribe
      Unsubscribe

      Request
      SendMessage

      SetEntity
      ReadEntity

      ReadParameter


      */
};
