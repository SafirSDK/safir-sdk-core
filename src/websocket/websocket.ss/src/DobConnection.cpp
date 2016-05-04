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
#include "DobConnection.h"

DobConnection::DobConnection::DobConnection(boost::asio::io_service &ioService, boost::function<void(const std::string&)> send, boost::function<void()> onStopOrder)
    :m_con()
    ,m_dispatcher(m_con, ioService)
    ,m_wsSend(send)
    ,m_onStopOrder(onStopOrder)
{

}

//------------------------------------------------------
// DOB events
//------------------------------------------------------
//StopHandler interface
void DobConnection::OnStopOrder()
{
    //TODO: create response
    m_onStopOrder();
}

// EntityHandler interface
void DobConnection::OnCreateRequest(const sd::EntityRequestProxy entityRequestProxy, sd::ResponseSenderPtr responseSender)
{

}
void DobConnection::OnUpdateRequest(const sd::EntityRequestProxy entityRequestProxy, sd::ResponseSenderPtr responseSender)
{

}
void DobConnection::OnDeleteRequest(const sd::EntityRequestProxy entityRequestProxy, sd::ResponseSenderPtr responseSender)
{

}
void DobConnection::OnRevokedRegistration(const sd::Typesystem::TypeId typeId, const sd::Typesystem::HandlerId &handlerId)
{

}
void DobConnection::OnCompletedRegistration(const sd::Typesystem::TypeId typeId, const sd::Typesystem::HandlerId& handlerId)
{

}

//Injection interface
void DobConnection::OnInjectedNewEntity(const sd::InjectedEntityProxy injectedEntityProxy)
{

}
void DobConnection::OnInjectedUpdatedEntity(const sd::InjectedEntityProxy injectedEntityProxy)
{

}
void DobConnection::OnInjectedDeletedEntity(const sd::InjectedEntityProxy injectedEntityProxy)
{

}
void DobConnection::OnInitialInjectionsDone(const sd::Typesystem::TypeId typeId, const sd::Typesystem::HandlerId& handlerId)
{

}

//EntitySubscriber interface
void DobConnection::OnNewEntity(const sd::EntityProxy entityProxy)
{

}
void DobConnection::OnUpdatedEntity(const sd::EntityProxy entityProxy)
{

}
void DobConnection::OnDeletedEntity(const sd::EntityProxy entityProxy, const bool)
{

}

//Requestor interface
void DobConnection::OnResponse(const sd::ResponseProxy responseProxy)
{

}
void DobConnection::OnNotRequestOverflow()
{

}

//MessageSender interface
void DobConnection::OnNotMessageOverflow()
{

}

//MessageSubscriber interface
void DobConnection::OnMessage(const sd::MessageProxy messageProxy)
{
    auto json=ts::Serialization::ToJson(messageProxy.GetBlob());
    m_wsSend(ts::Utilities::ToUtf8(json));
}

//ServiceHandler interface
void DobConnection::OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{

}
