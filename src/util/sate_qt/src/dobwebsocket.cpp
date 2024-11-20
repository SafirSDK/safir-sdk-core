/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
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
#include "dobwebsocket.h"
#include <QtConcurrent/QtConcurrent>
#include <QDebug>

namespace
{
	QUrl ToUrl(const QString& address, int port)
	{
		QString url = (address.startsWith("ws") ? address : QString("ws://%1").arg(address)) + ":" + QString::number(port);
		return QUrl(url);
	}
}

DobWebSocket::DobWebSocket(const QString& address, int port)
	: m_webSocket()
	, m_url(ToUrl(address, port))
{
    connect(&m_webSocket, &QWebSocket::connected, this, &DobWebSocket::WsConnected);
    connect(&m_webSocket, &QWebSocket::disconnected, this, &DobWebSocket::WsDisconnected);
}

void DobWebSocket::WsConnected()
{
    emit DobInterface::Info("Connected to DOB websocket on " + m_url.toString());

}

void DobWebSocket::WsDisconnected()
{
    emit DobInterface::Info("Disconnected from DOB websocket on " + m_url.toString());
}

void DobWebSocket::Open(const QString& name, int context)
{
    m_webSocket.open(m_url);
    // Connect to DOB in another thread. Signals the ConnectedToDobSignal when done.
    /*auto dummy = QtConcurrent::run([this, name, context]
    {
        int instancePart = 0;
        while (true)
        {
            try
            {
                m_dobConnection.Open(name.toStdWString(), std::to_wstring(instancePart), context, this, this);
                break;
            }
            catch (const Safir::Dob::NotOpenException&)
            {
                ++instancePart;
            }
        }

        emit DobInterface::ConnectedToDob(Str(Safir::Dob::ConnectionAspectMisc(this->m_dobConnection).GetConnectionName()));
        emit DobInterface::Info("<b>Connected to DOB!</b>");
    });*/
}

void DobWebSocket::Close()
{
}

void DobWebSocket::SubscribeMessage(int64_t typeId, const sdt::ChannelId& channel, bool includeSubclasses)
{
}

void DobWebSocket::UnsubscribeMessage(int64_t typeId)
{
}

void DobWebSocket::SubscribeEntity(int64_t typeId, const Safir::Dob::Typesystem::InstanceId& instance, bool includeSubclasses)
{
}

void DobWebSocket::UnsubscribeEntity(int64_t typeId)
{
}

void DobWebSocket::SubscribeRegistrations(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler, bool includeSubclasses)
{
}

void DobWebSocket::UnsubscribeRegistrations(int64_t typeId)
{
}

void DobWebSocket::RegisterEntityHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending, bool injection)
{
}

void DobWebSocket::RegisterServiceHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler, bool pending)
{
}

void DobWebSocket::Unregister(int64_t typeId)
{
}

void DobWebSocket::SendMessage(const Safir::Dob::MessagePtr& message, const Safir::Dob::Typesystem::ChannelId& channel)
{
}

void DobWebSocket::SendServiceRequest(const Safir::Dob::ServicePtr& request, const Safir::Dob::Typesystem::HandlerId& handler)
{
}

void DobWebSocket::CreateRequest(const Safir::Dob::EntityPtr& entity, const Safir::Dob::Typesystem::InstanceId& instance, const Safir::Dob::Typesystem::HandlerId& handler)
{
}

void DobWebSocket::UpdateRequest(const Safir::Dob::EntityPtr& entity, const Safir::Dob::Typesystem::InstanceId& instance)
{
}

void DobWebSocket::DeleteRequest(const Safir::Dob::Typesystem::EntityId& entityId)
{
}

void DobWebSocket::SetChanges(const Safir::Dob::EntityPtr& entity, const Safir::Dob::Typesystem::InstanceId& instance, const Safir::Dob::Typesystem::HandlerId& handler)
{
}

void DobWebSocket::SetAll(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler)
{
}

void DobWebSocket::Delete(const Safir::Dob::Typesystem::EntityId& entityId, const Safir::Dob::Typesystem::HandlerId& handler)
{
}

void DobWebSocket::DeleteAll(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler)
{
}
