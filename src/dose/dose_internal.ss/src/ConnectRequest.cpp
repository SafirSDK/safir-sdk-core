/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#include <Safir/Dob/Internal/ConnectRequest.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Internal/Connection.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    ConnectRequest::ConnectRequest():
        m_kind(NotSet)
    {

    }

    void ConnectRequest::Set(connect_tag_t, const std::string & connectionName, const ContextId contextId, const int pid)
    {
        ENSURE (m_kind == NotSet, << "Attempt to call Set(connect_tag_t, " << connectionName.c_str() << ", " << contextId
                << ") when something was already in the connect message");
        m_kind = Connect;
        m_connectionName = connectionName.c_str();
        m_contextId = contextId;
        m_pid = pid;
    }

    void ConnectRequest::GetAndClear(connect_tag_t, std::string & connectionName, ContextId & contextId, int & pid)
    {
        ENSURE (m_kind == Connect, << "Attempt to call GetAndClear(connect_tag_t, ...) when kind was " << m_kind);
        connectionName = m_connectionName.c_str();
        m_connectionName.clear();

        contextId = m_contextId;
        m_contextId = 0;

        pid = m_pid;
        m_pid = 0;

        m_kind = NotSet;
    }

    bool ConnectRequest::IsConnect() const
    {
        ENSURE (m_kind != NotSet, << "IsConnect was called when m_kind was NotSet!");
        return m_kind == Connect;
    }



    ConnectResponse::ConnectResponse():
        m_kind(NotSet)
    {

    }

    void ConnectResponse::Set(connect_tag_t, const ConnectResult result, const ConnectionPtr & connection)
    {
        ENSURE (m_kind == NotSet, << "Attempt to call Set(connect_tag_t, " << result << ", " << connection->Id()
                << ") when something was already in the connect response");

        m_kind = Connect;
        m_result = result;
        m_connection = connection;
    }

    void ConnectResponse::GetAndClear(connect_tag_t, ConnectResult & result, ConnectionPtr & connection)
    {
        ENSURE (m_kind == Connect, << "Attempt to call GetAndClear(connect_tag_t, ...) when kind was " << m_kind);
        result = m_result;
        m_result = Undefined;

        connection = m_connection;
        m_connection = NULL;

        m_kind = NotSet;
    }



}
}
}

