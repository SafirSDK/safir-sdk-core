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
#include "DobConnectionRegistry.h"

void DobConnectionRegistry::InsertConnection(const std::string& connectionName,
                                             std::shared_ptr<DobConnection> dobConnection,
                                             std::shared_ptr<boost::asio::io_context::strand> strand)
{
    std::lock_guard<std::mutex> lock(m_lock);
    m_connections[connectionName] = std::make_pair(strand, std::move(dobConnection));
}

std::pair<std::shared_ptr<boost::asio::io_context::strand>, std::shared_ptr<DobConnection>> DobConnectionRegistry::GetConnection(const std::string& connectionName) const
{
    std::lock_guard<std::mutex> lock(m_lock);

    const auto it = m_connections.find(connectionName);
    if (it == m_connections.end())
    {
        return {};
    }

    return it->second;
}

void DobConnectionRegistry::RemoveConnection(const std::string& connectionName)
{
    std::lock_guard<std::mutex> lock(m_lock);
    m_connections.erase(connectionName);
}
