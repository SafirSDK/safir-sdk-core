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
#pragma once

#include <memory>
#include <string>
#include <mutex>
#include <unordered_map>
#include "DobConnection.h"

class DobConnectionRegistry
{
public:
    void InsertConnection(const std::string& connectionName,
                          std::shared_ptr<DobConnection> dobConnection,
                          std::shared_ptr<boost::asio::io_context::strand> strand);
    std::pair<std::shared_ptr<boost::asio::io_context::strand>, std::shared_ptr<DobConnection>> GetConnection(const std::string& connectionName) const;
    void RemoveConnection(const std::string& connectionName);
    std::vector<std::string> GetAllConnectionNames() const;

private:
    mutable std::mutex m_lock;
    std::unordered_map<std::string, std::pair<std::shared_ptr<boost::asio::io_context::strand>, std::shared_ptr<DobConnection>>> m_connections;
};
