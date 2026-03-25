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
#include <utility>
#include <boost/asio.hpp>
#include <boost/asio/ip/tcp.hpp>

class DobConnectionRegistry;
class DobConnection;

class RestServer
{
public:
    RestServer(boost::asio::io_context& io,
               const std::shared_ptr<DobConnectionRegistry>& dobConnectionRegistry);

    void Run();
    void Terminate();

private:
    std::shared_ptr<DobConnectionRegistry> m_dobConnectionRegistry;
    boost::asio::ip::tcp::acceptor m_acceptor;
    bool m_isRunning;
    bool m_isTerminating;

    void StartAccept();

    RestServer(const RestServer&) = delete;
    RestServer& operator=(const RestServer&) = delete;
};
