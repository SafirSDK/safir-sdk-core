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

#include <functional>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include <boost/asio.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/beast/http.hpp>

class DobConnectionRegistry;
class DobConnection;

// Start an HTTP REST session for an already-accepted socket and a pre-read HTTP request.
// The request was read at the dispatch level (ApiServer) to determine it is not a
// WebSocket upgrade.
void StartRestSession(
    boost::asio::ip::tcp::socket socket,
    boost::beast::http::request<boost::beast::http::string_body> request,
    std::function<std::pair<std::shared_ptr<boost::asio::io_context::strand>,
                            std::shared_ptr<DobConnection>>(const std::string&)> getDobConnectionFunc,
    std::function<std::vector<std::string>()> getAllConnectionNamesFunc);
