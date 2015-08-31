/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com/)
*
* Created by: Anders Widén / anders.widen@consoden.se
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

#include <Safir/Dob/Connection.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include "StatusEntityHandler.h"
#include "CommandRequestHandler.h"

#include <boost/asio.hpp>

namespace Safir
{
namespace Control
{

class StatusApp : private boost::noncopyable,
                  public Safir::Dob::StopHandler
{
public:
    explicit StatusApp();

    ~StatusApp();

    void Run();

private:

    virtual void OnStopOrder();

    boost::asio::io_service    m_ioService;

    Safir::Utilities::AsioDispatcher        m_dispatcher;
    Safir::Dob::Connection                  m_connection;
    Safir::Control::StatusEntityHandler     m_statusEntityHandler;
    Safir::Control::CommandRequestHandler   m_commandRequestHandler;

};
}
}

