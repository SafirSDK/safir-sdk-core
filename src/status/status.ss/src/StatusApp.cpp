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
#include "StatusApp.h"


namespace Safir
{
namespace Control
{

StatusApp::StatusApp()
    : m_ioService()
    , m_dispatcher(m_connection, m_ioService)
    , m_statusEntityHandler(m_ioService)
    , m_commandRequestHandler(m_ioService)
{
    m_connection.Open(L"safir_control_status",  // Note the name. We want this to be handled as a normal connection.
                          L"main",
                          0,
                          this,
                          &m_dispatcher);


    m_statusEntityHandler.Start();
    m_commandRequestHandler.Start();
}

void StatusApp::OnStopOrder()
{
    m_commandRequestHandler.Stop();
    m_statusEntityHandler.Stop();

    m_ioService.stop();
}


void StatusApp::Run()
{
    boost::asio::io_service::work keepRunning(m_ioService);
    m_ioService.run();

    m_connection.Close();
}

StatusApp::~StatusApp()
{

}

}
}
