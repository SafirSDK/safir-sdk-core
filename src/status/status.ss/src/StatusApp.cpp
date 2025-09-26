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
    : m_ioContext()
    , m_work(boost::asio::make_work_guard(m_ioContext))
    , m_dispatcher(m_connection, m_ioContext)
    , m_statusEntityHandler(m_ioContext)
    , m_commandRequestHandler(m_ioContext)
    , m_connectionStatsHandler()
    , m_nodeInfoMirrorer()
    , m_tracerStatusHandler()
{
    m_connection.Open(L"safir_control_status",
                      L"main",
                      0,
                      this,
                      &m_dispatcher);


    m_statusEntityHandler.Start();
    m_commandRequestHandler.Start();
    m_connectionStatsHandler.Start();
    m_nodeInfoMirrorer.Start();
    m_tracerStatusHandler.Start();
}

void StatusApp::OnStopOrder()
{
    m_commandRequestHandler.Stop();
    m_statusEntityHandler.Stop();
    m_connectionStatsHandler.Stop();
    m_nodeInfoMirrorer.Stop();
    m_tracerStatusHandler.Stop();

    m_work.reset();
}


void StatusApp::Run()
{
    m_ioContext.run();

    m_connection.Close();
}

StatusApp::~StatusApp()
{

}

}
}
