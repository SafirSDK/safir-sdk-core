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
//#include <Safir/Dob/Internal/ControlConfig.h>
//#include <Safir/Dob/Internal/Connections.h>
//#include <Safir/Dob/Internal/InternalDefs.h>
//#include <Safir/Dob/Internal/Initialize.h>
//#include <Safir/Dob/OverflowException.h>
//#include <Safir/Dob/ThisNodeParameters.h>
//#include <Safir/Utilities/Internal/LowLevelLogger.h>
//#include <Safir/Utilities/Internal/SystemLog.h>
//#include <Safir/Utilities/CrashReporter.h>
//#include <boost/bind.hpp>
//#include <iostream>

namespace Safir
{
namespace Control
{

StatusApp::StatusApp(boost::asio::io_service& ioService)
    : m_ioService(ioService)
    , m_dispatcher(m_connection, ioService)
    , m_statusEntityHandler()
{
    m_connection.Open(L"safir_control_status",  // Note the name. We want this to be handled as a normal connection.
                          L"main",
                          0,
                          nullptr,
                          &m_dispatcher);


    m_statusEntityHandler.Start();
}

StatusApp::~StatusApp()
{
//    Stop();
}

void StatusApp::Stop()
{

}

//    void DoseMainApp::ExcludeNode(int64_t nodeId, int64_t nodeTypeId)
//    {
//        lllog(1) << "DOSE_MAIN: ExcludeNode cmd received."<<
//                    " NodeId=" <<  nodeId <<
//                    " NodeTypeId=" << nodeTypeId << std::endl;

//        m_distribution->ExcludeNode(nodeId, nodeTypeId);
//    }

//    void DoseMainApp::OnAppEvent(const ConnectionPtr & connection, bool disconnecting)
//    {
//        HandleAppEventHelper(connection);
//        if (disconnecting)
//        {
//            m_pendingRegistrationHandler->RemovePendingRegistrations(connection->Id());
//            m_requestHandler->HandleDisconnect(connection);
//        }
//    }

//    void DoseMainApp::HandleAppEventHelper(const ConnectionPtr & connection)
//    {
//        lllout << "HandleAppEventHelper for connection " << connection->NameWithCounter()
//               << ", id = " << connection->Id() << std::endl;

//        //Handle queued requests
//        m_requestHandler->HandleRequests(connection);

//        //Send messages
//        m_messageHandler->DistributeMessages(connection);

//        //Handle pending registrations
//        m_pendingRegistrationHandler->CheckForNewOrRemovedPendingRegistration(connection);
//    }

//    void DoseMainApp::LogStatus(const std::string& str)
//    {
//        lllog(1) << str.c_str() << std::endl;
//        m_wcoutStrand.dispatch([str]
//                               {
//                                   std::wcout << str.c_str() << std::endl;
//                               });
//    }
}
}
