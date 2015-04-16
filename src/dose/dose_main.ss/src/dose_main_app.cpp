/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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

#include "dose_main_app.h"
#include <Safir/Dob/Internal/ControlConfig.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/Initialize.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/CrashReporter.h>
#include <boost/bind.hpp>
#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    namespace //anonymous namespace
    {
        void DumpFunc(const char* const dumpPath)
        {
            std::wostringstream ostr;
            SEND_SYSTEM_LOG(Alert,
                            << "dose_main has generated a dump to:\n"
                            << dumpPath << "\n"
                            << "Please send this file to your nearest Dob developer, along with\n"
                            << "relevant information about what version of Safir SDK you are using");
        }
    }

    DoseApp::DoseApp(boost::asio::io_service& ioService):
        m_ioService(ioService),
        m_strand(ioService),
        m_wcoutStrand(ioService),
        m_work(new boost::asio::io_service::work(ioService)),
        m_distribution(),
        m_cmdReceiver(ioService,
                      m_strand.wrap([this](const std::string& nodeName,
                                           int64_t nodeId,
                                           int64_t nodeTypeId,
                                           const std::string& dataAddress)
                                    {
                                        Start(nodeName,
                                              nodeId,
                                              nodeTypeId,
                                              dataAddress);
                                    }),
                      m_strand.wrap([this](const std::string& nodeName,
                                           int64_t nodeId,
                                           int64_t nodeTypeId,
                                           const std::string& dataAddress)
                                    {
                                        InjectNode(nodeName,
                                                   nodeId,
                                                   nodeTypeId,
                                                   dataAddress);
                                    }),
                      m_strand.wrap([this](int64_t nodeId, int64_t nodeTypeId)
                                    {
                                        ExcludeNode(nodeId, nodeTypeId);
                                    }),
                      m_strand.wrap([this]()
                                    {
                                        Stop();
                                    })),


        m_signalSet(ioService)
    {
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        m_signalSet.add(SIGABRT);
        m_signalSet.add(SIGBREAK);
        m_signalSet.add(SIGINT);
        m_signalSet.add(SIGTERM);
#elif defined(linux) || defined(__linux) || defined(__linux__)
        m_signalSet.add(SIGQUIT);
        m_signalSet.add(SIGINT);
        m_signalSet.add(SIGTERM);
#endif

        m_signalSet.async_wait(m_strand.wrap([this](const boost::system::error_code& error,
                                                    const int signalNumber)
                                            {
                                                HandleSignal(error, signalNumber);
                                            }));

        Safir::Utilities::CrashReporter::RegisterCallback(DumpFunc);
        Safir::Utilities::CrashReporter::Start();

        // Start reception of commands from Control
        m_cmdReceiver.Start();
    }

    DoseApp::~DoseApp()
    {
        Stop();
    }

    void DoseApp::Stop()
    {
        const bool wasStopped = m_stopped.exchange(true);
        if (!wasStopped)
        {
            m_cmdReceiver.Stop();
            m_nodeInfoHandler->Stop();
            m_lockMonitor->Stop();

            m_connectionHandler->Stop();

            m_distribution->Stop();

            m_poolHandler->Stop();

            m_pendingRegistrationHandler->Stop();

            m_signalSet.cancel();

            m_memoryMonitor->Stop();

            m_work.reset();
        }
    }


    void DoseApp::Start(const std::string& nodeName,
                        int64_t nodeId,
                        int64_t nodeTypeId,
                        const std::string& dataAddress)
    {
        m_nodeId = nodeId;

        m_memoryMonitor.reset(new MemoryMonitor(m_ioService));

        m_distribution.reset(new Distribution(m_ioService,
                                              nodeName,
                                              nodeId,
                                              nodeTypeId,
                                              dataAddress));

        InitializeDoseInternalFromDoseMain(nodeId);

        m_lockMonitor.reset(new LockMonitor());

        m_messageHandler.reset(new MessageHandler(*m_distribution));

        m_requestHandler.reset(new RequestHandler(m_strand.get_io_service(),
                                                  *m_distribution));

        m_pendingRegistrationHandler.reset(new PendingRegistrationHandler(m_ioService,
                                                                          *m_distribution));

        m_poolHandler.reset(new PoolHandler(m_ioService,
                                            *m_distribution,
                                            [this](int64_t tid){m_pendingRegistrationHandler->CheckForPending(tid);},
                                            [this](const std::string& str){LogStatus(str);}));

        m_connectionHandler.reset(new ConnectionHandler(m_ioService,
                                                        *m_distribution,
                                                        [this](const ConnectionPtr& connection, bool disconnecting){OnAppEvent(connection, disconnecting);}));

        m_nodeInfoHandler.reset(new NodeInfoHandler(m_ioService, *m_distribution));
    }

    void DoseApp::InjectNode(const std::string& nodeName,
                             int64_t nodeId,
                             int64_t nodeTypeId,
                             const std::string& dataAddress)
    {
        lllog(1) << "DOSE_MAIN: InjectNode cmd received."<<
                  " NodeName=" << nodeName.c_str() <<
                  " NodeId=" <<  nodeId <<
                  " NodeTypeId=" << nodeTypeId <<
                  " DataAddress=" << dataAddress.c_str() << std::endl;

        ENSURE (m_distribution != nullptr, << "InjectNode cmd received before StartDoseMain cmd!");

        if (m_distribution->GetCommunication().Id() == nodeId)
        {
            lllog(1) << "DOSE_MAIN: Own node injected, starting distribution components!" << std::endl;

            // Own node has been included in the system state, now its time to start
            // the distribution mechanism.
            m_distribution->Start();
            m_poolHandler->Start();

            LogStatus("dose_main running...");
        }
        else
        {
            lllog(1) << "DOSE_MAIN: Injecting Node."<<
                      " NodeName=" << nodeName.c_str() <<
                      " NodeId=" <<  nodeId <<
                      " NodeTypeId=" << nodeTypeId <<
                      " DataAddress=" << dataAddress.c_str() << std::endl;

            m_distribution->InjectNode(nodeName,
                                       nodeId,
                                       nodeTypeId,
                                       dataAddress);
        }
    }

    void DoseApp::ExcludeNode(int64_t nodeId, int64_t nodeTypeId)
    {
        lllog(1) << "DOSE_MAIN: ExcludeNode cmd received."<<
                    " NodeId=" <<  nodeId <<
                    " NodeTypeId=" << nodeTypeId << std::endl;

        m_distribution->ExcludeNode(nodeId, nodeTypeId);
    }


    void DoseApp::HandleSignal(const boost::system::error_code& error,
                               const int signalNumber)
    {
        if (error)
        {
            if (error == boost::asio::error::operation_aborted)
            {
                // dose_main got stopped via a command from Control, do nothing
                return;
            }
            else
            {
               SEND_SYSTEM_LOG(Error,
                                << "DOSE_MAIN: Got a signals error: " << error);
            }
        }

        LogStatus("dose_main got signal " + boost::lexical_cast<std::string>(signalNumber) + ", shutting down.");

        Stop();
    }

    void DoseApp::OnAppEvent(const ConnectionPtr & connection, bool disconnecting)
    {
        int recLevel=0;
        HandleAppEventHelper(connection, recLevel);
        if (disconnecting)
        {
            m_pendingRegistrationHandler->RemovePendingRegistrations(connection->Id());
            m_requestHandler->HandleDisconnect(connection);
        }
    }

    void DoseApp::HandleAppEventHelper(const ConnectionPtr & connection, int & recursionLevel)
    {
        lllout << "HandleAppEventHelper for connection " << connection->NameWithCounter() << ", id = " << connection->Id() << std::endl;

        //Handle queued requests
        m_requestHandler->HandleRequests(connection);

        //Send messages
        m_messageHandler->DistributeMessages(connection);

        //Handle pending registrations
        m_pendingRegistrationHandler->CheckForNewOrRemovedPendingRegistration(connection);
    }

    void DoseApp::LogStatus(const std::string& str)
    {
        lllog(1) << str.c_str() << std::endl;
        m_wcoutStrand.dispatch([str]
                               {
                                   std::wcout << str.c_str() << std::endl;
                               });
    }

}
}
}
