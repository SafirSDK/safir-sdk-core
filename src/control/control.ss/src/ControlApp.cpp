/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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

#include "ControlApp.h"
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Utilities/CrashReporter.h>
#include <Safir/Utilities/Internal/Id.h>
#include <iostream>
#include <map>
#include <boost/regex.hpp>
#include <atomic>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267 4251)
#endif

#include <boost/filesystem.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

ControlApp::ControlApp(boost::asio::io_service& ioService,
                       const std::string&       doseMainPath,
                       const boost::int64_t     id)
    : m_ioService(ioService)
    , m_signalSet(ioService)
    , m_strand(ioService)
    , m_terminationTimer(ioService)
    , m_ctrlStopped(false)
    , m_doseMainRunning(false)
#if defined(linux) || defined(__linux) || defined(__linux__)
    , m_sigchldSet(ioService, SIGCHLD)
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    , m_handle(ioService)
#endif
{
    // Make some work to stop io_service from exiting.
    m_work = Safir::make_unique<boost::asio::io_service::work>(ioService);

    // Initiate Communication

    std::vector<Com::NodeTypeDefinition> commNodeTypes;

    for (const auto& nt: m_conf.nodeTypesParam)
    {
        commNodeTypes.push_back({nt.id,
                                 nt.name,
                                 nt.multicastAddressControl,
                                 nt.multicastAddressData,
                                 nt.heartbeatInterval,
                                 nt.retryTimeout,
                                 nt.maxLostHeartbeats});
    }

    m_communication.reset(new Com::Communication(Com::controlModeTag,
                                                 m_ioService,
                                                 m_conf.thisNodeParam.name,
                                                 id,
                                                 m_conf.thisNodeParam.nodeTypeId,
                                                 m_conf.thisNodeParam.controlAddress,
                                                 m_conf.thisNodeParam.dataAddress,
                                                 commNodeTypes));

    if (!m_conf.thisNodeParam.seeds.empty())
    {
        m_communication->InjectSeeds(m_conf.thisNodeParam.seeds);
    }

    // Initiate SystemPicture

    std::map<boost::int64_t, SP::NodeType> spNodeTypes;

    for (const auto& nt: m_conf.nodeTypesParam)
    {
        spNodeTypes.insert(std::make_pair(nt.id,
                                          SP::NodeType(nt.id,
                                                       nt.name,
                                                       nt.isLight,
                                                       boost::chrono::milliseconds(nt.heartbeatInterval),
                                                       nt.maxLostHeartbeats,
                                                       boost::chrono::milliseconds(nt.retryTimeout))));
    }


    m_sp.reset(new SP::SystemPicture(SP::master_tag,
                                     ioService,
                                     *m_communication,
                                     m_conf.thisNodeParam.name,
                                     id,
                                     m_conf.thisNodeParam.nodeTypeId,
                                     std::move(spNodeTypes),
                                     [](const int64_t /*incarnationId*/)
                                     {return true;})); //this just says that all incarnation ids are valid.

    m_doseMainCmdSender.reset(new Control::DoseMainCmdSender
                              (ioService,
                               // This is what we do when dose_main is ready to receive commands
                               [this, id]()
    {
        m_doseMainRunning = true;

        m_doseMainCmdSender->StartDoseMain(m_conf.thisNodeParam.name,
                                           id,
                                           m_conf.thisNodeParam.nodeTypeId,
                                           m_conf.thisNodeParam.dataAddress);

        m_sp->StartStateSubscription
                ([this](const SP::SystemState& newState)
        {
            m_stateHandler->SetNewState(newState);
        });

        m_communication->Start();
    })
                              );

    m_stateHandler.reset(new Control::SystemStateHandler
                         (id,

    // Node included callback
    [this](const Control::Node& node)
    {
        m_doseMainCmdSender->InjectNode(node.name,
                                        node.nodeId,
                                        node.nodeTypeId,
                                        node.dataAddress);
    },

    // Node down callback
    [this](const int64_t nodeId, const int64_t nodeTypeId)
    {
        m_doseMainCmdSender->ExcludeNode(nodeId, nodeTypeId);
    }));

#if defined(linux) || defined(__linux) || defined(__linux__)
    m_sigchldSet.async_wait(m_strand.wrap([this]
                                          (const boost::system::error_code& error, int signalNumber)
    {
        lllog(1) << "CTRL: Got signal " << signalNumber << " ... dose_main has exited" << std::endl;
        if (error)
        {
            SEND_SYSTEM_LOG(Error,
                            << "CTRL: Got a signals error: " << error);
        }

        m_doseMainRunning = false;

        // dose_main has exited, we can stop our timer that will slay dose_main
        m_terminationTimer.cancel();

        int statusCode;
        ::wait(&statusCode);

        if (WIFEXITED(statusCode))
        {
            auto status = WEXITSTATUS(statusCode);

            if (status != 0)
            {
                SEND_SYSTEM_LOG(Critical, << "CTRL: dose_main has exited with status code "  << status);
                std::wcout << "CTRL: dose_main has exited with status code "  << status  << std::endl;
            }
        }
        else
        {
            SEND_SYSTEM_LOG(Critical, << "CTRL: dose_main has exited, no exit status code provided");
            std::wcout << "CTRL: dose_main has exited, no exit status code provided" << std::endl;
        }

        if (!m_ctrlStopped)
        {
            StopControl();
        }
    }
    ));
#endif

    // Locate and start dose_main
    namespace fs = boost::filesystem;

    fs::path path(doseMainPath);

    if (fs::exists(path))
    {
        if (fs::is_directory(path) || !fs::is_regular_file(path))
        {
            std::ostringstream os;
            os << "CTRL: " << doseMainPath << " is a directory or a non regular file!" << std::endl;
            SEND_SYSTEM_LOG(Error, << os.str().c_str());
            throw std::logic_error(os.str().c_str());
        }
    }
    else
    {
        std::ostringstream os;
        os << "CTRL: Can't find " << doseMainPath << std::endl;
        SEND_SYSTEM_LOG(Error, << os.str().c_str());
        throw std::logic_error(os.str().c_str());
    }

    boost::system::error_code ec;

    m_doseMain.reset(new boost::process::child(boost::process::execute
                                                (boost::process::initializers::run_exe(path),
                                                 boost::process::initializers::set_on_error(ec),
                                                 boost::process::initializers::inherit_env()
#if defined(linux) || defined(__linux) || defined(__linux__)
                                                 ,boost::process::initializers::notify_io_service(ioService)
#endif
                                               )));

    if (ec)
    {
        SEND_SYSTEM_LOG(Error,
                        << "CTRL: Error run_exe: " << ec);
    }


#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    m_handle.assign(m_doseMain->process_handle());

    m_handle.async_wait(
                [this](const boost::system::error_code&)
    {
        lllog(1) << "CTRL: dose_main has exited" << std::endl;

        DWORD statusCode;
        ::GetExitCodeProcess(m_handle.native(), &statusCode);

        m_doseMainRunning = false;

        // dose_main has exited, we can stop our timer that will slay dose_main
        m_terminationTimer.cancel();

        if (statusCode != 0)
        {
            // dose_main has exited unexpectedly

            SEND_SYSTEM_LOG(Critical, << "CTRL: dose_main has exited with status code "  << statusCode);
            std::wcout << "CTRL: dose_main has exited with status code "  << statusCode  << std::endl;
        }

        if (!m_ctrlStopped)
        {
            StopControl();
        }
    }
    );
#endif

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

    m_signalSet.async_wait(m_strand.wrap([this]
                                           (const boost::system::error_code& error,
                                            const int signalNumber)
    {
        lllog(1) << "CTRL: Got signal " << signalNumber << " ... stop sequence initiated." << std::endl;
        if (error)
        {
            SEND_SYSTEM_LOG(Error,
                            << "CTRL: Got a signals error: " << error);
        }

        if (m_doseMainRunning)
        {
            StopDoseMain();
        }

        if (!m_ctrlStopped)
        {
            StopControl();
        }
    }
    ));

    m_doseMainCmdSender->Start();
}

ControlApp::~ControlApp()
{

}

void ControlApp::StopDoseMain()
{
    // Set up a timer that will kill dose_main the hard way if it doesn't stop within a reasonable time.
    m_terminationTimer.expires_from_now(boost::chrono::seconds(10));

    m_terminationTimer.async_wait([this]
                                  (const boost::system::error_code& error)
                                  {
                                      if (error == boost::asio::error::operation_aborted)
                                      {
                                          return;
                                      }

                                      SEND_SYSTEM_LOG(Critical,
                                                      << "CTRL: Can't stop dose_main in a controlled fashion"
                                                      << "... killing it!");

                                      // Kill dose_main the hard way
                                      boost::system::error_code ec;
                                      boost::process::terminate(*m_doseMain, ec);
                                      // We don't care about the error code from terminate. dose_main might
                                      // have exited by itself (which is good) and that will give an error.
                                  });

    // Send stop order to dose_main
    m_doseMainCmdSender->StopDoseMain();
}

void ControlApp::StopControl()
{
    m_sp->Stop();
    m_communication->Stop();
    m_doseMainCmdSender->Stop();
    m_signalSet.cancel();
    m_work.reset();
}


