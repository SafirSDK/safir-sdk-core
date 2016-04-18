/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
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

#include "SystemStateHandler.h"
#include "StopHandler.h"
#include "TerminateHandler.h"
#include "IncarnationBlackListHandler.h"
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Dob/Internal/ControlInfo.h>
#include <Safir/Dob/Internal/DoseMainCmd.h>
#include <Safir/Dob/Internal/ControlConfig.h>
#include <boost/atomic.hpp>
#include <boost/aligned_storage.hpp>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267 4251)
#endif

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#include "boost/process.hpp"
#include "boost/process/mitigate.hpp"

#if defined _MSC_VER
#  pragma warning (pop)
#endif

#include <string>

namespace Com = Safir::Dob::Internal::Com;
namespace SP = Safir::Dob::Internal::SP;
namespace Control = Safir::Dob::Internal::Control;

class ControlApp : private boost::noncopyable
{
public:
    ControlApp(boost::asio::io_service&         ioService,
               const boost::filesystem::path&   doseMainPath,
               const int64_t                    id,
               const bool                       ignoreControlCmd);

private:
    void Start();

    //first is controlAddress, second is dataAddress
    std::pair<Com::ResolvedAddress,Com::ResolvedAddress> ResolveAddresses();

    void StopDoseMain();

    void StopControl();

    void StopThisNode();

    void Shutdown();

    void Reboot();

    void SendControlInfo();

    boost::asio::io_service&                    m_ioService;
    bool                                        m_stopped;
    const boost::chrono::steady_clock::time_point m_resolutionStartTime;
    boost::asio::io_service::strand             m_strand;
    std::unique_ptr<TerminateHandler>           m_terminateHandler;
    int64_t                                     m_nodeId;
    const boost::filesystem::path               m_doseMainPath;
    const bool                                  m_ignoreControlCmd;
    boost::asio::steady_timer                   m_startTimer;
    boost::asio::steady_timer                   m_terminationTimer;
    const Control::Config                       m_conf;
    Control::IncarnationBlacklistHandler        m_incarnationBlackListHandler;
    bool                                        m_controlInfoReceiverReady;
    bool                                        m_controlStopped;
    bool                                        m_doseMainRunning;
    bool                                        m_requiredForStart;

    std::unique_ptr<boost::asio::io_service::work>  m_work;
    std::unique_ptr<Com::Communication>             m_communication;
    std::unique_ptr<SP::SystemPicture>              m_sp;
    std::unique_ptr<Control::StopHandler>           m_stopHandler;
    std::unique_ptr<Control::ControlInfoSender>     m_controlInfoSender;
    std::unique_ptr<Control::DoseMainCmdSender>     m_doseMainCmdSender;
    std::unique_ptr<Control::SystemStateHandler>    m_stateHandler;
    std::unique_ptr<boost::process::child>          m_doseMain;

    // 64 bit atomic needs to be aligned on 64 bit boundary even on 32 bit systems,
    // so we need to use alignment magic.
    typedef boost::aligned_storage<sizeof(boost::atomic<int64_t>),
                                   sizeof(boost::atomic<int64_t>)>::type AlignedStorage;
    std::unique_ptr<AlignedStorage>     m_incarnationIdStorage;
    boost::atomic<int64_t>&             m_incarnationId;

#if defined(linux) || defined(__linux) || defined(__linux__)
    boost::asio::signal_set m_sigchldSet;
    void SetSigchldHandler();
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    boost::asio::windows::object_handle m_handle;
#endif

};

