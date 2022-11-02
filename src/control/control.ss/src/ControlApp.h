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
#include <atomic>
#include <boost/aligned_storage.hpp>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267 4251)
#endif

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/process/child.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

#include <string>

namespace Com = Safir::Dob::Internal::Com;
namespace SP = Safir::Dob::Internal::SP;
namespace Control = Safir::Dob::Internal::Control;

class ControlApp
{
public:
    ControlApp(boost::asio::io_context&         io,
               const boost::filesystem::path&   doseMainPath,
               const int64_t                    id,
               const bool                       ignoreControlCmd);

    ControlApp(const ControlApp&) = delete;
    const ControlApp& operator=(const ControlApp&) = delete;

    ~ControlApp();

    void Stop() {StopThisNode();}
private:
    void LogStatus(const std::string& str);

    void Start();

    //first is controlAddress, second is dataAddress
    std::pair<Com::ResolvedAddress,Com::ResolvedAddress> ResolveAddresses();

    void StopDoseMain();

    void StopControl();

    void StopThisNode();

    void Shutdown();

    void Reboot();

    void SendControlInfo();

    void HandleDoseMainExit(const int exitCode, const std::error_code& error);

    boost::asio::io_context&                    m_io;
    boost::asio::executor_work_guard<boost::asio::io_context::executor_type> m_work;
    bool                                        m_stopped;
    const boost::chrono::steady_clock::time_point m_resolutionStartTime;
    boost::asio::io_context::strand             m_strand;
    boost::asio::io_context::strand             m_wcoutStrand;
    std::unique_ptr<TerminateHandler>           m_terminateHandler;
    const int64_t                               m_nodeId;
    const boost::filesystem::path               m_doseMainPath;
    const bool                                  m_ignoreControlCmd;
    boost::asio::steady_timer                   m_startTimer;
    boost::asio::steady_timer                   m_terminationTimer;
    const Control::Config                       m_conf;
    Control::IncarnationBlacklistHandler        m_incarnationBlackListHandler;
    bool                                        m_controlInfoReceiverReady;
    bool                                        m_doseMainRunning;
    bool                                        m_requiredForStart;
    bool                                        m_isLightNode;



    std::unique_ptr<Com::Communication>             m_communication;
    std::unique_ptr<SP::SystemPicture>              m_sp;
    std::unique_ptr<Control::StopHandler>           m_stopHandler;
    std::unique_ptr<Control::ControlInfoSender>     m_controlInfoSender;
    std::unique_ptr<Control::DoseMainCmdSender>     m_doseMainCmdSender;
    std::unique_ptr<Control::SystemStateHandler>    m_stateHandler;
    std::unique_ptr<boost::process::child>          m_doseMain;

    // 64 bit atomic needs to be aligned on 64 bit boundary even on 32 bit systems,
    // so we need to use alignment magic.
    typedef boost::aligned_storage<sizeof(std::atomic<int64_t>),
    sizeof(std::atomic<int64_t>)>::type AlignedStorage;
    std::unique_ptr<AlignedStorage>     m_incarnationIdStorage;
    std::atomic<int64_t>&             m_incarnationId;

    std::set<int64_t> m_lightNodeTypeIds;
    bool IsLightNode(int64_t nodeTypeId) const
    {
        return m_lightNodeTypeIds.find(nodeTypeId) != std::end(m_lightNodeTypeIds);
    }

};
