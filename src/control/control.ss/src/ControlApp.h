/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safir.sourceforge.net)
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
#include "IncarnationBlacklistHandler.h"
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Dob/Internal/DoseMainCmd.h>
#include <Safir/Dob/Internal/ControlConfig.h>

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
               const boost::int64_t             id);

    ~ControlApp();

private:

    void StopDoseMain();

    void StopControl();

    boost::asio::io_service&                    m_ioService;
    boost::asio::signal_set                     m_signalSet;
    boost::asio::io_service::strand             m_strand;
    boost::asio::steady_timer                   m_terminationTimer;
    Control::Config                             m_conf;
    Control::IncarnationBlacklistHandler        m_incarnationBlackListHandler;
    bool                                        m_ctrlStopped;
    bool                                        m_doseMainRunning;

    std::unique_ptr<boost::asio::io_service::work>  m_work;
    std::unique_ptr<Com::Communication>             m_communication;
    std::unique_ptr<SP::SystemPicture>              m_sp;
    std::unique_ptr<Control::DoseMainCmdSender>     m_doseMainCmdSender;
    std::unique_ptr<Control::SystemStateHandler>    m_stateHandler;
    std::unique_ptr<boost::process::child>          m_doseMain;

#if defined(linux) || defined(__linux) || defined(__linux__)
    boost::asio::signal_set m_sigchldSet;
    void SetSigchldHandler();
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    boost::asio::windows::object_handle m_handle;
#endif

};

