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

#include <Safir/Dob/Internal/ControlUtilsExportDefs.h>
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>
#include <string>

//forward declaration
namespace boost { namespace asio {
    class io_service;
}}

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4275)
#pragma warning (disable: 4251)
#endif

    enum CommandAction
    {
        STOP,
        SHUTDOWN,
        REBOOT
    };

    /**
     * Class to be used to receive Control commands
     */
    class CONTROL_UTILS_API ControlCmdReceiver
        : private boost::noncopyable
    {
    public:

        typedef std::function<void(CommandAction cmdAction, int64_t nodeId)> NodeCmdCb;

        typedef std::function<void(CommandAction cmdAction)> SystemCmdCb;

        ControlCmdReceiver(boost::asio::io_service& ioService,
                           const NodeCmdCb&         nodeCmdCb,
                           const SystemCmdCb&       systemCmdCb);

        // Start command reception
        void Start();

        // Stop command reception
        void Stop();

    private:

        class Impl;

        boost::shared_ptr<Impl> m_impl;
    };

    /**
     *  Class to be used to send Control commands
     */
    class CONTROL_UTILS_API ControlCmdSender
        : private boost::noncopyable
    {
    public:

        ControlCmdSender(boost::asio::io_service&      ioService,
                         const std::function<void()>   controlConnectedCb);

        // Start sender
        void Start();

        // Stop sender
        void Stop();

        // commands
        void NodeCmd(CommandAction  cmdAction,
                     int64_t        nodeId);

        void SystemCmd(CommandAction cmdAction);

    private:
        class Impl;

        boost::shared_ptr<Impl> m_impl;
    };

    CONTROL_UTILS_API std::pair<std::unique_ptr<char[]>, size_t> SerializeCmd(CommandAction  cmdAction);

    CONTROL_UTILS_API CommandAction DeserializeCmd(const char* data, size_t size);

#ifdef _MSC_VER
#pragma warning (pop)
#endif

}
}
}
}



