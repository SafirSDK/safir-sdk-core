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
#include <string>
#include <boost/asio/io_context.hpp>

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
     * Class used to receive Control commands over IPC.
     *
     * The receiver is expected to be a long‑lived component:
     * it should typically be created once at process startup,
     * call Start() and keep running for the lifetime of the
     * process. Short‑lived / transient receivers are not
     * supported well by the underlying IPC mechanism.
     */
    class CONTROL_UTILS_API ControlCmdReceiver
    {
    public:

        /**
         * Callback type invoked when a control command is received.
         *
         * @param cmdAction The received command action.
         * @param nodeId    The target node id (0 means "system‑wide").
         */
        typedef std::function<void(CommandAction cmdAction, int64_t nodeId)> CmdCb;

        /**
         * Construct a ControlCmdReceiver.
         *
         * @param io    io_context used for all asynchronous IPC operations.
         * @param cmdCb Callback invoked for each received command.
         */
        ControlCmdReceiver(boost::asio::io_context& io,
                           const CmdCb&             cmdCb);

        ControlCmdReceiver(const ControlCmdReceiver&) = delete;
        const ControlCmdReceiver& operator=(const ControlCmdReceiver&) = delete;

        /**
         * Start command reception.
         *
         * This will initiate (and keep) a connection to the
         * ControlCmdSender publisher on the "CONTROL_CMD" IPC
         * channel and begin delivering received commands via
         * the registered callback.
         */
        void Start();

        /**
         * Stop command reception.
         *
         * Disconnects from the publisher and stops any ongoing
         * asynchronous reads. After calling Stop(), no further
         * callbacks will be delivered until Start() is called
         * again.
         */
        void Stop();

    private:

        class Impl;

        std::shared_ptr<Impl> m_impl;
    };

    /**
     * Class used to send Control commands over IPC.
     *
     * USAGE AND IMPORTANT LIMITATIONS
     * ===============================
     *
     * - A ControlCmdSender sends commands over a fixed IPC channel
     *   ("CONTROL_CMD") to one or more ControlCmdReceiver instances
     *   on the same host.
     *
     * - This mechanism does NOT coordinate multiple independent
     *   processes that send at the same time:
     *
     *     * Only one process can safely act as the active publisher
     *       on the "CONTROL_CMD" IPC channel at any given moment.
     *
     *     * If two or more processes call SendCmd concurrently on
     *       the same host, there is a RACE: one of the commands may
     *       be lost or fail to reach the intended receiver even if
     *       the receiver is running.
     *
     * - Safe usage patterns:
     *     * A single process uses ControlCmdSender.
     *     * Or multiple processes use it, but they do not overlap
     *       in time when sending on the same host (commands are
     *       issued rarely and not simultaneously).
     *
     * If strict guarantees are required when several independent
     * processes send concurrently, additional higher‑level
     * coordination is needed on top of this API.
     */
    class CONTROL_UTILS_API ControlCmdSender
    {
    public:
        /**
         * Completion callback type for asynchronous SendCmd operations.
         *
         * @param error std::error_code indicating success or failure.
         */
        typedef std::function<void(const std::error_code& error)> CompletionCallback;

        /**
         * Construct a ControlCmdSender.
         *
         * @param io                 io_context used for asynchronous IPC.
         */
        ControlCmdSender(boost::asio::io_context&      io);

        ControlCmdSender(const ControlCmdSender&) = delete;
        const ControlCmdSender& operator=(const ControlCmdSender&) = delete;

        /**
         * Send a control command.
         *
         * The sender will attempt to deliver the given command to any
         * connected ControlCmdReceiver instances within the provided
         * timeout.
         *
         * @param cmdAction         Command to send (STOP, SHUTDOWN, REBOOT).
         * @param nodeId            Target node id (0 means "system‑wide").
         * @param timeout           Maximum time to wait for send completion.
         * @param completionCallback Callback invoked when sending is done
         *                           or has failed.
         */
        void SendCmd(CommandAction             cmdAction,
                     int64_t                   nodeId,
                     const std::chrono::milliseconds& timeout, // Recommend about 5 seconds here, the receiver resubscribes 1/sec
                     const CompletionCallback& completionCallback);

    private:
        class Impl;

        std::shared_ptr<Impl> m_impl;
    };

    CONTROL_UTILS_API std::pair<std::unique_ptr<char[]>, size_t> SerializeCmd(CommandAction  cmdAction,
                                                                              int64_t        nodeId);

    CONTROL_UTILS_API std::pair<CommandAction, int64_t> DeserializeCmd(const char* data, size_t size);

#ifdef _MSC_VER
#pragma warning (pop)
#endif

}
}
}
}
