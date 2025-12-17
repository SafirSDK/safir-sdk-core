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
#include <Safir/Dob/Internal/ControlCmd.h>
#include <Safir/Utilities/Internal/IpcPublisher.h>
#include <Safir/Utilities/Internal/IpcSubscriber.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Utilities/Internal/AsioStrandWrap.h>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4245)
#  pragma warning (disable: 4127)
#  pragma warning (disable: 4701)
#  pragma warning (disable: 4100)
#  pragma warning (disable: 4141)
#  pragma warning (disable: 4267)
#  pragma warning (disable: 4189)
#endif

#include "ControlCommands.pb.h"

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

#include <boost/asio/steady_timer.hpp>
#include <system_error>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{

namespace
{
    CommandAction getCommandAction(ControlCmd_CmdType cmdType)
    {
        switch (cmdType)
        {
            case ControlCmd_CmdType_STOP:
            {
                return STOP;
            }
            break;

            case ControlCmd_CmdType_SHUTDOWN:
            {
                return SHUTDOWN;
            }
            break;

            case ControlCmd_CmdType_REBOOT:
            {
                return REBOOT;
            }
            break;

            default:
            {
                throw std::logic_error("Received unknown control command!");
            }

        }
    }

    ControlCmd_CmdType getCmdType(CommandAction cmdAction)
    {
        switch (cmdAction)
        {
            case STOP:
            {
                return ControlCmd_CmdType_STOP;
            }
            break;

            case SHUTDOWN:
            {
                return ControlCmd_CmdType_SHUTDOWN;
            }
            break;

            case REBOOT:
            {
                return ControlCmd_CmdType_REBOOT;
            }
            break;

            default:
            {
                throw std::logic_error("Unknown value in enum CommandAction");
            }
        }
    }
}

    const std::string controlCmdChannel("CONTROL_CMD");

    // Receiver impl
    class ControlCmdReceiver::Impl
    {
    public:

        Impl(boost::asio::io_context&    io,
             const CmdCb&                cmdCb)
            : m_cmdCb(cmdCb)

        {
            m_ipcSubscriber.reset(new Safir::Utilities::Internal::IpcSubscriber(io,
                                                                                controlCmdChannel,
                                                                                [this](const char* data, size_t size)
                                                                                {
                                                                                    auto cmd = DeserializeCmd(data, size);

                                                                                    m_cmdCb(cmd.first, cmd.second);
                                                                                 }));
        }

        Impl(const Impl&) = delete;
        const Impl& operator=(const Impl&) = delete;

        void Start()
        {
            m_ipcSubscriber->Connect();
        }

        void Stop()
        {
            m_ipcSubscriber->Disconnect();
        }


    private:

        std::unique_ptr<Safir::Utilities::Internal::IpcSubscriber> m_ipcSubscriber;

        CmdCb   m_cmdCb;

        void RecvDataCb(const char* data, size_t size)
        {
            auto cmd = DeserializeCmd(data, size);

            m_cmdCb(cmd.first, cmd.second);

        }
    };

    ControlCmdReceiver::ControlCmdReceiver(boost::asio::io_context&     io,
                                           const CmdCb&                 cmdCb)
        : m_impl(Safir::make_unique<Impl>(io,
                                          cmdCb))
    {
    }

    void ControlCmdReceiver::Start()
    {
        m_impl->Start();
    }

    void ControlCmdReceiver::Stop()
    {
        m_impl->Stop();
    }

    // Sender impl
    class ControlCmdSender::Impl
    {
    public:

        Impl(boost::asio::io_context& io)
            : m_strand(io)
            , m_ipcPublisher(io,
                             controlCmdChannel,
                             // subscriberConnectedCb: called when a subscriber connects
                             Safir::Utilities::Internal::WrapInStrand(m_strand,
                                                                       [this]()
                                                                       {
                                                                           if (m_connectedCallback != nullptr && m_connectedCallback.get() != nullptr)
                                                                           {
                                                                               // Invoke the current per-send callback.
                                                                               (*m_connectedCallback)();
                                                                           }
                                                                       }),
                             nullptr)
            , m_connectedCallback()
            , m_hasActiveSend(false)
        {
        }

        Impl(const Impl&) = delete;
        const Impl& operator=(const Impl&) = delete;


        void SendCmd(CommandAction                      cmdAction,
                     int64_t                            nodeId,
                     const std::chrono::milliseconds&   timeout,
                     const CompletionCallback&          completionCallback)
        {
            // Make a copy of the user callback so its captures remain valid
            // for the lifetime of this send attempt, even if the caller's
            // original functor goes out of scope.
            auto cb = std::make_shared<CompletionCallback>(completionCallback);

            // All shared state is managed on m_strand to avoid races.
            boost::asio::dispatch(m_strand,
                                  [this,
                                   cmdAction,
                                   nodeId,
                                   timeout,
                                   cb]()
                                  {
                                      // Only allow one in-flight send per sender instance.
                                      if (m_hasActiveSend)
                                      {
                                          // Report "operation_in_progress" immediately.
                                          (*cb)(std::make_error_code(std::errc::operation_in_progress));
                                          return;
                                      }

                                      m_hasActiveSend = true;

                                      // Per-call timer, owned through shared_ptr so lifetime
                                      // is tied to the completion of this send attempt.
                                      auto timer = std::make_shared<boost::asio::steady_timer>(m_strand.context());
                                      timer->expires_after(timeout);

                                      // Common finish helper, must be called on m_strand.
                                      // We capture a shared_ptr copy of the *new* connected
                                      // callback below to ensure it stays alive at least
                                      // until done() has executed.
                                      auto done = [this, cb, timer](const std::error_code& ec)
                                      {
                                          // Per-send lifetime: stop accepting and close the
                                          // IPC endpoint after this send attempt (success or
                                          // timeout). This releases the channel so that other
                                          // processes can subsequently become publishers.
                                          m_ipcPublisher.Stop();

                                          // Invoke user callback on the strand (we are already
                                          // on it), with the error code.
                                          (*cb)(ec);

                                          // Clear per-call state
                                          m_connectedCallback.reset();
                                          m_hasActiveSend = false;
                                      };

                                      //we need to make sure to keep a copy of the callback until done() is finished.
                                      auto perSendCallback = std::make_shared<std::function<void()>>();

                                      // Timeout path: if timer expires before a subscriber
                                      // connects and triggers the send, we treat this as a
                                      // timeout.
                                      timer->async_wait(
                                          boost::asio::bind_executor(
                                              m_strand,
                                              [this, done, perSendCallback](const boost::system::error_code& ec)
                                              {
                                                  if (ec == boost::asio::error::operation_aborted)
                                                  {
                                                      // Timer was cancelled because we sent successfully.
                                                      return;
                                                  }
                                                  // Treat any non-aborted completion as timeout.
                                                  done(std::make_error_code(std::errc::timed_out));
                                              }));

                                      // Connected path: this callback will be invoked from the
                                      // IpcPublisher's subscriberConnectedCb, wrapped in the
                                      // same strand, so ordering with respect to the above
                                      // setup is well-defined.
                                      *perSendCallback =
                                          [this, cmdAction, nodeId, timer, done, perSendCallback]()
                                          {
                                              // Serialize and send the command.
                                              auto data = SerializeCmd(cmdAction, nodeId);
                                              m_ipcPublisher.Send(std::move(data.first),
                                                                  static_cast<uint32_t>(data.second));

                                              // Cancel the timeout timer; ignore any error.
                                              boost::system::error_code ignored;
                                              timer->cancel(ignored);

                                              // Report success.
                                              done(std::error_code());
                                          };

                                      // Store the per-send callback in the member so the
                                      // subscriberConnectedCb can invoke it.
                                      m_connectedCallback = perSendCallback;

                                      // Finally, start accepting connections. If a subscriber
                                      // is already present, the subscriberConnectedCb may fire
                                      // soon after this call and will invoke m_connectedCallback.
                                      m_ipcPublisher.Start();
                                  });
        }

    private:
        boost::asio::io_context::strand              m_strand;
        Safir::Utilities::Internal::IpcPublisher     m_ipcPublisher;
        std::shared_ptr<std::function<void()>>       m_connectedCallback;
        bool                                         m_hasActiveSend;
    };

    ControlCmdSender::ControlCmdSender(boost::asio::io_context&       io)
        : m_impl(Safir::make_unique<Impl>(io))
    {
    }

    void ControlCmdSender::SendCmd(CommandAction                      cmdAction,
                                   int64_t                            nodeId,
                                   const std::chrono::milliseconds&   timeout,
                                   const CompletionCallback&          completionCallback)
    {
        m_impl->SendCmd(cmdAction,
                        nodeId,
                        timeout,
                        completionCallback);
    }

    std::pair<std::unique_ptr<char[]>, size_t> SerializeCmd(CommandAction cmdAction,
                                                            int64_t       nodeId)
    {
        ControlCmd cmd;

        cmd.set_cmd_type(getCmdType(cmdAction));

        if (nodeId != 0)
        {
            cmd.set_node_id(nodeId);
        }

        const auto size = cmd.ByteSizeLong();
        auto data = std::unique_ptr<char[]>(new char[size]);
        cmd.SerializeWithCachedSizesToArray
            (reinterpret_cast<google::protobuf::uint8*>(data.get()));

        return std::make_pair(std::move(data), size);
    }

    std::pair<CommandAction, int64_t> DeserializeCmd(const char* data, size_t size)
    {
        ControlCmd controlCmd;

        controlCmd.ParseFromArray(data, static_cast<int>(size));

        std::pair<CommandAction, int64_t> res;

        res.first = getCommandAction(controlCmd.cmd_type());

        if (controlCmd.has_node_id())
        {
            res.second = controlCmd.node_id();
        }
        else
        {
            res.second = 0;
        }

        return res;
    }

}
}
}
}
