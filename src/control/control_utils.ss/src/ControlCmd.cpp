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
#include "ControlCommands.pb.h"

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
        : private boost::noncopyable
    {
    public:

        Impl(boost::asio::io_service&    ioService,
             const CmdCb&                cmdCb)
            : m_cmdCb(cmdCb)

        {
            m_ipcSubscriber.reset(new Safir::Utilities::Internal::IpcSubscriber(ioService,
                                                                                controlCmdChannel,
                                                                                [this](const char* data, size_t size)
                                                                                {
                                                                                    auto cmd = DeserializeCmd(data, size);

                                                                                    m_cmdCb(cmd.first, cmd.second);
                                                                                 }));
        }

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

    ControlCmdReceiver::ControlCmdReceiver(boost::asio::io_service&     ioService,
                                           const CmdCb&                 cmdCb)
        : m_impl(Safir::make_unique<Impl>(ioService,
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
        : private boost::noncopyable
    {
    public:

        Impl(boost::asio::io_service&       ioService,
             const std::function<void()>    controlConnectedCb)
            : m_ipcPublisher(ioService, controlCmdChannel, controlConnectedCb, NULL)
        {
        }

        void Start()
        {
           m_ipcPublisher.Start();
        }

        void Stop()
        {
           m_ipcPublisher.Stop();
        }

        void SendCmd(CommandAction  cmdAction,
                     int64_t        nodeId)
        {
            auto data = SerializeCmd(cmdAction, nodeId);

            m_ipcPublisher.Send(std::move(data.first), static_cast<uint32_t>(data.second));

        }

    private:

        Safir::Utilities::Internal::IpcPublisher m_ipcPublisher;
    };

    ControlCmdSender::ControlCmdSender(boost::asio::io_service&       ioService,
                                       const std::function<void()>    controlConnectedCb)
        : m_impl(Safir::make_unique<Impl>(ioService, controlConnectedCb))
    {
    }

    void ControlCmdSender::Start()
    {
        m_impl->Start();
    }

    void ControlCmdSender::Stop()
    {
        m_impl->Stop();
    }

    void ControlCmdSender::SendCmd(CommandAction  cmdAction,
                                   int64_t        nodeId)
    {
        m_impl->SendCmd(cmdAction,
                        nodeId);
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

        const auto size = cmd.ByteSize();
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


