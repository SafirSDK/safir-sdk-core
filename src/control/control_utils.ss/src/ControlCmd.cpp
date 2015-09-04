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
             const NodeCmdCb&            nodeCmdCb,
             const SystemCmdCb&          systemCmdCb)
            : m_nodeCmdCb(nodeCmdCb),
              m_systemCmdCb(systemCmdCb)
        {
            m_ipcSubscriber.reset( new Safir::Utilities::Internal::IpcSubscriber(ioService,
                                                                                 controlCmdChannel,
                                                                                 [this](const char* data, size_t size)
                                                                                 {
                                                                                     RecvDataCb(data, size);
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

        NodeCmdCb   m_nodeCmdCb;
        SystemCmdCb m_systemCmdCb;

        void RecvDataCb(const char* data, size_t size)
        {
            ControlCmd controlCmd;

            controlCmd.ParseFromArray(data, static_cast<int>(size));

            if (controlCmd.has_node_id())
            {
                m_nodeCmdCb(getCommandAction(controlCmd.cmd_type()), controlCmd.node_id());
            }
            else
            {
                m_systemCmdCb(getCommandAction(controlCmd.cmd_type()));
            }
        }
    };

    ControlCmdReceiver::ControlCmdReceiver(boost::asio::io_service&     ioService,
                                           const NodeCmdCb&             nodeCmdCb,
                                           const SystemCmdCb&           systemCmdCb)
        : m_impl(Safir::make_unique<Impl>(ioService,
                                          nodeCmdCb,
                                          systemCmdCb))
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

        void NodeCmd(CommandAction  cmdAction,
                     int64_t        nodeId)
        {
            ControlCmd cmd;

            cmd.set_cmd_type(getCmdType(cmdAction));
            cmd.set_node_id(nodeId);

            const auto size = cmd.ByteSize();
            auto data = std::unique_ptr<char[]>(new char[size]);
            cmd.SerializeWithCachedSizesToArray
                (reinterpret_cast<google::protobuf::uint8*>(data.get()));

            m_ipcPublisher.Send(std::move(data), size);

        }

        void SystemCmd(CommandAction  cmdAction)
        {
            auto sc = SerializeCmd(cmdAction);

            m_ipcPublisher.Send(std::move(sc.first), sc.second);
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

    void ControlCmdSender::NodeCmd(CommandAction  cmdAction,
                                   int64_t        nodeId)
    {
        m_impl->NodeCmd(cmdAction,
                        nodeId);
    }

    void ControlCmdSender::SystemCmd(CommandAction  cmdAction)
    {
        m_impl->SystemCmd(cmdAction);
    }

    std::pair<std::unique_ptr<char[]>, size_t> SerializeCmd(CommandAction cmdAction)
    {
        ControlCmd cmd;

        cmd.set_cmd_type(getCmdType(cmdAction));

        const auto size = cmd.ByteSize();
        auto data = std::unique_ptr<char[]>(new char[size]);
        cmd.SerializeWithCachedSizesToArray
            (reinterpret_cast<google::protobuf::uint8*>(data.get()));

        return std::make_pair(std::move(data), size);
    }
}
}
}
}


