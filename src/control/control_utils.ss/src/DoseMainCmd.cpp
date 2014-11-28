/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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
#include <Safir/Dob/Internal/DoseMainCmd.h>
#include <Safir/Utilities/Internal/IpcPublisher.h>
#include <Safir/Utilities/Internal/IpcSubscriber.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include "ControlCommands.pb.h"
#include <set>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{    
    const std::string doseMainCmdChannel("DOSE_MAIN_CMD");

    // Receiver impl
    class DoseMainCmdReceiver::Impl
        : private boost::noncopyable
    {
    public:

        Impl(boost::asio::io_service& ioService,
             const InjectNodeCmdCb&   cmdCb)
            : m_ipcSubscriber(ioService,
                              doseMainCmdChannel,
                              [this](const char* data, size_t size)
                              {
                                  RecvDataCb(data, size);
                              }),
              m_injectNodeCmdCb(cmdCb)
        {
        }

        void Start()
        {
            m_ipcSubscriber.Connect();
        }

        void Stop()
        {
            m_ipcSubscriber.Disconnect();
        }


    private:

        Safir::Utilities::Internal::IpcSubscriber m_ipcSubscriber;

        InjectNodeCmdCb     m_injectNodeCmdCb;

        void RecvDataCb(const char* data, size_t size)
        {
            ControlCmd controlCmd;

            controlCmd.ParseFromArray(data, static_cast<int>(size));

            if (controlCmd.has_inject_node_cmd())
            {
               m_injectNodeCmdCb(controlCmd.request_id(),
                                 controlCmd.inject_node_cmd().node_name(),
                                 controlCmd.inject_node_cmd().node_id(),
                                 controlCmd.inject_node_cmd().node_type_id(),
                                 controlCmd.inject_node_cmd().data_address());
            }
            // More commands here
            else
            {
               throw std::logic_error("Received unknown control command!");
            }

        }
    };

    DoseMainCmdReceiver::DoseMainCmdReceiver(boost::asio::io_service& ioService,
                                             const InjectNodeCmdCb&   cmdCb)
        : m_impl(Safir::make_unique<Impl>(ioService, cmdCb))
    {
    }

    void DoseMainCmdReceiver::Start()
    {
        m_impl->Start();
    }

    void DoseMainCmdReceiver::Stop()
    {
        m_impl->Stop();
    }

    // Sender impl
    class DoseMainCmdSender::Impl
        : private boost::noncopyable
    {
    public:

        Impl(boost::asio::io_service& ioService)
            : m_ipcPublisher(ioService, doseMainCmdChannel, nullptr, nullptr)
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

        void InjectNode(int64_t requestId,
                        const std::string& nodeName,
                        int64_t nodeId,
                        int64_t nodeTypeId,
                        const std::string& dataAddress)
        {
            ControlCmd controlCmd;

            controlCmd.set_request_id(requestId);

            controlCmd.mutable_inject_node_cmd()->set_node_name(nodeName);
            controlCmd.mutable_inject_node_cmd()->set_node_id(nodeId);
            controlCmd.mutable_inject_node_cmd()->set_node_type_id(nodeTypeId);
            controlCmd.mutable_inject_node_cmd()->set_data_address(dataAddress);

            Send(controlCmd);
        }

    private:

        Safir::Utilities::Internal::IpcPublisher m_ipcPublisher;

        void Send(const ControlCmd& cmd)
        {
            const auto size = cmd.ByteSize();
            auto data = std::unique_ptr<char[]>(new char[size]);
            cmd.SerializeWithCachedSizesToArray
                (reinterpret_cast<google::protobuf::uint8*>(data.get()));
            m_ipcPublisher.Send(std::move(data), size);
        }

    };

    DoseMainCmdSender::DoseMainCmdSender(boost::asio::io_service& ioService)
        : m_impl(Safir::make_unique<Impl>(ioService))
    {
    }

    void DoseMainCmdSender::Start()
    {
        m_impl->Start();
    }

    void DoseMainCmdSender::Stop()
    {
        m_impl->Stop();
    }

    void DoseMainCmdSender::InjectNode(int64_t requestId,
                                       const std::string& nodeName,
                                       int64_t nodeId,
                                       int64_t nodeTypeId,
                                       const std::string& dataAddress)
    {
        m_impl->InjectNode(requestId,
                           nodeName,
                           nodeId,
                           nodeTypeId,
                           dataAddress);
    }


}
}
}
}


