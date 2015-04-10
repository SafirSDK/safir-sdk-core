/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2014 (http://www.consoden.se)
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

        Impl(boost::asio::io_service&   ioService,
             const IncludeNodeCmdCb&    startDoseMainCb,
             const IncludeNodeCmdCb&    injectNodeCb,
             const ExcludeNodeCmdCb&    excludeNodeCb,
             const StopDoseMainCb&      stopDoseMainCb)
            : m_ipcSubscriber(ioService,
                              doseMainCmdChannel,
                              [this](const char* data, size_t size)
                              {
                                  RecvDataCb(data, size);
                              }),
              m_startDoseMainCb(startDoseMainCb),
              m_injectNodeCb(injectNodeCb),
              m_excludeNodeCb(excludeNodeCb),
              m_stopDoseMainCb(stopDoseMainCb)
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

        IncludeNodeCmdCb    m_startDoseMainCb;
        IncludeNodeCmdCb    m_injectNodeCb;
        ExcludeNodeCmdCb    m_excludeNodeCb;
        StopDoseMainCb      m_stopDoseMainCb;

        void RecvDataCb(const char* data, size_t size)
        {
            ControlCmd controlCmd;

            controlCmd.ParseFromArray(data, static_cast<int>(size));

            switch (controlCmd.cmd_type())
            {
                case controlCmd.START:
                {
                    m_startDoseMainCb(controlCmd.node_name(),
                                      controlCmd.node_id(),
                                      controlCmd.node_type_id(),
                                      controlCmd.data_address());
                }
                break;

                case controlCmd.INJECT_NODE:
                {
                    m_injectNodeCb(controlCmd.node_name(),
                                   controlCmd.node_id(),
                                   controlCmd.node_type_id(),
                                   controlCmd.data_address());
                }
                break;

                case controlCmd.EXCLUDE_NODE:
                {
                    m_excludeNodeCb(controlCmd.node_id(),
                                    controlCmd.node_type_id());
                }
                break;

                case controlCmd.STOP:
                {
                    m_stopDoseMainCb();
                }
                break;

                default:
                {
                    throw std::logic_error("Received unknown control command!");
                }

            }
        }
    };

    DoseMainCmdReceiver::DoseMainCmdReceiver(boost::asio::io_service&   ioService,
                                             const IncludeNodeCmdCb&    startDoseMainCb,
                                             const IncludeNodeCmdCb&    injectNodeCb,
                                             const ExcludeNodeCmdCb&    excludeNodeCb,
                                             const StopDoseMainCb&      stopDoseMainCb)
        : m_impl(Safir::make_unique<Impl>(ioService,
                                          startDoseMainCb,
                                          injectNodeCb,
                                          excludeNodeCb,
                                          stopDoseMainCb))
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

        Impl(boost::asio::io_service&       ioService,
             const std::function<void()>    doseMainConnectedCb)
            : m_ipcPublisher(ioService, doseMainCmdChannel, doseMainConnectedCb, nullptr)
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

        void StartDoseMain(const std::string&   nodeName,
                           int64_t              nodeId,
                           int64_t              nodeTypeId,
                           const std::string&   dataAddress)
        {
            ControlCmd controlCmd;

            controlCmd.set_cmd_type(ControlCmd::START);
            controlCmd.set_node_name(nodeName);
            controlCmd.set_node_id(nodeId);
            controlCmd.set_node_type_id(nodeTypeId);
            controlCmd.set_data_address(dataAddress);

            Send(controlCmd);
        }

        void InjectNode(const std::string&  nodeName,
                        int64_t             nodeId,
                        int64_t             nodeTypeId,
                        const std::string&  dataAddress)
        {
            ControlCmd controlCmd;

            controlCmd.set_cmd_type(ControlCmd::INJECT_NODE);
            controlCmd.set_node_name(nodeName);
            controlCmd.set_node_id(nodeId);
            controlCmd.set_node_type_id(nodeTypeId);
            controlCmd.set_data_address(dataAddress);

            Send(controlCmd);
        }

        void ExcludeNode(int64_t nodeId,
                         int64_t nodeTypeId)
        {
            ControlCmd controlCmd;

            controlCmd.set_cmd_type(ControlCmd::EXCLUDE_NODE);
            controlCmd.set_node_id(nodeId);
            controlCmd.set_node_type_id(nodeTypeId);

            Send(controlCmd);
        }

        void StopDoseMain()
        {
            ControlCmd controlCmd;

            controlCmd.set_cmd_type(ControlCmd::STOP);

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

    DoseMainCmdSender::DoseMainCmdSender(boost::asio::io_service&       ioService,
                                         const std::function<void()>    doseMainConnectedCb)
        : m_impl(Safir::make_unique<Impl>(ioService, doseMainConnectedCb))
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

    void DoseMainCmdSender::StartDoseMain(const std::string&    nodeName,
                                          int64_t               nodeId,
                                          int64_t               nodeTypeId,
                                          const std::string&    dataAddress)
    {
        m_impl->StartDoseMain(nodeName,
                              nodeId,
                              nodeTypeId,
                              dataAddress);
    }

    void DoseMainCmdSender::InjectNode(const std::string&   nodeName,
                                       int64_t              nodeId,
                                       int64_t              nodeTypeId,
                                       const std::string&   dataAddress)
    {
        m_impl->InjectNode(nodeName,
                           nodeId,
                           nodeTypeId,
                           dataAddress);
    }

    void DoseMainCmdSender::ExcludeNode(int64_t nodeId, int64_t nodeTypeId)
    {
        m_impl->ExcludeNode(nodeId, nodeTypeId);
    }

    void DoseMainCmdSender::StopDoseMain()
    {
        m_impl->StopDoseMain();
    }


}
}
}
}


