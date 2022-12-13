/******************************************************************************
*
* Copyright Saab AB, 2014-2015 (http://safirsdkcore.com)
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
#include <set>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4127)
#endif

#include "DoseMainCommands.pb.h"

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{
    const std::string doseMainCmdChannel("DOSE_MAIN_CMD");

    std::string NodeStateToString(NodeState nodeState)
    {
        return DoseMainCmd_NodeStateType_Name(static_cast<DoseMainCmd_NodeStateType>(nodeState));
    }

    // Receiver impl
    class DoseMainCmdReceiver::Impl
    {
    public:

        Impl(boost::asio::io_context&       io,
             const IncludeNodeCmdCb&        startDoseMainCb,
             const IncludeNodeCmdCb&        injectNodeCb,
             const ExcludeNodeCmdCb&        excludeNodeCb,
             const StoppedNodeIndicationCb& stoppedNodeIndicationCb,
             const StopDoseMainCb&          stopDoseMainCb,
             const NodeStateChangedCb&      nodeStateChangedCb)
            : m_startDoseMainCb(startDoseMainCb),
              m_injectNodeCb(injectNodeCb),
              m_excludeNodeCb(excludeNodeCb),
              m_stoppedNodeIndicationCb(stoppedNodeIndicationCb),
              m_stopDoseMainCb(stopDoseMainCb),
              m_nodeStateChangedCb(nodeStateChangedCb)
        {
            m_ipcSubscriber.reset( new Safir::Utilities::Internal::IpcSubscriber(io,
                                                                                 doseMainCmdChannel,
                                                                                 [this](const char* data, size_t size)
                                                                                 {
                                                                                     RecvDataCb(data, size);
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

        IncludeNodeCmdCb        m_startDoseMainCb;
        IncludeNodeCmdCb        m_injectNodeCb;
        ExcludeNodeCmdCb        m_excludeNodeCb;
        StoppedNodeIndicationCb m_stoppedNodeIndicationCb;
        StopDoseMainCb          m_stopDoseMainCb;
        NodeStateChangedCb      m_nodeStateChangedCb;

        void RecvDataCb(const char* data, size_t size)
        {
            DoseMainCmd doseMainCmd;

            doseMainCmd.ParseFromArray(data, static_cast<int>(size));

            switch (doseMainCmd.cmd_type())
            {
                case doseMainCmd.START:
                {
                    m_startDoseMainCb(doseMainCmd.node_name(),
                                      doseMainCmd.node_id(),
                                      doseMainCmd.node_type_id(),
                                      doseMainCmd.data_address());
                }
                break;

                case doseMainCmd.INJECT_NODE:
                {
                    m_injectNodeCb(doseMainCmd.node_name(),
                                   doseMainCmd.node_id(),
                                   doseMainCmd.node_type_id(),
                                   doseMainCmd.data_address());
                }
                break;

                case doseMainCmd.EXCLUDE_NODE:
                {
                    m_excludeNodeCb(doseMainCmd.node_id(),
                                    doseMainCmd.node_type_id());
                }
                break;

                case doseMainCmd.STOPPED_NODE_INDICATION:
                {
                    m_stoppedNodeIndicationCb(doseMainCmd.node_id());
                }
                break;

                case doseMainCmd.STOP_DOSE_MAIN:
                {
                    m_stopDoseMainCb();
                }
                break;

                case doseMainCmd.NODE_STATE:
                {
                    auto nodeState = static_cast<NodeState>(doseMainCmd.node_state());
                    m_nodeStateChangedCb(nodeState);
                }
                break;

                default:
                {
                    throw std::logic_error("Received unknown doseMain command!");
                }

            }
        }
    };

    DoseMainCmdReceiver::DoseMainCmdReceiver(boost::asio::io_context&       io,
                                             const IncludeNodeCmdCb&        startDoseMainCb,
                                             const IncludeNodeCmdCb&        injectNodeCb,
                                             const ExcludeNodeCmdCb&        excludeNodeCb,
                                             const StoppedNodeIndicationCb& stoppedNodeIndicationCb,
                                             const StopDoseMainCb&          stopDoseMainCb,
                                             const NodeStateChangedCb&      nodeStateChangedCb)
        : m_impl(Safir::make_unique<Impl>(io,
                                          startDoseMainCb,
                                          injectNodeCb,
                                          excludeNodeCb,
                                          stoppedNodeIndicationCb,
                                          stopDoseMainCb,
                                          nodeStateChangedCb))
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
    {
    public:

        Impl(boost::asio::io_context&       io,
             const std::function<void()>    doseMainConnectedCb)
            : m_ipcPublisher(io, doseMainCmdChannel, doseMainConnectedCb, NULL)
        {
        }

        Impl(const Impl&) = delete;
        const Impl& operator=(const Impl&) = delete;

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
            DoseMainCmd doseMainCmd;

            doseMainCmd.set_cmd_type(DoseMainCmd::START);
            doseMainCmd.set_node_name(nodeName);
            doseMainCmd.set_node_id(nodeId);
            doseMainCmd.set_node_type_id(nodeTypeId);
            doseMainCmd.set_data_address(dataAddress);

            Send(doseMainCmd);
        }

        void InjectNode(const std::string&  nodeName,
                        int64_t             nodeId,
                        int64_t             nodeTypeId,
                        const std::string&  dataAddress)
        {
            DoseMainCmd doseMainCmd;

            doseMainCmd.set_cmd_type(DoseMainCmd::INJECT_NODE);
            doseMainCmd.set_node_name(nodeName);
            doseMainCmd.set_node_id(nodeId);
            doseMainCmd.set_node_type_id(nodeTypeId);
            doseMainCmd.set_data_address(dataAddress);

            Send(doseMainCmd);
        }

        void ExcludeNode(int64_t nodeId,
                         int64_t nodeTypeId)
        {
            DoseMainCmd doseMainCmd;

            doseMainCmd.set_cmd_type(DoseMainCmd::EXCLUDE_NODE);
            doseMainCmd.set_node_id(nodeId);
            doseMainCmd.set_node_type_id(nodeTypeId);

            Send(doseMainCmd);
        }

        void StoppedNodeIndication(int64_t nodeId)
        {
            DoseMainCmd doseMainCmd;

            doseMainCmd.set_cmd_type(DoseMainCmd::STOPPED_NODE_INDICATION);
            doseMainCmd.set_node_id(nodeId);

            Send(doseMainCmd);
        }

        void StopDoseMain()
        {
            DoseMainCmd doseMainCmd;

            doseMainCmd.set_cmd_type(DoseMainCmd::STOP_DOSE_MAIN);

            Send(doseMainCmd);
        }

        void NodeStateChanged(NodeState nodeState)
        {
            DoseMainCmd doseMainCmd;
            doseMainCmd.set_cmd_type(DoseMainCmd::NODE_STATE);
            doseMainCmd.set_node_state(static_cast<DoseMainCmd_NodeStateType>(nodeState));
            Send(doseMainCmd);
        }

    private:

        Safir::Utilities::Internal::IpcPublisher m_ipcPublisher;

        void Send(const DoseMainCmd& cmd)
        {
            const auto size = cmd.ByteSizeLong();
            auto data = std::unique_ptr<char[]>(new char[size]);
            cmd.SerializeWithCachedSizesToArray
                (reinterpret_cast<google::protobuf::uint8*>(data.get()));
            m_ipcPublisher.Send(std::move(data), static_cast<uint32_t>(size));
        }

    };

    DoseMainCmdSender::DoseMainCmdSender(boost::asio::io_context&       io,
                                         const std::function<void()>    doseMainConnectedCb)
        : m_impl(Safir::make_unique<Impl>(io, doseMainConnectedCb))
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

    void DoseMainCmdSender::StoppedNodeIndication(int64_t nodeId)
    {
        m_impl->StoppedNodeIndication(nodeId);
    }

    void DoseMainCmdSender::StopDoseMain()
    {
        m_impl->StopDoseMain();
    }

    void DoseMainCmdSender::NodeStateChanged(NodeState nodeState)
    {
        m_impl->NodeStateChanged(nodeState);
    }


}
}
}
}


