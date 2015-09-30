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

#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Dob/Internal/ControlCmd.h>
#include <Safir/Dob/Internal/DoseMainCmd.h>
#include <Safir/Dob/Internal/ControlConfig.h>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <boost/chrono.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include <set>
#include <string>
#include <unordered_map>
#include <utility>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{

    /**
     * @brief The StopHandler class handles the logic for Stop, Shutdown or Reboot of single nodes as well as
     *        a complete system.
     */
    template<typename Communication,
             typename SP,
             typename ControlCmdReceiver,
             typename DoseMainCmdSender,
             typename ControlConfig>
    class StopHandlerBasic
    {
    public:

        typedef std::function<void()> StopSafirNodeCb;
        typedef std::function<void()> ShutdownCb;
        typedef std::function<void()> RebootCb;
        typedef std::function<void()> StopSystemCb;

        StopHandlerBasic(boost::asio::io_service&   ioService,
                         Communication&             communication,
                         SP&                        sp,
                         DoseMainCmdSender&         doseMainCmdSender,
                         ControlConfig&             config,
                         const StopSafirNodeCb      stopSafirNodeCb,
                         const ShutdownCb           shutdownCb,
                         const RebootCb             rebootCb,
                         const StopSystemCb         stopSystemCb,
                         bool                       ignoreCmd)
            : m_strand(ioService),
              m_communication(communication),
              m_sp(sp),
              m_doseMainCmdSender(doseMainCmdSender),
              m_config(config),
              m_stopSafirNodeCb(stopSafirNodeCb),
              m_shutdownCb(shutdownCb),
              m_rebootCb(rebootCb),
              m_stopSystemCb(stopSystemCb),
              m_ignoreCmd(ignoreCmd),
              m_stopOrderMsgTypeId(LlufId_Generate64("Safir.Dob.Control.StopOrderMsgTypeId")),
              m_stopNotificationMsgTypeId(LlufId_Generate64("Safir.Dob.Control.StopNotificationMsgTypeId")),
              m_sendTimer(ioService),
              m_stopTimer(ioService),
              m_localNodeStopInProgress(false),
              m_systemStop(false)

        {
            for (unsigned int idx = 0; idx < m_config.nodeTypesParam.size(); ++idx)
            {
                auto nodeTypeId = m_config.nodeTypesParam[idx].id;
                auto maxStopDuration = (boost::chrono::milliseconds(m_config.nodeTypesParam[idx].heartbeatInterval) *
                                        m_config.nodeTypesParam[idx].maxLostHeartbeats) * 2;

                m_nodeTypeTable.insert(std::make_pair(nodeTypeId, maxStopDuration));

            }

            // Set up callbacks to receive stop commands locally via IPC
            m_controlCmdReceiver.reset(new ControlCmdReceiver(ioService,
                                                              m_strand.wrap(
                                                              [this](Control::CommandAction cmdAction, int64_t nodeId)
                                                              {
                                                                  if (nodeId != 0)
                                                                  {
                                                                      // An local order to stop a specific node

                                                                      if (nodeId == m_communication.Id())
                                                                      {
                                                                          HandleLocalNodeStop(cmdAction);
                                                                      }
                                                                      else
                                                                      {
                                                                          StopExternalNode(cmdAction, nodeId, nodeId);
                                                                      }
                                                                  }
                                                                  else
                                                                  {
                                                                      // A local order to stop the system
                                                                      HandleSystemStop(cmdAction);
                                                                  }

                                                              })));

            // Set up callback to receive stop commands via control channel from an external node.
            communication.SetDataReceiver(m_strand.wrap(
                                         [this](const int64_t from,
                                                const int64_t /*nodeTypeId*/,
                                                const char* const data,
                                                const size_t size)
                                         {
                                             HandleStopOrderFromExternalNode(from,
                                                                              boost::shared_ptr<const char[]>(data),size);
                                         }),
                                         m_stopOrderMsgTypeId,
                                         [](size_t size){return new char[size];},
                                         [](const char * data){delete[] data;});

            // Set up callback to receive stop notifications via control channel from an external node.
            communication.SetDataReceiver(m_strand.wrap(
                                          [this](const int64_t from,
                                                 const int64_t /*nodeTypeId*/,
                                                 const char* const /*data*/,
                                                 const size_t /*size*/)
                                          {
                                              HandleStopNotificationFromExternalNode(from);
                                          }),
                                          m_stopNotificationMsgTypeId,
                                          [](size_t size){return new char[size];},
                                          [](const char * data){delete[] data;});
        }

        ~StopHandlerBasic()
        {
            // This is about the last thing that happens when a node is stopped. When this destructor is
            // executed we know that the ioService is empty and therefor this is a good place to make
            // a shutdown or reboot.
            switch (m_localNodeStopCmdAction)
            {
                case STOP:
                {
                    // Do nothing
                }
                break;

                case SHUTDOWN:
                {
                    m_shutdownCb();
                }
                break;

                case REBOOT:
                {
                    m_rebootCb();
                }
                break;
            }
        }

        void Start()
        {
            m_controlCmdReceiver->Start();
        }

        void Stop()
        {
            m_strand.post([this]()
                          {
                              m_controlCmdReceiver->Stop();
                              m_sendTimer.cancel();
                              m_stopTimer.cancel();
                          });


        }

        void AddNode(const int64_t nodeId, const int64_t nodeTypeId)
        {
            m_strand.post([this, nodeId, nodeTypeId]()
                          {
                              if (nodeId == m_communication.Id())
                              {
                                  // We don't save own node
                                  return;
                              }

                              Node node(nodeTypeId);

                              if (m_systemStop)
                              {
                                  // A node is added while we are about to stop the system

                                  node.stopInProgress = true;
                                  node.cmdAction = m_localNodeStopCmdAction;
                                  node.cmdNodeId = 0;
                              }

                              m_nodeTable.insert(std::make_pair(nodeId, node));
                          });
        }

        void RemoveNode(const int64_t nodeId)
        {
            m_strand.post([this, nodeId]()
                          {
                              m_nodeTable.erase(nodeId);
                          });
        }

    private:

        boost::asio::io_service::strand m_strand;

        Communication&      m_communication;
        SP&                 m_sp;
        DoseMainCmdSender&  m_doseMainCmdSender;
        ControlConfig&      m_config;

        const StopSafirNodeCb   m_stopSafirNodeCb;
        const ShutdownCb        m_shutdownCb;
        const RebootCb          m_rebootCb;
        const StopSystemCb      m_stopSystemCb;

        const bool  m_ignoreCmd;

        std::unique_ptr<ControlCmdReceiver> m_controlCmdReceiver;

        const int64_t   m_stopOrderMsgTypeId;
        const int64_t   m_stopNotificationMsgTypeId;

        boost::asio::steady_timer m_sendTimer;
        boost::asio::steady_timer m_stopTimer;


        bool                    m_localNodeStopInProgress;
        Control::CommandAction  m_localNodeStopCmdAction;

        bool m_systemStop;

        std::unordered_map<int64_t, boost::chrono::milliseconds> m_nodeTypeTable;

        struct Node
        {
            Node(int64_t _nodeTypeId)
                : nodeTypeId(_nodeTypeId),
                  stopInProgress(false)
            {
            }

            const int64_t                       nodeTypeId;

            boost::chrono::steady_clock::time_point maxStopTime;
            Control::CommandAction                  cmdAction;
            int64_t                                 cmdNodeId;
            bool                                    stopInProgress;
        };

        std::unordered_map<int64_t, Node> m_nodeTable;

        void StopThisNode()
        {
            if (m_ignoreCmd)
            {
                return;
            }

            // We are about to die, stop any sending of stop commands to other nodes
            m_sendTimer.cancel();

            // Tell other nodes that this node is about to stop.
            SendStopNotification();

            // Wait a short while and then send the notification again, just in case.

            m_stopTimer.expires_from_now(boost::chrono::milliseconds(300));
            m_stopTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
                                                 {
                                                     if (error == boost::asio::error::operation_aborted)
                                                     {
                                                         return;
                                                     }
                                                     SendStopNotification();

                                                     // The notification has been sent a second time, now we stop this node
                                                     m_stopSafirNodeCb();
                                                 }));
        }

        void HandleLocalNodeStop(CommandAction cmdAction)
        {
            m_localNodeStopInProgress = true;
            m_localNodeStopCmdAction = cmdAction;

            bool externalNodeStopInProgress = false;

            for (auto nodeIt = m_nodeTable.begin(); nodeIt != m_nodeTable.end(); ++nodeIt)
            {
                if (nodeIt->second.stopInProgress)
                {
                    externalNodeStopInProgress = true;
                    break;
                }
            }

            if (externalNodeStopInProgress)
            {
                // There is an outstanding stop request to one or more external nodes. Don't
                // execute stop of this node now. (It will be stopped when all outstanding stop requests
                // have been handled.
            }
            else
            {
                StopThisNode();
            }
        }

        void HandleSystemStop(CommandAction cmdAction)
        {
            m_systemStop = true;

            m_localNodeStopInProgress = true;
            m_localNodeStopCmdAction = cmdAction;

            m_stopSystemCb();  // Notify other parts that we got a system stop order

            // Initiate stop of all known nodes
            auto now = boost::chrono::steady_clock::now();
            for (auto nodeIt = m_nodeTable.begin(); nodeIt != m_nodeTable.end(); ++nodeIt)
            {
                nodeIt->second.cmdAction = cmdAction;
                nodeIt->second.cmdNodeId = 0;
                nodeIt->second.stopInProgress = true;
                nodeIt->second.maxStopTime = now + m_nodeTypeTable[nodeIt->second.nodeTypeId];
            }

            m_sendTimer.expires_from_now(boost::chrono::seconds(0));
            m_sendTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
                                                 {
                                                     if (error == boost::asio::error::operation_aborted)
                                                     {
                                                         return;
                                                     }
                                                     SendAllOutstanding();
                                                 }));
        }

        void StopExternalNode(Control::CommandAction cmdAction, int64_t cmdNodeId, int64_t toNodeId)
        {
            auto nodeIt = m_nodeTable.find(toNodeId);
            if (nodeIt == m_nodeTable.end())
            {
                // External node is already down
                return;
            }

            if (nodeIt->second.stopInProgress)
            {
                // There is a pending stop order to the external node
                return;
            }

            nodeIt->second.cmdAction = cmdAction;
            nodeIt->second.cmdNodeId = cmdNodeId;
            nodeIt->second.stopInProgress = true;
            nodeIt->second.maxStopTime = boost::chrono::steady_clock::now() +
                                         m_nodeTypeTable[nodeIt->second.nodeTypeId];

            m_sendTimer.expires_from_now(boost::chrono::seconds(0));
            m_sendTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
                                                 {
                                                     if (error == boost::asio::error::operation_aborted)
                                                     {
                                                         return;
                                                     }
                                                     SendAllOutstanding();
                                                 }));
        }

        void SendAllOutstanding()
        {
            bool externalNodeStopInProgress = false;

            auto now = boost::chrono::steady_clock::now();

            std::set<int64_t> pendingNodeTypeIds;

            for (auto nodeIt = m_nodeTable.begin(); nodeIt != m_nodeTable.end(); ++nodeIt)
            {
                if (nodeIt->second.stopInProgress)
                {
                    if (now < nodeIt->second.maxStopTime)
                    {
                        externalNodeStopInProgress = true;

                        if (m_systemStop)
                        {
                            // We are in system stop mode. Here we collect only the nodeTypeId:s so that
                            // just one message for each nodeTypeId will be sent to Communication.
                            pendingNodeTypeIds.insert(nodeIt->second.nodeTypeId);
                        }
                        else
                        {
                            // Send stop order to a single node
                            auto cmd = SerializeCmd(nodeIt->second.cmdAction, nodeIt->second.cmdNodeId);

                            m_communication.Send(nodeIt->first,
                                                 nodeIt->second.nodeTypeId,
                                                 boost::shared_ptr<char[]>(std::move(cmd.first)),
                                                 cmd.second, // size
                                                 m_stopOrderMsgTypeId,
                                                 true); // delivery guarantee

                            // We don't care about overflow towards Communication since we will resend
                            // outstanding stop commmands until the node disappears.
                        }
                    }
                    else
                    {
                        SEND_SYSTEM_LOG(Informational,
                                        << "Node " << nodeIt->first
                                        << " doesn't receive or doesn't act on a stop order");

                        nodeIt->second.stopInProgress = false;
                    }
                }
            }

            if (externalNodeStopInProgress)
            {
                for (auto nodeTypeIdIt = pendingNodeTypeIds.begin();
                     nodeTypeIdIt != pendingNodeTypeIds.end();
                     ++nodeTypeIdIt)
                {
                    auto cmd = SerializeCmd(m_localNodeStopCmdAction, 0);

                    m_communication.Send(0, // all nodes ...
                                         *nodeTypeIdIt, // ... of this type
                                         boost::shared_ptr<char[]>(std::move(cmd.first)),
                                         cmd.second, // size
                                         m_stopOrderMsgTypeId,
                                         true); // delivery guarantee
                }

                m_sendTimer.expires_from_now(boost::chrono::seconds(1));
                m_sendTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
                                                     {
                                                         if (error == boost::asio::error::operation_aborted)
                                                         {
                                                             return;
                                                         }
                                                         SendAllOutstanding();
                                                     }));
            }
            else if (m_localNodeStopInProgress)
            {
                // All external nodes are gone (or doesn't act on the stop order) and this node is
                // also on death row ... time to die.
                StopThisNode();
            }
        }

        void HandleStopOrderFromExternalNode(const int64_t from,
                                             const boost::shared_ptr<const char[]>& data,
                                             const size_t size)
        {
            // Test/debug flag
            if (m_ignoreCmd)
            {
                return;
            }

            auto cmd = DeserializeCmd(data.get(), size);

            m_localNodeStopCmdAction = cmd.first;

            if (cmd.second == 0)
            {
                // Receivied a system stop command

                if (m_systemStop)
                {
                    // We are already in "system stop mode"

                    if (from < m_communication.Id())
                    {
                        // Ignore the stop command if this node is in system stop mode itself
                        // and the sender has a lower nodeId.
                        return;
                    }
                }
                else
                {
                    m_stopSystemCb();
                }
            }

            StopThisNode();
        }

        void HandleStopNotificationFromExternalNode(const int64_t from)
        {
            // We got an indication that a node is about to stop. To get a nice, responsive system we
            // tell both the master instance and the slave instance of System Picture about this.

            m_sp.ExcludeNode(from);  // Master is the SP we have here

            // Send a message to dose_main so the slave SP can be told .
            m_doseMainCmdSender.StoppedNodeIndication(from);
        }

        void SendStopNotification()
        {
            // Send stop notifications to all nodes

            // First, collect all node types that we know about ...
            std::set<int64_t> nodeTypeIds;

            for (auto nodeIt = m_nodeTable.begin(); nodeIt != m_nodeTable.end(); ++nodeIt)
            {
                nodeTypeIds.insert(nodeIt->second.nodeTypeId);
            }

            // ... and send a stop notification to all nodes of each node type
            for (auto nodeTypeIdIt = nodeTypeIds.begin();
                 nodeTypeIdIt != nodeTypeIds.end();
                 ++nodeTypeIdIt)
            {
                m_communication.Send(0, // all nodes
                                     *nodeTypeIdIt,
                                     boost::shared_ptr<char[]>(new char[1]),  // dummy
                                     1, // size
                                     m_stopNotificationMsgTypeId,
                                     false); // no delivery guarantee
            }
        }

    };

    typedef StopHandlerBasic<Safir::Dob::Internal::Com::Communication,
                             Safir::Dob::Internal::SP::SystemPicture,
                             ControlCmdReceiver,
                             DoseMainCmdSender,
                             Config> StopHandler;

}
}
}
}



