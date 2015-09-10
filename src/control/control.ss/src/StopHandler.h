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
#include <Safir/Utilities/Internal/Id.h>
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
    template<typename Communication, typename SP, typename ControlCmdReceiver>
    class StopHandlerBasic
    {
    public:

        typedef std::function<void()> StopSafirNodeCb;
        typedef std::function<void()> StopSystemCb;

        StopHandlerBasic(boost::asio::io_service&   ioService,
                         Communication&             communication,
                         SP&                        sp,
                         const StopSafirNodeCb      stopSafirNodeCb,
                         const StopSystemCb         stopSystemCb,
                         bool                       ignoreCmd)
            : m_communication(communication),
              m_sp(sp),
              m_stopSafirNodeCb(stopSafirNodeCb),
              m_stopSystemCb(stopSystemCb),
              m_ignoreCmd(ignoreCmd),
              m_controlCmdMsgTypeId(LlufId_Generate64("Safir.Dob.Control.CmdMsgType")),
              m_sendTimer(ioService),
              m_localNodeStopInProgress(false),
              m_systemStop(false)

        {
            // Set up callbacks to receive stop commands locally via IPC
            m_controlCmdReceiver.reset(new ControlCmdReceiver(ioService,
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

                                                              }));

            // Set up callback to receive stop commands via control channel from an external node.
            communication.SetDataReceiver([this](const int64_t from,
                                                 const int64_t /*nodeTypeId*/,
                                                 const char* const data,
                                                 const size_t size)
                                          {
                                              ReceiveFromExternalNode(from,
                                                                      boost::shared_ptr<const char[]>(data),size);
                                          },
                                          m_controlCmdMsgTypeId,
                                          [](size_t size){return new char[size];},
                                          [](const char * data){delete[] data;});
        }

        void Start()
        {
            m_controlCmdReceiver->Start();
        }

        void Stop()
        {
            m_controlCmdReceiver->Stop();
            m_sendTimer.cancel();
        }

        void AddNode(const int64_t nodeId, const int64_t nodeTypeId)
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

            m_nodeTable.insert(std::make_pair(nodeId, Node(nodeTypeId)));
        }

        void RemoveNode(const int64_t nodeId)
        {
            m_nodeTable.erase(nodeId);
        }

    private:

        Communication&  m_communication;
        SP&             m_sp;

        const StopSafirNodeCb m_stopSafirNodeCb;
        const StopSystemCb m_stopSystemCb;

        const bool  m_ignoreCmd;

        std::unique_ptr<ControlCmdReceiver> m_controlCmdReceiver;

        const int64_t   m_controlCmdMsgTypeId;

        boost::asio::steady_timer m_sendTimer;

        bool                    m_localNodeStopInProgress;
        Control::CommandAction  m_localNodeStopCmdAction;

        bool m_systemStop;

        struct Node
        {
            Node(int64_t _nodeTypeId)
                : nodeTypeId(_nodeTypeId),
                  stopInProgress(false)
            {
            }

            int64_t                 nodeTypeId;
            Control::CommandAction  cmdAction;
            int64_t                 cmdNodeId;
            bool                    stopInProgress;
        };

        std::unordered_map<int64_t, Node> m_nodeTable;

        void StopThisNode(CommandAction  /*cmdAction*/)
        {
            if (m_ignoreCmd)
            {
                return;
            }

            // Skicka "Jag kommer gå ned" här

            // Systemlog?

            m_stopSafirNodeCb();
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
                return;
            }

            StopThisNode(cmdAction);
        }

        void HandleSystemStop(CommandAction cmdAction)
        {
            std::wcout << "CTRL: Received system cmd. Action: " << cmdAction << std::endl;

            m_systemStop = true;

            m_localNodeStopInProgress = true;
            m_localNodeStopCmdAction = cmdAction;

            m_stopSystemCb();  // Notify other parts that we got a system stop order

            // Initiate stop of all known nodes
            for (auto nodeIt = m_nodeTable.begin(); nodeIt != m_nodeTable.end(); ++nodeIt)
            {
                nodeIt->second.cmdAction = cmdAction;
                nodeIt->second.cmdNodeId = 0;
                nodeIt->second.stopInProgress = true;
            }

            m_sendTimer.expires_from_now(boost::chrono::seconds(0));
            m_sendTimer.async_wait([this](const boost::system::error_code& error)
                                   {
                                       if (error == boost::asio::error::operation_aborted)
                                       {
                                           return;
                                       }
                                       SendAllOutstanding();
                                   });

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

            m_sendTimer.expires_from_now(boost::chrono::seconds(0));
            m_sendTimer.async_wait([this](const boost::system::error_code& error)
                                   {
                                       if (error == boost::asio::error::operation_aborted)
                                       {
                                           return;
                                       }
                                       SendAllOutstanding();
                                   });

        }

        void SendAllOutstanding()
        {
            bool externalNodeStopInProgress = false;

            for (auto nodeIt = m_nodeTable.begin(); nodeIt != m_nodeTable.end(); ++nodeIt)
            {
                if (nodeIt->second.stopInProgress)
                {
                    auto cmd = SerializeCmd(nodeIt->second.cmdAction, nodeIt->second.cmdNodeId);

                    m_communication.Send(nodeIt->first,
                                         nodeIt->second.nodeTypeId,
                                         boost::shared_ptr<char[]>(std::move(cmd.first)),
                                         cmd.second, // size
                                         m_controlCmdMsgTypeId,
                                         true); // delivery guarantee

                    // We don't care about overflow towards Communication since we will resend
                    // outstanding stop commmands until the node disapears.
                    externalNodeStopInProgress = true;
                }
            }

            if (externalNodeStopInProgress)
            {
                m_sendTimer.expires_from_now(boost::chrono::seconds(2));
                m_sendTimer.async_wait([this](const boost::system::error_code& error)
                                       {
                                           if (error == boost::asio::error::operation_aborted)
                                           {
                                               return;
                                           }
                                           SendAllOutstanding();
                                       });
            }
            else if (m_localNodeStopInProgress)
            {
                // All external nodes are gone and this node is also on death row ... time to die.
                StopThisNode(m_localNodeStopCmdAction);
            }
        }

        void ReceiveFromExternalNode(const int64_t /*from*/,
                                     const boost::shared_ptr<const char[]>& data,
                                     const size_t size)
        {
            auto cmd = DeserializeCmd(data.get(), size);

            if (cmd.second == 0)
            {
                // Receivied a system stop command
                m_stopSystemCb();
            }

            StopThisNode(cmd.first);
        }

    };

    typedef StopHandlerBasic<Safir::Dob::Internal::Com::Communication,
                             Safir::Dob::Internal::SP::SystemPicture,
                             ControlCmdReceiver> StopHandler;

}
}
}
}


