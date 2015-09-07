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

        typedef std::function<void()> StopSafirCb;

        StopHandlerBasic(boost::asio::io_service&   ioService,
                         Communication&             communication,
                         SP&                        sp,
                         const StopSafirCb          stopSafirCb,
                         bool                       ignoreCmd)
            : m_communication(communication),
              m_sp(sp),
              m_stopSafirCb(stopSafirCb),
              m_ignoreCmd(ignoreCmd),
              m_controlCmdMsgTypeId(LlufId_Generate64("Safir.Dob.Control.CmdMsgType")),
              m_resendTimer(ioService),
              resendTimerRunning(false)

        {
            // Set up callbacks to receive stop commands locally via IPC
            m_controlCmdReceiver.reset(new ControlCmdReceiver(ioService,
                                                              [this](Control::CommandAction cmdAction, int64_t nodeId)
                                                              {
                                                                  if (nodeId == m_communication.Id())
                                                                  {
                                                                      HandleNodeCmd(cmdAction);
                                                                  }
                                                                  else
                                                                  {
                                                                      SendToExternalNode(cmdAction, nodeId);
                                                                  }

                                                              },
                                                              [this](Control::CommandAction cmdAction)
                                                              {
                                                                  HandleSystemCmd(cmdAction);
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
        }

        void AddNode(const int64_t nodeId, const int64_t nodeTypeId)
        {
            m_nodeTable.insert(std::make_pair(nodeId, Node(nodeTypeId)));
        }

        void RemoveNode(const int64_t nodeId)
        {
            m_nodeTable.erase(nodeId);
        }

        void HandleNodeCmd(CommandAction  /*cmdAction*/)
        {
            if (m_ignoreCmd)
            {
                return;
            }

            // Skicka "Jag kommer gå ned" här

            // Systemlog?

            m_stopSafirCb();
        }

        void HandleSystemCmd(CommandAction  cmdAction)
        {
            std::wcout << "CTRL: Received system cmd. Action: " << cmdAction << std::endl;

            if (m_ignoreCmd)
            {
                return;
            }

//            HandleControlCmd(cmdAction);
        }

    private:

        Communication&  m_communication;
        SP&             m_sp;

        const StopSafirCb m_stopSafirCb;

        const bool  m_ignoreCmd;

        std::unique_ptr<ControlCmdReceiver> m_controlCmdReceiver;

        const int64_t   m_controlCmdMsgTypeId;

        boost::asio::steady_timer m_resendTimer;
        bool resendTimerRunning;

        struct Node
        {
            Node(int64_t _nodeTypeId)
                : nodeTypeId(_nodeTypeId),
                  outstandingCmd(false),
                  communicationOverflow(false)
            {
            }

            int64_t nodeTypeId;
            Control::CommandAction cmdAction;
            bool    outstandingCmd;
            bool    communicationOverflow;
        };

        std::unordered_map<int64_t, Node> m_nodeTable;

        void Resend()
        {
            resendTimerRunning = false;

            for (auto it = m_nodeTable.begin(); it != m_nodeTable.end(); ++it)
            {
                if (it->second.communicationOverflow)
                {
                    SendToExternalNode(it->second.cmdAction, it->first);
                }
            }
        }

        void SendToExternalNode(Control::CommandAction cmdAction, int64_t nodeId)
        {
            auto nodeIt = m_nodeTable.find(nodeId);
            if (nodeIt == m_nodeTable.end())
            {
                // External node is already down
                return;
            }

            nodeIt->second.cmdAction = cmdAction;

            auto cmd = SerializeCmd(cmdAction);

            auto ok = m_communication.Send(nodeId,
                                           nodeIt->second.nodeTypeId,
                                           boost::shared_ptr<char[]>(std::move(cmd.first)),
                                           cmd.second, // size
                                           m_controlCmdMsgTypeId,
                                           true); // delivery guarantee

            if (!ok)
            {
                nodeIt->second.communicationOverflow = true;
                if (!resendTimerRunning)
                {
                    m_resendTimer.async_wait([this](const boost::system::error_code& error)
                                            {
                                                if (!!error && error == boost::asio::error::operation_aborted)
                                                {
                                                    // We are stopped, just return
                                                    return;
                                                }

                                                Resend();

                                            });
                    resendTimerRunning = true;

                }
            }
            else
            {
                nodeIt->second.outstandingCmd = true;
                nodeIt->second.communicationOverflow = false;
            }

        }

        void ReceiveFromExternalNode(const int64_t from,
                                     const boost::shared_ptr<const char[]>& data,
                                     const size_t size)
        {
            auto cmdAction = DeserializeCmd(data.get(),  size);

            HandleNodeCmd(cmdAction);

        }

        //void ControlApp::SendControlCmd(std::pair<std::unique_ptr<char[]>, size_t> cmd)
        //{
        //    // Send the request to all known nodes
        //    for (auto nt = m_conf.nodeTypesParam.cbegin(); nt != m_conf.nodeTypesParam.cend(); ++nt)
        //    {
        //        auto ok = m_communication->Send(0, // all nodes...
        //                                        nt->id, // ... of this node type
        //                                        boost::shared_ptr<char[]>(std::move(cmd.first)),
        //                                        cmd.second, // size
        //                                        m_controlCmdMsgTypeId,
        //                                        true); // delivery guarantee
        //        if (ok)
        //        {
        //            lllog(3) << "DOSE_MAIN: Sent HavePersistenceDataRequest to nodeId " << node->first << std::endl;
        //        }
        //        else
        //        {
        //           m_unsentRequests.insert(*node);
        //        }
        //    }


        //}

    };

    typedef StopHandlerBasic<Safir::Dob::Internal::Com::Communication,
                             Safir::Dob::Internal::SP::SystemPicture,
                             ControlCmdReceiver> StopHandler;

}
}
}
}



