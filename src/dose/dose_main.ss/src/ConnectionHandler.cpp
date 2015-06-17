/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://www.consoden.se)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "ConnectionHandler.h"
#include "PendingRegistrationHandler.h"
#include "RequestHandler.h"
#include "Distribution.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace
{
    static const int64_t ConnectionMessageDataTypeId=4477521173098643793; //DoseMain.ConnectionMessage
}

    ConnectionHandler::ConnectionHandler(boost::asio::io_service& ioService,
                                         Distribution& distribution,
                                         const std::function<void(const ConnectionPtr& connection, bool disconnecting)>& onAppEvent,
                                         const std::function<void(int64_t)>& checkPendingReg,
                                         const std::function<void(const std::string& str)>& logStatus)
        : m_strand(ioService),
          m_communication(distribution.GetCommunication()),
          m_onAppEvent(onAppEvent),
          m_poolHandler(m_strand, distribution, checkPendingReg, logStatus),
          m_processInfoHandler(ioService, distribution)
    {
        distribution.SubscribeNodeEvents(
            [](const std::string&, int64_t /*id*/, int64_t /*nt*/, const std::string&)
            {
                Connections::Instance().ForEachConnectionPtr([=](const ConnectionPtr& con){con->SignalIn();});
            },
            [](int64_t id, int64_t)
            {
                Connections::Instance().RemoveConnectionFromNode(id, [=](const ConnectionPtr& con)
                {
                    if (!con->IsLocal())
                        con->SetNodeDown();
                });
            }
        );

        for (auto nt : distribution.GetNodeTypeIds())
        {
            m_sendQueues.insert(std::make_pair(nt, SendQueue()));
            m_communication.SetQueueNotFullCallback([=](int64_t){HandleSendQueues();}, nt);
        }

        m_communication.SetDataReceiver([=](int64_t /*fromNodeId*/, int64_t /*fromNodeType*/, const char *data, size_t /*size*/)
        {
            m_strand.post([=]
            {
                const DistributionData state=DistributionData::ConstConstructor(new_data_tag, data);
                DistributionData::DropReference(data);
                if (state.GetType()==DistributionData::Action_Connect)
                {
                    lllog(4)<<"ConnectionHandler - AddConnection "<<state.GetConnectionName()<<std::endl;
                    Connections::Instance().AddConnection(state.GetConnectionName(),
                                                          state.GetCounter(),
                                                          state.GetSenderId().m_contextId,
                                                          state.GetSenderId());
                    m_poolHandler.HandleConnect(state.GetSenderId());
                }
                else if (state.GetType()==DistributionData::Action_Disconnect)
                {
                    lllog(4)<<"ConnectionHandler - RemoveConnection "<<state.GetConnectionName()<<std::endl;
                    const ConnectionPtr connection = Connections::Instance().GetConnection(state.GetSenderId(), std::nothrow);
                    m_onAppEvent(connection, true);
                    Connections::Instance().RemoveConnection(connection);
                    m_poolHandler.HandleDisconnect(state.GetSenderId());
                }
            });
        }, ConnectionMessageDataTypeId, [=](size_t s){return DistributionData::NewData(s);});

        //start connect thread
        m_connectEvent=false;
        m_connectionOutEvent=false;
        m_handleEventsNotified=false;
    }

    void ConnectionHandler::Start()
    {
        m_strand.dispatch([=]
        {
            m_connectionThread = boost::thread([this]() {ConnectionThread();});
            m_poolHandler.Start();
        });
    }

    void ConnectionHandler::Stop()
    {
        m_strand.post([=]
        {
            m_poolHandler.Stop();
            m_processInfoHandler.Stop();

            if (m_connectionThread.get_id() != boost::thread::id())
            {
                //set the interrupt state so that when we generate the spurious signal
                //the thread will be interrupted at the interruption_point.
                m_connectionThread.interrupt();
                Connections::Instance().GenerateSpuriousConnectOrOutSignal();

                m_connectionThread.join();
                m_connectionThread = boost::thread();
            }
        });
    }

    void ConnectionHandler::SendAll(const std::pair<boost::shared_ptr<const char[]>, size_t>& data)
    {
        for (auto& vt : m_sendQueues)
        {
            vt.second.push(data);
        }
        HandleSendQueues();
    }

    void ConnectionHandler::HandleSendQueues()
    {
        for (auto& ntQ : m_sendQueues) //ntQ = pair<nodeType, SendQueue>
        {
            while (!ntQ.second.empty())
            {
                const auto& msg=ntQ.second.front();

                if (m_communication.Send(0, ntQ.first, msg.first, msg.second, ConnectionMessageDataTypeId, true))
                {
                    lllog(5)<<"ConnectionHandler - Send to to nodeType "<<ntQ.first<<std::endl;
                    ntQ.second.pop();
                }
                else
                {
                    lllog(5)<<"ConnectionHandler - Failed Send to to nodeType "<<ntQ.first<<std::endl;
                    break;
                }
            }
        }
    }

    void ConnectionHandler::ConnectionThread()
    {
        try
        {
            for (;;)
            {
                bool connect, connectionOut;
                Connections::Instance().WaitForDoseMainSignal(connect, connectionOut);

                boost::this_thread::interruption_point();

                //Note that we cannot just do this->m_connectionOut = connectionOut, since that might clear flags that
                //have not been handled yet.
                if (connect)
                {
                    lllog(9) << "ConnectionThread - CONNECT "<< std::endl;
                    m_connectEvent=true;
                }
                if (connectionOut)
                {
                    lllog(9) << "ConnectionThread - CONNECT_OUT "<< std::endl;
                    m_connectionOutEvent=true;
                }

                if (m_handleEventsNotified==false)
                {
                    lllog(9) << "ConnectionThread - NOTIFY "<< std::endl;
                    m_handleEventsNotified=true;
                    m_strand.post([this]{HandleEvents();});
                }
            }
        }
        catch (const boost::thread_interrupted&)
        {
            //do nothing, just exit
        }
    }

    void ConnectionHandler::HandleEvents()
    {
        lllog(9) << "ConnectionHandler::HandleEvents "<< std::endl;


        m_handleEventsNotified=false;

        bool gotConnectEvent = false;
        bool gotConnectOutEvent = false;
        //if we have a connect event we want to ensure that
        //we handle any outstanding disconnects ("died" flags in the
        //connections), so we fake a connectionOutEvent.
        //we must "harvest" the shared flag only once in this routine, so we
        //use a local variable to avoid having a connector signal the connect
        //event after we've already passed the out event handling code, but before
        //we get to the connect event handling code.

        bool oldConnectEvent = m_connectEvent.exchange(false);
        if (oldConnectEvent)
        {
            m_connectionOutEvent=true;
            gotConnectEvent = true;
        }

        bool oldConnectOutEvent=m_connectionOutEvent.exchange(false);
        if (oldConnectOutEvent)
        {
            gotConnectOutEvent = true;
        }

        if (gotConnectOutEvent)
        {
            std::vector<ConnectionPtr> deadConnections;
            Connections::Instance().HandleConnectionOutEvents([&](const ConnectionPtr& c){HandleConnectionOutEvent(c, deadConnections);});

            for (std::vector<ConnectionPtr>::iterator it = deadConnections.begin();
                 it != deadConnections.end(); ++it)
            {
                HandleDisconnect(*it);
                Connections::Instance().RemoveConnection(*it);
            }
        }

        //we do this after connectionOutEvents
        if (gotConnectEvent)
        {
            Connections::Instance().HandleConnect([this](const ConnectionPtr& connection)
            {
                lllog(4) << "ConnectionHandler::HandleConnect: Sending new connection "
                       << connection->NameWithCounter() << " id = " << connection->Id() << std::endl;

                SendAll(ConnectDataPtr(connection->Id(), connection->NameWithoutCounter(), connection->Counter()));

                m_processInfoHandler.ConnectionAdded(connection);

            });
        }
    }

    void ConnectionHandler::HandleDisconnect(const ConnectionPtr & connection)
    {
        lllout << "ConnectionHandler::HandleDisconnect: Disconnected " << connection->NameWithCounter() << " id = " << connection->Id() << std::endl;

        //try to handle some outstanding stuff (this does not guarantee that all gets handled,
        // e.g. communication overflow may stop something in here.).
        std::vector<ConnectionPtr> dummy;

        HandleConnectionOutEvent(connection,dummy);

        //Distribute the disconnection to communication if Connection resides on this node
        if (connection->Id().m_node==m_communication.Id())
        {
            //TODO: remove this when we send dose_mains connections to remote nodes.
            if (std::string(connection->NameWithoutCounter()).find(";dose_main;") != std::string::npos)
            {
                return;
            }

            lllog(4) << "ConnectionHandler::HandleDisconnect: Sending disconnect for connection "
                   << connection->NameWithCounter() << " id = " << connection->Id() << std::endl;

            SendAll(DisconnectDataPtr(connection->Id()));
        }

        // Remove the connection from the processInfo structure
        m_processInfoHandler.ConnectionRemoved(connection);

        m_onAppEvent(connection, true);
    }


    void ConnectionHandler::HandleConnectionOutEvent(const ConnectionPtr & connection, std::vector<ConnectionPtr>& deadConnections)
    {
        m_onAppEvent(connection, false);

        if (connection->IsDead())
        {
            lllout << "Connection is dead: " << connection->NameWithCounter() << ", disconnecting."<< std::endl;
            deadConnections.push_back(connection);
        }
    }
}
}
}
