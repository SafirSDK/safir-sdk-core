/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
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
          m_processInfoHandler(ioService),
          m_keepStateWhileDetached(distribution.GetNodeTypeConfiguration().GetThisNodeType().keepStateWhileDetached)
    {
        distribution.SubscribeNodeEvents(
            // InjectNode
            [](const std::string& /*nodeName*/, int64_t /*nodeId*/, int64_t /*nt*/, const std::string& /*dataAddr*/)
            {
                Connections::Instance().ForEachConnectionPtr([=](const ConnectionPtr& con){con->SignalIn();});
            },
            // ExcludeNode
            [keepState = m_keepStateWhileDetached, &distribution](int64_t nodeId, int64_t /*nt*/)
            {
                if (distribution.IsDetached() && keepState)
                {
                    Connections::Instance().SetDetachFlagForConnectionsFromNode(nodeId, true);
                }
                else
                {
                    Connections::Instance().RemoveConnectionFromNode(nodeId);
                }
            }
        );

        distribution.SubscribeAttachedDetached([this](bool sameSystem)
        {
            if (m_keepStateWhileDetached && !sameSystem)
            {
                lllog(5)<< L"ConnectionHandler - Attach to a new system. Remove all kept detached states, if any." << std::endl;
                Connections::Instance().RemoveDetachedConnections();
            }
            m_poolHandler.SetDetached(false);
        },
        [this]()
        {
            m_poolHandler.SetDetached(true);
        });

        for (auto nt = distribution.GetNodeTypeIds().cbegin(); nt != distribution.GetNodeTypeIds().cend(); ++nt)

        {
            m_sendQueues.insert(std::make_pair(*nt, SendQueue()));
            m_communication.SetQueueNotFullCallback([this](int64_t)
                                                    {
                                                        boost::asio::post(m_strand, [this]{HandleSendQueues();});
                                                    }, *nt);
        }

        m_communication.SetDataReceiver([this](int64_t /*fromNodeId*/, int64_t /*fromNodeType*/, const char *data, size_t /*size*/)
        {
            if (!m_running)
            {
                return;
            }

            boost::asio::post(m_strand, [this,data]
            {
                if (!m_running)
                {
                    return;
                }

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
                    lllog(4)<<"ConnectionHandler - RemoveConnection "<<state.GetSenderId()<<std::endl;
                    const ConnectionPtr connection = Connections::Instance().GetConnection(state.GetSenderId(), std::nothrow);
                    if (connection == nullptr)
                    {
                        lllog(4) << "We don't have that connection, discarding Disconnect" << std::endl;
                        return;
                    }

                    m_onAppEvent(connection, true);
                    Connections::Instance().RemoveConnection(connection);
                    m_poolHandler.HandleDisconnect(state.GetSenderId());
                }
            });
        }, ConnectionMessageDataTypeId, [](size_t s){return DistributionData::NewData(s);}, [](const char* data){DistributionData::DropReference(data);});

        //start connect thread
        m_connectEvent=false;
        m_connectionOutEvent=false;
        m_handleEventsNotified=false;
    }

    void ConnectionHandler::Start()
    {
        if (m_running.exchange(true))
        {
            return; // was already started
        }

        boost::asio::dispatch(m_strand, [this]
        {
            m_connectionThread = boost::thread([this]() {ConnectionThread();});
            m_poolHandler.Start();
        });
    }

    void ConnectionHandler::Stop()
    {
        lllog(5) << "ConnectionHandler: Entering Stop" << std::endl;
        if (!m_running.exchange(false))
        {
            lllog(5) << "ConnectionHandler: Already stopped" << std::endl;
            return; // was already stopped
        }

        boost::asio::post(m_strand, [this]
        {
            lllog(5) << "ConnectionHandler: Stop" << std::endl;
            m_processInfoHandler.Stop();
            m_poolHandler.Stop([this]{StopConnectionThread();});
        });
    }

    void ConnectionHandler::StopConnectionThread()
    {
        lllog(5) << "ConnectionHandler: Entering StopConnectionThread" << std::endl;
        boost::asio::dispatch(m_strand, [this]
        {
            m_connectionThreadRunning = false;
            if (m_connectionThread.get_id() != boost::thread::id())
            {
                lllog(5) << "ConnectionHandler: Stopping connection thread" << std::endl;
                //set the interrupt state so that when we generate the spurious signal
                //the thread will be interrupted at the interruption_point.
                m_connectionThread.interrupt();
                Connections::Instance().GenerateSpuriousConnectOrOutSignal();

                lllog(5) << "ConnectionHandler: Joining connection thread" << std::endl;
                m_connectionThread.join();
                m_connectionThread = boost::thread();
                lllog(5) << "ConnectionHandler: Connection thread succesfully stopped." << std::endl;
            }
        });
    }

    void ConnectionHandler::SendAll(const std::pair<Safir::Utilities::Internal::SharedConstCharArray, size_t>& data)
    {
        if (!m_connectionThreadRunning)
        {
            return;
        }

        for (auto vt = m_sendQueues.begin(); vt != m_sendQueues.end(); ++vt)
        {
            vt->second.push(data);
        }
        HandleSendQueues();
    }

    void ConnectionHandler::HandleSendQueues()
    {
        if (!m_connectionThreadRunning)
        {
            return;
        }

        for (auto ntQ = m_sendQueues.begin(); ntQ != m_sendQueues.end(); ++ntQ)
        {
            while (!ntQ->second.empty())
            {
                const auto& msg=ntQ->second.front();

                if (m_communication.Send(0, ntQ->first, msg.first, msg.second, ConnectionMessageDataTypeId, true))
                {
                    lllog(5)<<"ConnectionHandler - Send to to nodeType "<<ntQ->first<<std::endl;
                    ntQ->second.pop();
                }
                else
                {
                    lllog(5)<<"ConnectionHandler - Failed Send to to nodeType "<<ntQ->first<<std::endl;
                    break;
                }
            }
        }
    }

    void ConnectionHandler::ConnectionThread()
    {
        m_connectionThreadRunning = true;
        try
        {
            while (m_connectionThreadRunning)
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
                    boost::asio::post(m_strand, [this]{HandleEvents();});
                }
            }
        }
        catch (const boost::thread_interrupted&)
        {
            //do nothing, just exit
        }
    }

    // Calls to this method is only posted from the connectionThread
    void ConnectionHandler::HandleEvents()
    {
        if (!m_connectionThreadRunning)
        {
            return;
        }

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
        if (!m_connectionThreadRunning)
        {
            return;
        }
        lllout << "ConnectionHandler::HandleDisconnect: Disconnected " << connection->NameWithCounter() << " id = " << connection->Id() << std::endl;

        //try to handle some outstanding stuff (this does not guarantee that all gets handled,
        // e.g. communication overflow may stop something in here.).
        std::vector<ConnectionPtr> dummy;

        HandleConnectionOutEvent(connection,dummy);

        //Distribute the disconnection to communication if Connection resides on this node
        if (connection->Id().m_node==m_communication.Id())
        {
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
        if (!m_connectionThreadRunning)
        {
            return;
        }
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
