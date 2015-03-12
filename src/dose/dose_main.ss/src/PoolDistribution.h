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
#pragma once
#include <unordered_map>
#include <functional>
#include <boost/make_shared.hpp>
#include <boost/asio.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    static const int64_t PoolDistributionInfoDataTypeId=-3446507522969672286; //DoseMain.PoolDistributionInfo
    enum class PoolDistributionInfo : uint8_t {RequestPd = 1, PdComplete = 2};

    ///
    /// This class handles a pool distribution to a single node.
    ///
    class PoolDistribution
    {
    public:
        //    void PoolHandler::PoolDistributionWorker()
        //    {
        //#if 0 //stewart
        //        boost::shared_ptr<ExceptionInfo> exceptionInfo;

        //        try
        //        {
        //            lllout << "Pool distribution thread started" << std::endl;

        //        m_poolDistributionThreadId = boost::this_thread::get_id();
        //        m_threadMonitor->StartWatchdog(m_poolDistributionThreadId, L"pool distribution thread");

        //            // Wait until persistent data is ready.
        //            if (!m_persistHandler->IsPersistentDataReady())
        //            {
        //                lllout << "Pool distribution thread is waiting for persistence data from DOPE" << std::endl;
        //                while (!m_persistHandler->IsPersistentDataReady())
        //                {
        //                    boost::this_thread::sleep_for(boost::chrono::milliseconds(10)); //sleep is interruption point

        //                m_threadMonitor->KickWatchdog(m_poolDistributionThreadId);
        //                }
        //                lllout << "Pool distribution thread thinks DOPE is done." << std::endl;
        //            }


        //            lllout << "Distributing connections" << std::endl;
        //            // get connections
        //            ConnectionMsgsToSend.clear();
        //            Connections::Instance().ForEachConnection(boost::bind(&PoolHandler::PDConnection,this,_1));
        //            // send connections
        //            ConnectionMsgs::const_iterator msgIter;
        //            for ( msgIter = ConnectionMsgsToSend.begin( ) ; msgIter != ConnectionMsgsToSend.end( ) ; msgIter++ )
        //            {
        //                boost::this_thread::interruption_point();
        //                m_ecom->SendPoolDistributionData(*msgIter, *m_threadMonitor, m_poolDistributionThreadId);
        //            }
        //            ConnectionMsgsToSend.clear();
        //            lllout << "Setting up connection in each context for pool distribution" << std::endl;

        //            DummyDispatcher dispatcher;
        //            DummySubscriber dummySubscriber;

        //            // Distribute all contexts
        //            for (ContextId context = 0; context < Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
        //            {
        //                boost::this_thread::interruption_point();
        //                Safir::Dob::Connection pdConnection;

        //                //Note the connection name, we do NOT want to get special treatment for being
        //                //in dose_main, since we're a different thread. But we want to be allowed in
        //                //always, so don't change connection string "dose_main_pd" because it is always allowed to connect,
        //                // even before we let -1 connections in.
        //                pdConnection.Open(L"dose_main_pd",L"pool_distribution", context,NULL, &dispatcher);
        //                StartSubscriptions(pdConnection, dummySubscriber,true,false);
        //                const std::wstring connectionName = ConnectionAspectMisc(pdConnection).GetConnectionName();
        //                ConnectionPtr connection = Connections::Instance().GetConnectionByName(Typesystem::Utilities::ToUtf8(connectionName));

        //                connection->GetDirtySubscriptionQueue().Dispatch(boost::bind(&PoolHandler::PDDispatchSubscription,this,_1,_2,_3));
        //            }

        //            lllout << "Pool distribution completed (calling ecom::PoolDistributionCompleted" << std::endl;

        //            m_ecom->PoolDistributionCompleted(*m_threadMonitor, m_poolDistributionThreadId);
        //        }
        //        catch (const boost::thread_interrupted&)
        //        {
        //            //do nothing but exit the thread.
        //            return;
        //        }
        //        catch (const Dob::Typesystem::Internal::CommonExceptionBase& e)
        //        {
        //            exceptionInfo.reset
        //                (new ExceptionInfo(e.GetTypeId(),
        //                                   Dob::Typesystem::Utilities::ToUtf8(e.GetExceptionInfo())));
        //        }
        //        catch (const std::exception & e)
        //        {
        //            exceptionInfo.reset(new ExceptionInfo(0,
        //                                                  e.what()));
        //        }
        //        catch (...)
        //        {
        //            exceptionInfo.reset(new ExceptionInfo(0,
        //                                                  "Unknown exception (caught as ...)"));
        //        }

        //        if (exceptionInfo != NULL)
        //        {
        //            SEND_SYSTEM_LOG(Alert,
        //                            << "An exception was caught inside PoolHandler::PoolDistributionWorker!"
        //                            << "The exception will be rethrown in the main thread");
        //        }

        //        lllout << "Signalling pool distribution completed"<< std::endl;

        //        //No need for m_isNotified flag guard, since this happens very seldom.
        //        m_ioService.post(boost::bind(&PoolHandler::PDCompletedHandler,this,exceptionInfo));

        //        m_threadMonitor->StopWatchdog(m_poolDistributionThreadId);
        //#endif
        //    }



        //    void PoolHandler::PDCompletedHandler(const boost::shared_ptr<ExceptionInfo>& exceptionInfo)
        //    {
        //        lllout << "Pool distribution is completed"<< std::endl;
        //        m_pdThread.join();
        //        m_pdThread = boost::thread();

        //        if (exceptionInfo != NULL)
        //        {
        //            SEND_SYSTEM_LOG(Alert,
        //                            << "An exception occurred in pool distribution, rethrowing.");
        //            exceptionInfo->Rethrow();
        //        }
        //        m_connectionHandler->MaybeSignalConnectSemaphore();
        //    }

        //    void PoolHandler::PDDispatchSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
        //    {
        //        boost::this_thread::interruption_point();

        //        exitDispatch = false;
        //        dontRemove = false;

        //        DistributionData realState = subscription->GetState()->GetRealState();

        //        if (!realState.IsNoState() && realState.GetType() == DistributionData::RegistrationState)
        //        {
        //            // Registration state
        //            subscription->DirtyFlag().Process(boost::bind(&PoolHandler::PDProcessRegistrationState,
        //                                                          this,
        //                                                          boost::cref(subscription)));
        //        }
        //        else
        //        {
        //            // Entity state
        //            subscription->DirtyFlag().Process(boost::bind(&PoolHandler::PDProcessEntityState,
        //                                                          this,
        //                                                          boost::cref(subscription)));
        //        }
        //    }

        //    bool PoolHandler::PDProcessRegistrationState(const SubscriptionPtr & /*subscription*/)
        //    {
        //#if 0 //stewart
        //        if (subscription->GetLastRealState().IsNoState())
        //        {
        //            const DistributionData state = subscription->GetCurrentRealState();


        //            if (!state.IsNoState())
        //            {
        //                //Local states are to be sent to other nodes.
        //                //Unregistration states (regardless of whether they are local or not) are always sent.
        //                if (IsLocal(state) ||
        //                    !state.IsRegistered())
        //                {
        //                    m_ecom->SendPoolDistributionData(state, *m_threadMonitor, m_poolDistributionThreadId);
        //                }
        //            }
        //            subscription->SetLastRealState(state); //update this so we only dispatch this state once (see "if" above)
        //        }
        //#endif

        //        return true; //PD send always succeeds
        //    }

        //    void PoolHandler::PDConnection(const Connection & connection)
        //    {
        //        if (connection.IsLocal())
        //        {
        //            if (std::string(connection.NameWithoutCounter()).find(";dose_main;") != std::string::npos)
        //            {
        //                return;
        //            }

        //            //We're only interested in local node connections
        //            DistributionData msg(connect_message_tag,
        //                                 connection.Id(),
        //                                 connection.NameWithoutCounter(),
        //                                 connection.Counter());

        //            // send later to avoid locking up shmem
        //            ConnectionMsgsToSend.push_back(msg);
        //        }
        //    }

        //    bool IsLocal(const DistributionData& state)
        //    {
        //        return state.GetSenderId().m_node == g_thisNode;
        //    }

        //    bool PoolHandler::PDProcessEntityState(const SubscriptionPtr & /*subscription*/)
        //    {
        //#if 0 //stewart
        //        // All nodes send ghost and injection data on PD!
        //        // Do not send updates

        //        //Real state
        //        if (subscription->GetLastRealState().IsNoState())
        //        {
        //            const DistributionData currentState = subscription->GetCurrentRealState();

        //            if (!currentState.IsNoState())
        //            {
        //                //Send all local states
        //                //send all ghosts (the owner node is probably down...)
        //                //send all delete states (so that new nodes get the correct timestamps)
        //                if (IsLocal(currentState) ||
        //                    currentState.GetEntityStateKind() == DistributionData::Ghost ||
        //                    !currentState.HasBlob())
        //                {
        //                    m_ecom->SendPoolDistributionData(currentState, *m_threadMonitor, m_poolDistributionThreadId);
        //                }
        //            }
        //            subscription->SetLastRealState(currentState);
        //        }

        //        //injection state
        //        if (subscription->GetLastInjectionState().IsNoState())
        //        {
        //            const DistributionData currentState = subscription->GetCurrentInjectionState();

        //            if (!currentState.IsNoState())
        //            {
        //                m_ecom->SendPoolDistributionData(currentState, *m_threadMonitor, m_poolDistributionThreadId);
        //                subscription->SetLastInjectionState(currentState);
        //            }
        //        }
        //#endif
        //        return true; //PD send always succeeds
        //    }


    private:

    };

    //********************************************************************************************************

    ///
    /// Responsible for sending poolDistributionRequests to all nodes at start-up.
    /// When all poolDistributions are received, this class calls the pdComplete callback.
    ///
    template <class CommunicationT>
    class PoolDistributionRequestor
    {
    public:
        PoolDistributionRequestor(boost::asio::io_service::strand& strand,
                                  CommunicationT& communication,
                                  std::unordered_map<int64_t, int64_t>& nodes,
                                  std::function<void()> pdComplete)
            :m_strand(strand)
            ,m_communication(communication)
            ,m_nodes(nodes)
            ,m_pdComplete(pdComplete)
        {
            for (auto& vt : m_nodes)
            {
                m_poolDistributionRequests.insert({vt.first, false});
            }
            m_strand.post([=]{SendPoolDistributionRequests();});
        }

        void ReceivedPoolDistributionCompleteFrom(int64_t nodeId)
        {
            m_poolDistributionRequests.erase(nodeId);
            if (m_poolDistributionRequests.empty())
            {
                m_pdComplete();
            }
        }

    private:
        boost::asio::io_service::strand& m_strand;
        CommunicationT& m_communication;
        std::unordered_map<int64_t, int64_t>& m_nodes;
        std::function<void()> m_pdComplete; //signal pdComplete after all our pdRequests has reported pdComplete
        std::unordered_map<int64_t, bool> m_poolDistributionRequests;

        void SendPoolDistributionRequests()
        {
            auto req=boost::make_shared<char[]>(sizeof(PoolDistributionInfo));
            (*reinterpret_cast<PoolDistributionInfo*>(req.get()))=PoolDistributionInfo::RequestPd;

            bool unsentRequests=false;

            for (auto it=m_poolDistributionRequests.begin(); it!=m_poolDistributionRequests.end();)
            {
                auto nodeIt=m_nodes.find(it->first);
                if (nodeIt==m_nodes.end())
                {
                    //node does no longer exist, remove it since we no longer expect to get a pd from that node.
                    it=m_poolDistributionRequests.erase(it);
                    continue;
                }

                if (it->second)
                {
                    //pd request has already been successfully sent in previous calls to this method
                    ++it;
                    continue;
                }

                //try to send pd request to the node
                if (m_communication.Send(nodeIt->first, nodeIt->second, req, sizeof(PoolDistributionInfo), PoolDistributionInfoDataTypeId, true))
                {
                    it->second=true; //pd request has been sent
                    ++it;
                }
                else
                {
                    unsentRequests=true; //could not send request right now due to overflow
                    ++it;
                }
            }

            if (unsentRequests)
            {
                //there are nodes that has not got an pd request yet, retry later
                m_strand.post([=]{SendPoolDistributionRequests();}); //set timer instead
            }
        }
    };
}
}
}
