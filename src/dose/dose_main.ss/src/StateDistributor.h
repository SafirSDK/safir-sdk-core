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
#include <atomic>
#include <boost/lexical_cast.hpp>
#include <boost/asio.hpp>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/ConnectionId.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/ConnectionAspectInjector.h>
#include <Safir/Dob/NodeParameters.h>

#include <boost/function.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    static const int64_t RegistrationStateDataTypeId=6915466164769792349; //DoseMain.RegistrationState
    static const int64_t EntityStateDataTypeId=5802524208372516084; //DoseMain.EntityState

    template <class CommunicationT>
    class StateDistributor
            :private Safir::Dob::Dispatcher
            ,private Safir::Dob::StopHandler
            ,private Safir::Dob::RegistrationSubscriber
            ,private Safir::Dob::EntitySubscriber
            ,private boost::noncopyable
    {
    public:
        StateDistributor(CommunicationT& com, boost::asio::io_service::strand& strand, int64_t nodeType)
            :m_communication(com)
            ,m_strand(strand)
            ,m_nodeType(nodeType)
            ,m_connections(static_cast<size_t>(Safir::Dob::NodeParameters::NumberOfContexts()))
        {
            m_dispatcherNotified=false;
        }

        void Start()
        {
            m_strand.dispatch([=]{SubscribeStates();});
        }

        void Stop()
        {
            m_strand.dispatch([=]
            {
                for (auto context=0; context<Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
                {
                    m_connections[static_cast<size_t>(context)].connection.Close();
                }
            });
        }

    private:
        struct SubcriptionConnection
        {
            Safir::Dob::Connection connection;
            Safir::Dob::Internal::ConnectionPtr connectionPtr;
        };

        CommunicationT& m_communication;
        boost::asio::io_service::strand& m_strand;
        int64_t m_nodeType;
        std::vector<SubcriptionConnection> m_connections;
        std::atomic_bool m_dispatcherNotified;

        //dispatch states
        virtual void OnDoDispatch()
        {
            if (!m_dispatcherNotified)
            {
                m_dispatcherNotified=true;
                m_strand.dispatch([=]
                {
                    m_dispatcherNotified=false;

                    for (auto context = 0; context < Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
                    {
                        auto& queue=m_connections[static_cast<size_t>(context)].connectionPtr->GetDirtySubscriptionQueue();
                        queue.Dispatch([=](const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
                        {
                            DistributionData realState = subscription->GetState()->GetRealState();
                            if (!realState.IsNoState() && realState.GetType()==DistributionData::RegistrationState)
                            {
                                // Registration state
                                dontRemove=!subscription->DirtyFlag().Process([this, &subscription]{return ProcessRegistrationState(subscription);});
                            }
                            else
                            {
                                // Entity state
                                dontRemove=!subscription->DirtyFlag().Process([this, &subscription]{return ProcessEntityState(subscription);});
                            }
                            //dontRemove is true if we got an overflow, and if we did we dont want to keep sending anything to dose_com.
                            exitDispatch = dontRemove;
                        });
                    }
                });
            }
        }

        //dummy consumer impl
        virtual void OnStopOrder() {}        
        virtual void OnRegistered(const Safir::Dob::Typesystem::TypeId, const Safir::Dob::Typesystem::HandlerId&) {}
        virtual void OnUnregistered(const Safir::Dob::Typesystem::TypeId, const Safir::Dob::Typesystem::HandlerId&) {}
        virtual void OnNewEntity(const Safir::Dob::EntityProxy) {}
        virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy) {}
        virtual void OnDeletedEntity(const Safir::Dob::EntityProxy, const bool) {}

        void SubscribeStates()
        {
            // dose_main must subscribe for states in all contexts
            for (auto context=0; context<Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
            {
                auto& subCon=m_connections[static_cast<size_t>(context)];
                subCon.connection.Open(L"dose_main", L"states_"+boost::lexical_cast<std::wstring>(m_nodeType), context, this, this);

                subCon.connection.SubscribeRegistration(Entity::ClassTypeId,
                                                        Typesystem::HandlerId::ALL_HANDLERS,
                                                        true, //includeSubclasses
                                                        false, //restartSubscription
                                                        this);

                subCon.connection.SubscribeRegistration(Service::ClassTypeId,
                                                        Typesystem::HandlerId::ALL_HANDLERS,
                                                        true, //includeSubclasses
                                                        false, //restartSubscription
                                                        this);

                ConnectionAspectInjector(subCon.connection).SubscribeEntity(Entity::ClassTypeId,
                                                                            true,  //includeUpdates,
                                                                            true,  //includeSubclasses
                                                                            false, //restartSubscription
                                                                            false, //wantsGhostDelete
                                                                            false, //wantsLastState
                                                                            false, //doesntWantSourceIsPermanentStore
                                                                            true,  //wantsAllStateChanges
                                                                            false, //timestampChangeInfo
                                                                            this);


                auto connectionName = ConnectionAspectMisc(subCon.connection).GetConnectionName();
                subCon.connectionPtr=Connections::Instance().GetConnectionByName(Typesystem::Utilities::ToUtf8(connectionName));
            }
        }

        //    bool IsLocal(const DistributionData& state)
        //    {
        //        return state.GetSenderId().m_node == g_thisNode;
        //    }

            bool ProcessEntityState(const SubscriptionPtr& subscription)
            {
                return true;
//                bool complete = true;

//                //Real state
//                {
//                    const DistributionData currentState = subscription->GetCurrentRealState();
//                    const DistributionData lastState = subscription->GetLastRealState();

//                    if (currentState != lastState && !currentState.IsNoState())
//                    {
//                        //if sender is not local node we dont send to dosecom, and just ignore it.
//                        if (!IsLocal(currentState))
//                        {
//                            subscription->SetLastRealState(currentState);
//                        }
//                        else
//                        {
//        #if 0 //stewart
//                            if (ecom->Send(currentState))
//        #endif
//                            {
//                                subscription->SetLastRealState(currentState);
//                            }
//        #if 0 //stewart
//                            else
//                            {
//                                complete = false;
//                            }
//        #endif
//                        }
//                    }
//                }

//                //injection state
//                {
//                    const DistributionData currentState = subscription->GetCurrentInjectionState();
//                    const DistributionData lastState = subscription->GetLastInjectionState();

//                    if (currentState != lastState && !currentState.IsNoState())
//                    {
//                        //if sender is not local node we dont send to dosecom, and just ignore it.
//                        if (!IsLocal(currentState))
//                        {
//                            subscription->SetLastInjectionState(currentState);
//                        }
//                        else
//                        {
//#if 0 //stewart
//                            if (ecom->Send(currentState))
//#endif
//                            {
//                                subscription->SetLastInjectionState(currentState);
//                            }
//#if 0 //stewart
//                            else
//                            {
//                                complete = false;
//                            }
//#endif
//                        }
//                    }
//                }

//                return complete;
            }



            bool ProcessRegistrationState(const SubscriptionPtr& subscription)
            {
                return true;
//                const DistributionData currentState = subscription->GetCurrentRealState();
//                const DistributionData lastState = subscription->GetLastRealState();

//                if (currentState == lastState)
//                {
//                    return true;
//                }

//                //if sender is not local node we dont send to dosecom, and just ignore it.
//                if (currentState.GetSenderId().m_node!=m_communication.Id())
//                {
//                    return true;
//                }
//#if 0 //stewart
//                const bool success = ecom->Send(currentState);

//                if (success)
//#endif
//                {
//                    pendingHandler->CheckForPending(currentState.GetTypeId());
//                    subscription->SetLastRealState(currentState);
//                }

//#if 0 //stewart
//                return success;
//#else
//                return true;
//#endif
            }
    };
}
}
}
