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
#pragma once
#include <atomic>
#include <boost/lexical_cast.hpp>
#include <boost/asio.hpp>
#include "Distribution.h"
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/ConnectionId.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/ConnectionAspectInjector.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/Internal/DistributionScopeReader.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    static const int64_t RegistrationStateDataTypeId=6915466164769792349; //DoseMain.RegistrationState
    static const int64_t EntityStateDataTypeId=5802524208372516084; //DoseMain.EntityState

    ///
    /// This class is responsible for distributing states to other nodes of a certain
    /// nodeType during normal execution.
    //
    /// PoolHandler keeps one instance of StateDistributor per nodeType.
    /// PooDistribution is handled separately see PoolDistribution.h
    ///
    template <class DistributionT>
    class StateDistributor
            :private Safir::Dob::Dispatcher
            ,private Safir::Dob::StopHandler
            ,private Safir::Dob::RegistrationSubscriber
            ,private Safir::Dob::EntitySubscriber
            ,private boost::noncopyable
    {
    public:
        StateDistributor(int64_t nodeType,
                         Distribution& distribution,
                         boost::asio::io_service::strand& strand,
                         const std::function<void(int64_t)>& checkPendingReg)
            :m_nodeType(nodeType)
            ,m_distribution(distribution)
            ,m_strand(strand)
            ,m_checkPendingReg(checkPendingReg)
            ,m_connections(static_cast<size_t>(Safir::Dob::NodeParameters::NumberOfContexts()))
        {
            m_dispatcherNotified=false;
            m_distribution.GetCommunication().SetQueueNotFullCallback([this](int64_t){OnDoDispatch();}, m_nodeType);
            for (auto it = m_connections.begin(); it != m_connections.end(); ++it)
            {
                it->reset(new SubcriptionConnection());
            }
        }

        void Start()
        {
            m_strand.dispatch([this]{SubscribeStates();});
        }

        void Stop()
        {
#if (!defined NDEBUG && !defined SAFIR_DISABLE_CHECK_STRAND)
            ENSURE(m_strand.running_in_this_thread(),
                   << "StateDistributor::Stop must be called from within the correct strand!");
#endif
            for (auto context=0; context<Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
            {
                m_connections[static_cast<size_t>(context)]->connection.Close();
            }
        }

        void CheckForPending(Typesystem::TypeId typeId)
        {
            m_strand.dispatch([this,typeId]
            {
                m_checkPendingReg(typeId);
            });
        }

    private:
        struct SubcriptionConnection
        {
            Safir::Dob::Connection connection;
            Safir::Dob::Internal::ConnectionPtr connectionPtr;
        };

        int64_t m_nodeType;
        DistributionT& m_distribution;
        boost::asio::io_service::strand& m_strand;
        std::function<void(int64_t)> m_checkPendingReg;
        std::vector<std::unique_ptr<SubcriptionConnection>> m_connections;
        std::atomic<bool> m_dispatcherNotified;

        static inline Safir::Utilities::Internal::SharedConstCharArray ToPtr(const DistributionData& d)
        {
            Safir::Utilities::Internal::SharedConstCharArray p(d.GetReference(), [](const char* ptr){DistributionData::DropReference(ptr);});
            return p;
        }

        //dispatch states
        void OnDoDispatch() override SAFIR_GCC_VISIBILITY_BUG_WORKAROUND
        {
            if (!m_dispatcherNotified)
            {
                m_dispatcherNotified=true;
                m_strand.dispatch([this]
                {
                    m_dispatcherNotified=false;

                    for (auto context = 0; context < Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
                    {
                        auto& queue=m_connections[static_cast<size_t>(context)]->connectionPtr->GetDirtySubscriptionQueue();
                        queue.Dispatch([this](const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
                        {
                            dontRemove=false;
                            DistributionData realState = subscription->GetState()->GetRealState();
                            if (!realState.IsNoState() && realState.GetType()==DistributionData::RegistrationState)
                            {
                                dontRemove=!subscription->DirtyFlag().Process([this, &subscription]
                                {
                                    return this->ProcessRegistrationState(subscription);
                                });
                            }
                            else
                            {
                                // Entity state
                                dontRemove=!subscription->DirtyFlag().Process([this, &subscription]
                                {
                                    return this->ProcessEntityState(subscription);
                                });
                            }
                            //dontRemove is true if we got an overflow, and if we did we
                            //dont want to keep sending anything to communication
                            exitDispatch = dontRemove;
                        });
                    }
                });
            }
        }

        //dummy consumer impl
        void OnStopOrder() override {}
        void OnRegistered(const Safir::Dob::Typesystem::TypeId, const Safir::Dob::Typesystem::HandlerId&) override {}
        void OnUnregistered(const Safir::Dob::Typesystem::TypeId, const Safir::Dob::Typesystem::HandlerId&) override {}
        void OnNewEntity(const Safir::Dob::EntityProxy) override {}
        void OnUpdatedEntity(const Safir::Dob::EntityProxy) override {}
        void OnDeletedEntity(const Safir::Dob::EntityProxy, const bool) override {}

        void SubscribeStates()
        {
            // dose_main must subscribe for states in all contexts
            for (auto context=0; context<Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
            {
                auto& subCon=m_connections[static_cast<size_t>(context)];
                subCon->connection.Open(L"dose_main", L"states_"+boost::lexical_cast<std::wstring>(m_nodeType), context, this, this);

                subCon->connection.SubscribeRegistration(Entity::ClassTypeId,
                                                        Typesystem::HandlerId::ALL_HANDLERS,
                                                        true, //includeSubclasses
                                                        false, //restartSubscription
                                                        this);

                subCon->connection.SubscribeRegistration(Service::ClassTypeId,
                                                        Typesystem::HandlerId::ALL_HANDLERS,
                                                        true, //includeSubclasses
                                                        false, //restartSubscription
                                                        this);

                ConnectionAspectInjector(subCon->connection).SubscribeEntity(Entity::ClassTypeId,
                                                                            true,  //includeUpdates,
                                                                            true,  //includeSubclasses
                                                                            false, //restartSubscription
                                                                            false, //wantsGhostDelete
                                                                            false, //wantsLastState
                                                                            false, //doesntWantSourceIsPermanentStore
                                                                            true,  //wantsAllStateChanges
                                                                            false, //timestampChangeInfo
                                                                            this);


                auto connectionName = ConnectionAspectMisc(subCon->connection).GetConnectionName();
                subCon->connectionPtr=Connections::Instance().GetConnectionByName(Typesystem::Utilities::ToUtf8(connectionName));
            }
        }

        bool ProcessEntityState(const SubscriptionPtr& subscription)
        {
            if (subscription->GetState()->IsDetached())
            {
                // Never send detached states
                return true;
            }

            bool complete = true;

            //Real state
            {
                const DistributionData currentState = subscription->GetCurrentRealState();
                const DistributionData lastState = subscription->GetLastRealState();

                if (currentState != lastState && !currentState.IsNoState())
                {
                    //if sender is not local node or object is local we just ignore it.
                    if (currentState.GetSenderId().m_node!=m_distribution.GetCommunication().Id() ||
                        DistributionScopeReader::Instance().IsLocal(currentState.GetTypeId()))
                    {
                        subscription->SetLastRealState(currentState);
                    }
                    else
                    {
                        bool success=m_distribution.GetCommunication().Send
                            (0, m_nodeType, ToPtr(currentState), currentState.Size(), EntityStateDataTypeId, true);
                        if (success)
                        {
                            subscription->SetLastRealState(currentState);
                        }
                        else
                        {
                            complete = false;
                        }
                    }
                }
            }

            //injection state
            {
                const DistributionData currentState = subscription->GetCurrentInjectionState();
                const DistributionData lastState = subscription->GetLastInjectionState();

                if (currentState != lastState && !currentState.IsNoState())
                {
                    //if sender is not local node or object is local we just ignore it.
                    if (currentState.GetSenderId().m_node!=m_distribution.GetCommunication().Id() ||
                        DistributionScopeReader::Instance().IsLocal(currentState.GetTypeId()))
                    {
                        subscription->SetLastInjectionState(currentState);
                    }
                    else
                    {
                        bool success=m_distribution.GetCommunication().Send
                            (0, m_nodeType, ToPtr(currentState), currentState.Size(), EntityStateDataTypeId, true);
                        if (success)
                        {
                            subscription->SetLastInjectionState(currentState);
                        }
                        else
                        {
                            complete = false;
                        }
                    }
                }
            }

            return complete;
        }

        bool ProcessRegistrationState(const SubscriptionPtr& subscription)
        {
            if (subscription->GetState()->IsDetached())
            {
                // Never send detached states
                return true;
            }

            const DistributionData currentState = subscription->GetCurrentRealState();
            const DistributionData lastState = subscription->GetLastRealState();

            if (currentState == lastState)
            {
                return true;
            }

            //if sender is not local node we dont send to dosecom, and just ignore it.
            if (currentState.GetSenderId().m_node!=m_distribution.GetCommunication().Id())
            {
                return true;
            }

            //dont send local objects
            if (DistributionScopeReader::Instance().IsLocal(currentState.GetTypeId()))
            {
                return true;
            }

            //dont send local context
            if (Safir::Dob::NodeParameters::LocalContexts(currentState.GetSenderId().m_contextId))
            {
                return true;
            }

            bool success=m_distribution.GetCommunication().Send
                (0, m_nodeType, ToPtr(currentState), currentState.Size(), RegistrationStateDataTypeId, true);

            if (success)
            {
                m_checkPendingReg(currentState.GetTypeId());
                subscription->SetLastRealState(currentState);

                lllog(8) << "DoseMain.StateDistributor - RegistrationState sent to other nodes " << std::endl << currentState.Image() << std::endl;
            }

            return success;
        }
    };
}
}
}
