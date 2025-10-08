/******************************************************************************
*
* Copyright Saab AB, 2023 (http://safirsdkcore.com)
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
#include <queue>
#include <functional>
#include <memory>
#include <string>
#include <boost/lexical_cast.hpp>
#include <boost/asio.hpp>
#include <boost/chrono.hpp>
#include <boost/asio/steady_timer.hpp>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/ConnectionAspectInjector.h>
#include <Safir/Utilities/Internal/SharedCharArray.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/Internal/DistributionScopeReader.h>
#include <Safir/Dob/LowMemoryException.h>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4245)
#  pragma warning (disable: 4127)
#  pragma warning (disable: 4701)
#  pragma warning (disable: 4100)
#  pragma warning (disable: 4141)
#  pragma warning (disable: 4267)
#  pragma warning (disable: 4189)
#endif

#include "PoolSyncInfo.pb.h"

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    ///
    /// Handles a single pool distribution to a specific node. This class is only to be used by
    /// PoolDistributor. To start a new pool distribution to a node, use PoolDistributor.AddPoolDistribution.
    /// NOTE: All public methods in this class must be called from the strand passed in the ctor.
    ///
    template <class DistributionT>
    class PoolDistribution
            :public std::enable_shared_from_this< PoolDistribution<DistributionT> >
            ,public Safir::Dob::EntitySubscriber
            ,public Safir::Dob::RegistrationSubscriber
            ,public Safir::Dob::Dispatcher
    {
    public:
        PoolDistribution(int64_t nodeId,
                         int64_t nodeType,
                         const std::shared_ptr<SmartSyncState>& syncState,
                         boost::asio::io_context::strand& strand,
                         DistributionT& distribution,
                         const std::function<void(int64_t)>& completionHandler)
            :m_nodeId(nodeId)
            ,m_nodeType(nodeType)
            ,m_syncState(syncState)
            ,m_receiverIsLightNode(distribution.IsLightNode(nodeType))
            ,m_strand(strand)
            ,m_timer(strand.context())
            ,m_distribution(distribution)
            ,m_completionHandler(completionHandler)
        {
        }

        ~PoolDistribution()
        {
            if (m_cancelled && !m_cancelledCalled)
            {
                lllog(5) << L"PoolHandler: Pooldistribution was cancelled but was destructed without calling the cancelHandler!" << std::endl;
            }
            try
            {
                if (m_dobConnection.IsOpen())
                {
                    m_dobConnection.Close();
                }
            }
            catch (const Safir::Dob::LowMemoryException& e)
            {
                lllog(5) << L"PoolHandler: Pooldistribution to " << m_nodeId << L" got LowMemoryException in destructor. " << e.GetMessage() << std::endl;
                SEND_SYSTEM_LOG(Error, << L"PoolHandler: Pooldistribution to " << m_nodeId << L" got LowMemoryException in destructor. " << e.GetMessage());
            }
        }

        void Run() SAFIR_GCC_VISIBILITY_BUG_WORKAROUND
        {
            // Starts the PD to a node.
            // Info will be sent in this order:
            // 1. Disconnects, Unregistrations
            // 2. New Connections
            // 3. New registrationStates, updated and new EntityStates (uses a dob subscription, sends in the same order it is handled over from subscription)
            // 4. Deleted EntityStates

            // Always called from m_strand
            if (m_started || HasBeenCancelled())
            {
                lllog(5)<<"PoolHandler: Pooldistribution never started since it is already running or has been cancelled, nodeId="<<m_nodeId<<std::endl;
                return;
            }

            m_started=true;

            // post a new job for this pd
            boost::asio::post(m_strand, [self = this->shared_from_this()]
            {
                lllog(5)<<"PoolHandler: Start PoolDistribution to "<<self->m_nodeId<<std::endl;

                if (self->HasBeenCancelled())
                {
                    lllog(5)<<"PoolHandler: PD was cancelled "<<self->m_nodeId<<std::endl;
                    // This PD has been cancelled
                    return;
                }

                if (SharedMemoryObject::GetMemoryLevel() >= MemoryLevel::Low)
                {
                    lllog(5) << L"PoolHandler: Abort PoolDistribution to " << self->m_nodeId << L" since this node is low on shared memory." <<std::endl;
                    SEND_SYSTEM_LOG(Error, << L"PoolHandler: Abort PoolDistribution to " << self->m_nodeId << L" since this node is low on shared memory.");
                    self->SendPdAbort();
                    return;
                }

                if (Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().LogLevel() >= 5)
                {
                    std::wostringstream os;
                    self->m_syncState->ToString(os);
                    lllog(5) << L"Start PD using smartSyncState: " << std::endl << os.str() << std::endl;
                }

                //If receiver is lightNode, we must send deleted connections and unregistrations
                if (self->m_receiverIsLightNode)
                {
                    // get deleted connections
                    for (const auto& con : self->m_syncState->connections)
                    {
                        ConnectionId connId(self->m_distribution.GetNodeId(), con.context, con.connectionId);
                        auto conPtr = Connections::Instance().GetConnection(connId, std::nothrow);
                        if (!conPtr || conPtr->Counter() != con.counter)
                        {
                            // connection does not exist anymore, or has new counter which means it has been disconnected and then reconnected.
                            self->m_prioritizedStates.push(DistributionData(disconnect_message_tag, connId));
                        }
                        else
                        {
                            // the connection exists, check for any deleted registrations
                            auto registrations = conPtr->GetRegisteredHandlers();
                            for (const auto& reg : con.registrations)
                            {
                                bool exists = std::any_of(std::begin(registrations), std::end(registrations), [&reg](auto r) {
                                    return r.typeId == reg.typeId && r.handlerId == reg.handlerId && r.regTime == reg.registrationTime;
                                    });

                                if (!exists)
                                {
                                    // registration state doesn't exist any more, add a unregister state to the send queue
                                    self->m_prioritizedStates.push(
                                        DistributionData(registration_state_tag,
                                            ConnectionId(connId.m_node, connId.m_contextId, -1),
                                            reg.typeId,
                                            Typesystem::HandlerId(reg.handlerId),
                                            InstanceIdPolicy::HandlerDecidesInstanceId, // Dummy for an unreg state
                                            DistributionData::Unregistered,
                                            LamportTimestamp::MakeTimestamp(reg.registrationTime, connId.m_node))
                                    );
                                }
                            }
                        }
                    }
                }
                

                //collect all connections on this node that receiver does not already have
                Connections::Instance().ForEachConnection([self](const Connection& connection)
                {
                    const auto localContext=Safir::Dob::NodeParameters::LocalContexts(connection.Id().m_contextId);
                    const auto connectionOnThisNode=connection.IsLocal();
                    const auto detachedConnection = connection.IsDetached();

                    if (!detachedConnection && !localContext && connectionOnThisNode)
                    {
                        bool exists = std::any_of(std::begin(self->m_syncState->connections), std::end(self->m_syncState->connections), [&connection](auto c){
                            return connection.Id().m_id == c.connectionId && connection.Id().m_contextId == c.context && connection.Counter() == c.counter;
                        });
                        if (!exists)
                        {
                            self->m_prioritizedStates.push(DistributionData(connect_message_tag,
                                                                connection.Id(),
                                                                connection.NameWithoutCounter(),
                                                                connection.Counter()));
                        }
                    }
                });

                self->SendConnectionsAndUnregistrations(); //will trigger the sequence SendConnectionsAndUnregistrations() -> SendStates() -> SendPdComplete()
            });
        }

        void Cancel(const std::function<void()>& cancelled = []{})
        {
            lllog(1) << "PoolHandler: Cancel called for PoolDistribution to " << m_nodeId << std::endl;
            m_onCancelled = cancelled;
            m_cancelled = true;
        }

        int64_t NodeId() const {return m_nodeId;}
        int64_t NodeType() const {return m_nodeType;}
        bool IsStarted() const {return m_started;}

        std::wstring ToString() const
        {
            std::wostringstream os;
            os << L"PD to node: " << m_nodeId << std::boolalpha << L", started: " << m_started << L", cancelled: " << m_cancelled;
            return os.str();
        }

    private:
        static const int64_t PoolDistributionInfoDataTypeId=-3446507522969672286; //DoseMain.PoolDistributionInfo
        static const int64_t ConnectionMessageDataTypeId=4477521173098643793; //DoseMain.ConnectionMessage
        static const int64_t RegistrationStateDataTypeId=6915466164769792349; //DoseMain.RegistrationState
        static const int64_t EntityStateDataTypeId=5802524208372516084; //DoseMain.EntityState

        const int64_t m_nodeId;
        const int64_t m_nodeType;
        std::shared_ptr<SmartSyncState> m_syncState;
        const bool m_receiverIsLightNode;
        boost::asio::io_context::strand& m_strand;
        boost::asio::steady_timer m_timer;
        DistributionT& m_distribution;
        std::function<void(int64_t)> m_completionHandler;
        
        // States that must be sent before normal reg/entity states
        // Queue layout will be:
        // +---------------------------------------------------------+
        // | deleted connections | unregistrations | new connections |
        // +---------------------------------------------------------+
        std::queue<DistributionData> m_prioritizedStates; 
        
        Safir::Dob::Connection m_dobConnection;

        bool m_started = false;
        bool m_cancelled = false;
        std::function<void()> m_onCancelled;
        bool m_cancelledCalled = false;

        inline bool HasBeenCancelled()
        {
            if (m_cancelled)
            {
                lllog(5)<<L"PoolHandler: PoolDistribution to " << m_nodeId << L" has been cancelled" <<std::endl;
                m_onCancelled();
                m_cancelledCalled = true;
                m_onCancelled = []{}; // prevent calling completion handler again in vain
                return true;
            }
            return false;
        }

        void SendConnectionsAndUnregistrations()
        {
            if (HasBeenCancelled())
            {
                return;
            }

            while (!m_prioritizedStates.empty())
            {
                const DistributionData& d=m_prioritizedStates.front();
                auto dataTypeId = d.GetType() == DistributionData::RegistrationState ? RegistrationStateDataTypeId : ConnectionMessageDataTypeId;

                Safir::Utilities::Internal::SharedConstCharArray p(d.GetReference(), [=](const char* ptr){DistributionData::DropReference(ptr);});

                if (m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, p, d.Size(), dataTypeId, true))
                {
                    m_prioritizedStates.pop();
                }
                else
                {
                    break;
                }
            }

            if (m_prioritizedStates.empty())
            {
                boost::asio::post(m_strand, [self = this->shared_from_this()]
                {
                    self->SendStates(0);
                });
            }
            else
            {
                SetTimer([self = this->shared_from_this()]{self->SendConnectionsAndUnregistrations();});
            }
        }

        void SendStates(int context)
        {
            if (HasBeenCancelled())
            {
                return;
            }

            if (context>=Safir::Dob::NodeParameters::NumberOfContexts())
            {
                boost::asio::post(m_strand, [self = this->shared_from_this()]
                {
                    self->SendPdComplete();
                });
                return;
            }

            try
            {
                if (m_dobConnection.IsOpen())
                {
                    m_dobConnection.Close();
                }

                auto instancePart = boost::lexical_cast<std::wstring>(m_distribution.GetNodeId()) + L"_" + boost::lexical_cast<std::wstring>(m_nodeId);
                m_dobConnection.Open(L"PD", instancePart, context, NULL, this);

                m_dobConnection.SubscribeRegistration(Entity::ClassTypeId,
                                                      Typesystem::HandlerId::ALL_HANDLERS,
                                                      true, //includeSubclasses
                                                      false, //restartSubscription
                                                      this);


                m_dobConnection.SubscribeRegistration(Service::ClassTypeId,
                                                      Typesystem::HandlerId::ALL_HANDLERS,
                                                      true, //includeSubclasses
                                                      false, //restartSubscription
                                                      this);

                ConnectionAspectInjector(m_dobConnection).SubscribeEntity(Entity::ClassTypeId,
                                                                          false, //includeUpdates,
                                                                          true,  //includeSubclasses
                                                                          false, //restartSubscription
                                                                          false, //wantsGhostDelete
                                                                          false, //wantsLastState
                                                                          false, //doesntWantSourceIsPermanentStore
                                                                          true,  //wantsAllStateChanges
                                                                          false, //timestampChangeInfo
                                                                          this);
            }
            catch (const Safir::Dob::LowMemoryException& e)
            {
                lllog(5) << L"PoolHandler: Pooldistribution to " << m_nodeId << L" failed. Caught LowMemoryException. " << e.GetMessage() << std::endl;
                SEND_SYSTEM_LOG(Error, << L"PoolHandler: Pooldistribution to " << m_nodeId << L" failed. Caught LowMemoryException. " << e.GetMessage());
                boost::asio::post(m_strand, [self = this->shared_from_this()]{self->SendPdAbort();});
                return;
            }

            auto connectionName=ConnectionAspectMisc(m_dobConnection).GetConnectionName();
            auto conPtr=Connections::Instance().GetConnectionByName(Typesystem::Utilities::ToUtf8(connectionName));
            DispatchStates(conPtr, context);
        }

        void DispatchStates(const Safir::Dob::Internal::ConnectionPtr& conPtr, int context) SAFIR_GCC_VISIBILITY_BUG_WORKAROUND
        {
            if (HasBeenCancelled())
            {
                return;
            }

            bool overflow=false;
            conPtr->GetDirtySubscriptionQueue().Dispatch([this, conPtr, context, &overflow](const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove)
            {
                dontRemove=false;
                DistributionData realState = subscription->GetState()->GetRealState();
                if (!realState.IsNoState() && !DistributionScopeReader::Instance().IsLocal(realState.GetTypeId()))
                {
                    if (realState.GetType()==DistributionData::RegistrationState)
                    {
                        // Registration state
                        dontRemove=!subscription->DirtyFlag().Process([this, context, &subscription]{return ProcessRegistrationState(context, subscription);});
                    }
                    else
                    {
                        // Entity state
                        dontRemove=!subscription->DirtyFlag().Process([this, context, &subscription]{return ProcessEntityState(context, subscription);});
                    }
                }
                //dontRemove is true if we got an overflow, and if we did we dont want to keep sending anything to communication.
                exitDispatch = dontRemove;
                overflow=exitDispatch;
            });

            if (overflow)
            {
                SetTimer([self = this->shared_from_this(), conPtr,context]{self->DispatchStates(conPtr, context);});
            }
            else //continue with next context
            {
                boost::asio::dispatch(m_strand, [self = this->shared_from_this(), context]{self->SendDeletedEntityStates(context);});
            }
        }

        SmartSyncState::Entity* GetNextEntityInContext(int32_t context)
        {
            for (auto& con : m_syncState->connections)
            {
                if (con.context == context)
                {
                    for (auto& reg : con.registrations)
                    {
                        if (!reg.entities.empty())
                        {
                            return &reg.entities.front();
                        }
                    }
                }
            }

            return nullptr; // no entity found in context
        }

        void SendDeletedEntityStates(int context)
        {
            if (HasBeenCancelled())
            {
                return;
            }

            if (m_receiverIsLightNode)
            {
                while (true)
                {
                    // any entities left in m_syncState is states that were not found anymore and a deleteEntity must be sent.
                    SmartSyncState::Entity* entity = GetNextEntityInContext(context);

                    if (entity == nullptr)
                    {
                        break; // we are done with this context
                    }

                    // Send delete state
                    // if send ok, remove entityIt
                    // if overflow, set timer and keep entityIt
                    // There is no existing real state, create a new real state 'deleted'
                    const auto thisNodeId = m_distribution.GetNodeId();

                    DistributionData deleteState(entity_state_tag,
                        // Correct node number and context but no connection id for delete states
                        ConnectionId(thisNodeId, context, -1),
                        entity->registration->typeId,
                        Safir::Dob::Typesystem::HandlerId(entity->registration->handlerId),
                        LamportTimestamp::MakeTimestamp(entity->registration->registrationTime, thisNodeId), //regtime
                        Safir::Dob::Typesystem::InstanceId(entity->instanceId),
                        LamportTimestamp::MakeTimestamp(entity->creationTime, thisNodeId),      // creation time
                        DistributionData::Real,
                        true,                          // deleted by owner
                        false,                         // false => source is not permanent store
                        NULL);                         // no blob

                    // To make the receiver accept the state, we have to increment the original version number
                    deleteState.SetVersion(VersionNumber(static_cast<uint16_t>(entity->version)));
                    deleteState.IncrementVersion();

                    Safir::Utilities::Internal::SharedConstCharArray p(deleteState.GetReference(), [=](const char* ptr) {DistributionData::DropReference(ptr); });
                    if (m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, p, deleteState.Size(), EntityStateDataTypeId, true))
                    {
                        entity->registration->DeleteEntity(entity->instanceId);
                    }
                    else
                    {
                        SetTimer([self = this->shared_from_this(), context] {self->SendDeletedEntityStates(context); });
                        return;
                    }
                }
            }

            boost::asio::dispatch(m_strand, [self = this->shared_from_this(), context] {self->SendStates(context + 1); });
        }

        void SendPdComplete()
        {
            if (HasBeenCancelled())
            {
                return;
            }

            try
            {
                m_dobConnection.Close();
            }
            catch (const Safir::Dob::LowMemoryException& e)
            {
                lllog(5) << L"PoolHandler: Pooldistribution to " << m_nodeId << L" got LowMemoryException when closing dobConnection. The PD managed to finish before the error occurred. " << e.GetMessage() << std::endl;
                SEND_SYSTEM_LOG(Error, << L"PoolHandler: Pooldistribution to " << m_nodeId << L" got LowMemoryException when closing dobConnection. The PD managed to finish before the error occurred. " << e.GetMessage());
            }

            Pd::PoolSyncInfo poolSyncInfo;
            poolSyncInfo.set_messagetype(Pd::PoolSyncInfo_PdMsgType::PoolSyncInfo_PdMsgType_PdComplete);
            const auto size = poolSyncInfo.ByteSizeLong();
            Safir::Utilities::Internal::SharedCharArray payload = Safir::Utilities::Internal::MakeSharedArray(size);
            google::protobuf::uint8* buf=reinterpret_cast<google::protobuf::uint8*>(const_cast<char*>(payload.get()));
            poolSyncInfo.SerializeWithCachedSizesToArray(buf);

            if (m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, payload, size, PoolDistributionInfoDataTypeId, true))
            {
                lllog(5)<<"PoolHandler: Completed PoolDistribution to "<<m_nodeId<<std::endl;
                m_completionHandler(m_nodeId);
            }
            else
            {
                SetTimer([self = this->shared_from_this()]{self->SendPdComplete();});
            }
        }

        void SendPdAbort()
        {
            if (HasBeenCancelled())
            {
                return;
            }

            try
            {
                // Try to close the connection
                m_dobConnection.Close();
            }
            catch (const Safir::Dob::LowMemoryException&){}

            Pd::PoolSyncInfo poolSyncInfo;
            poolSyncInfo.set_messagetype(Pd::PoolSyncInfo_PdMsgType::PoolSyncInfo_PdMsgType_PdAbort);
            const auto size = poolSyncInfo.ByteSizeLong();
            Safir::Utilities::Internal::SharedCharArray payload = Safir::Utilities::Internal::MakeSharedArray(size);
            google::protobuf::uint8* buf=reinterpret_cast<google::protobuf::uint8*>(const_cast<char*>(payload.get()));
            poolSyncInfo.SerializeWithCachedSizesToArray(buf);

            if (m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, payload, size, PoolDistributionInfoDataTypeId, true))
            {
                lllog(5) << L"PoolHandler: PoolDistribution to " << m_nodeId
                         << L" failed since this node is low on shared memory. PdAbort has been sent." <<std::endl;
                //m_distribution.ForceExcludeNode(m_nodeId);
                m_completionHandler(m_nodeId);
            }
            else
            {
                SetTimer([self = this->shared_from_this()]{self->SendPdAbort();});
            }
        }

        bool ProcessEntityState(int context, const SubscriptionPtr& subscription)
        {
            if (HasBeenCancelled())
            {
                return true;
            }
            // All nodes send ghost and injection data on PD!
            // Do not send updates
            lllog(5)<<L"PoolHandler: PoolDistribution.ProcessEntityState: " << subscription->GetCurrentRealState().Image() << std::endl;

            if (subscription->GetState()->IsDetached())
            {
                // never send detached states in PD
                lllog(5)<<L"PoolHandler: PoolDistribution - Dont send detached entity state."<<std::endl;
                return true;
            }

            bool success=true;

            //Real state
            if (subscription->GetLastRealState().IsNoState())
            {
                const DistributionData currentState = subscription->GetCurrentRealState();

                if (m_receiverIsLightNode) // receiver is light node
                {
                    //Send all states owned by someone on this node
                    //It is a real state, not ghost or other junk
                    //It is an existing state, not a deleted state
                    if (currentState.GetSenderId().m_node == m_distribution.GetCommunication().Id() &&
                        currentState.GetEntityStateKind() == DistributionData::Real &&
                        currentState.HasBlob())
                    {
                        auto entity = m_syncState->GetEntity(context, currentState.GetHandlerId().GetRawValue(), currentState.GetTypeId(), currentState.GetInstanceId().GetRawValue());
                        bool entityChanged = entity == nullptr ||
                            entity->version != currentState.GetVersion().GetRawValue() ||
                            entity->creationTime != currentState.GetCreationTime().GetRawValue();

                        // Only send states that is new or changed.
                        if (entityChanged)
                        {
                            if (!m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, ToPtr(currentState), currentState.Size(), EntityStateDataTypeId, true))
                            {
                                success = false;
                            }
                        }

                        // Remove states that has been handled. When all states are sent we will send delete states for any remining states in m_syncState->entities
                        if (success && entity != nullptr)
                        {
                            entity->registration->DeleteEntity(entity->instanceId);
                        }
                    }

                }
                else if (m_distribution.IsLightNode()) // this node is light node
                {
                    //Send all states owned by someone on this node
                    //It is a real state, not ghost or other junk
                    //It is an existing state, not a deleted state
                    if (currentState.GetSenderId().m_node==m_distribution.GetCommunication().Id() &&
                        currentState.GetEntityStateKind() == DistributionData::Real &&
                            currentState.HasBlob())
                    {                        
                        if (!m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, ToPtr(currentState), currentState.Size(), EntityStateDataTypeId, true))
                        {
                            success=false;
                        }
                    }
                }
                else // sender and receiver are normal nodes
                {
                    //Send all states owned by someone on this node
                    //send all ghosts (the owner node is probably down...)
                    //send all delete states (so that new nodes get the correct timestamps)
                    if (currentState.GetSenderId().m_node==m_distribution.GetCommunication().Id() ||
                        currentState.GetEntityStateKind() == DistributionData::Ghost ||
                        !currentState.HasBlob())
                    {
                        if (!m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, ToPtr(currentState), currentState.Size(), EntityStateDataTypeId, true))
                        {
                            success=false;
                        }
                    }
                }

                if (success)
                {
                    subscription->SetLastRealState(currentState);
                }
            }

            //injection state
            if (subscription->GetLastInjectionState().IsNoState() && !m_distribution.IsLightNode() && !m_receiverIsLightNode)
            {
                const DistributionData currentState = subscription->GetCurrentInjectionState();

                if (!currentState.IsNoState())
                {
                    if (!m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, ToPtr(currentState), currentState.Size(), EntityStateDataTypeId, true))
                    {
                        success=false;
                    }
                    else
                    {
                        subscription->SetLastInjectionState(currentState);
                    }
                }
            }

            return success;
        }

        bool ProcessRegistrationState(int /*context*/, const SubscriptionPtr& subscription)
        {
            bool success=true;

            if (subscription->GetState()->IsDetached())
            {
                // never send detached states in PD
                lllog(5)<<L"PoolHandler: PoolDistribution - Dont send detached registration state."<<std::endl;
                return true;
            }

            if (subscription->GetLastRealState().IsNoState())
            {
                const DistributionData state = subscription->GetCurrentRealState();

                if (!state.IsNoState())
                {
                    const auto ownerOnThisNode = state.GetSenderId().m_node==m_distribution.GetCommunication().Id();
                    const auto isRegistration = state.IsRegistered();

                    // RegistrationStates owned by someone on this node are to be sent to other nodes.
                    // Unregistration states are always sent, since ghosts may be in WaitingStates
                    // waiting for the unreg (only applies between two normal nodes).
                    // However UnregStates are never sent to lightNodes here, since they have alreayd been sent
                    // in SendConnectionsAndUnregistrations()
                    //if (ownerOnThisNode || (normalNode && !m_receiverIsLightNode && !isRegistration))
                    if ((isRegistration && ownerOnThisNode) || (!isRegistration && !m_receiverIsLightNode))
                    {
                        const auto conPtr = Connections::Instance().GetConnection(state.GetSenderId(), std::nothrow);
                        auto counter = conPtr != nullptr ? conPtr->Counter() : -1;
                        bool receiverHasRegState = std::any_of(std::begin(m_syncState->connections), std::end(m_syncState->connections), [counter, &state](const auto& c)
                        {
                            return c.context == state.GetSenderId().m_contextId &&
                                    c.connectionId == state.GetSenderId().m_id &&
                                    c.counter == counter &&
                                    std::any_of(std::begin(c.registrations), std::end(c.registrations), [&state](const auto& r)
                                    {
                                        return r.typeId == state.GetTypeId() && r.handlerId == state.GetHandlerId() && r.registrationTime >= state.GetRegistrationTime().GetRawValue();
                                    });
                        });

                        if (!receiverHasRegState)
                        {
                            if (!m_distribution.GetCommunication().Send(m_nodeId, m_nodeType, ToPtr(state), state.Size(), RegistrationStateDataTypeId, true))
                            {
                                success=false;
                            }
                        }
                    }
                }

                if (success)
                {
                    subscription->SetLastRealState(state);
                }
            }
            return success;
        }

        void SetTimer(const std::function<void()>& completionHandler)
        {
            if (HasBeenCancelled())
            {
                return;
            }

            m_timer.expires_after(std::chrono::milliseconds(10));
            m_timer.async_wait(m_strand.wrap([self = this->shared_from_this(), completionHandler](const boost::system::error_code&)
            {
                if (self->HasBeenCancelled())
                {
                    return;
                }
                completionHandler();
            }));
        }

        static inline Safir::Utilities::Internal::SharedConstCharArray ToPtr(const DistributionData& d)
        {
            Safir::Utilities::Internal::SharedConstCharArray p(d.GetReference(), [](const char* ptr){DistributionData::DropReference(ptr);});
            return p;
        }

        //Dummy consumer
        void OnDoDispatch() override {}
        void OnRegistered(const Safir::Dob::Typesystem::TypeId, const Safir::Dob::Typesystem::HandlerId&) override {}
        void OnUnregistered(const Safir::Dob::Typesystem::TypeId, const Safir::Dob::Typesystem::HandlerId&) override {}
        void OnNewEntity(const Safir::Dob::EntityProxy) override {}
        void OnUpdatedEntity(const Safir::Dob::EntityProxy) override {}
        void OnDeletedEntity(const Safir::Dob::EntityProxy, const bool) override {}
    };
}
}
}
