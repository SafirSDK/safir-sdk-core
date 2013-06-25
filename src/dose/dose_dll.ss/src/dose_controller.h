/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / stjoot
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

#ifndef _dose_controller_h
#define _dose_controller_h

#include <Safir/Dob/Internal/Interface.h>
#include "dose_dispatcher.h"
#include "dose_dispatch_thread.h"
#include "dose_consumer_references.h"

// Sdk
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/Subscription.h>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Internal/SubscriptionOptions.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/ConnectionQueueId.h>
#include <Safir/Dob/CallbackId.h>

#include <map>
#include "Postponer.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class Controller
    {
        // Contruction/destruction
    public:
        Controller();

        void SetInstanceId(long id); //set id of this instance of Controller object

        //--------------------------------------------
        // Startup and Initialization methods
        //--------------------------------------------
        bool IsConnected();

        void Connect(const char* connectionNameCommonPart,
                     const char* connectionNameInstancePart,
                     const ContextId contextId,
                     long lang,
                     const ConsumerId & connectionOwner,
                     const ConsumerId & dispatcher,
                     OnDispatchCb* onDispatchCb, //callback instead of event
                     OnStopOrderCb* onStopOrderCb,
                     OnNewEntityCb* onNewEntityCb,
                     OnUpdatedEntityCb* onUpdatedEntityCb,
                     OnDeletedEntityCb* onDeletedEntityCb,
                     OnCreateRequestCb* onCreateRequestCb,
                     OnUpdateRequestCb* onUpdateRequestCb,
                     OnDeleteRequestCb* onDeleteRequestCb,
                     OnServiceRequestCb* onServiceRequestCb,
                     OnResponseCb* onResponseCb,
                     OnMessageCb* onMessageCb,
                     OnRegisteredCb* onRegisteredCb,
                     OnUnregisteredCb* onUnregisteredCb,
                     OnRevokedRegistrationCb* onRevokedRegistrationCb,
                     OnCompletedRegistrationCb* onCompletedRegistrationCb,
                     OnInjectedNewEntityCb* onInjectedNewEntityCb,
                     OnInjectedUpdatedEntityCb* onInjectedUpdatedEntityCb,
                     OnInjectedDeletedEntityCb* onInjectedDeletedEntityCb,
                     OnInitialInjectionsDoneCb* onInitialInjectionsDoneCb,
                     OnNotRequestOverflowCb* onNotRequestOverflowCb,
                     OnNotMessageOverflowCb* onNotMessageOverflowCb,
                     OnDropReferenceCb* onDropReferenceCb);

        void ConnectSecondary(long lang,
                              OnNewEntityCb* onNewEntityCb,
                              OnUpdatedEntityCb* onUpdatedEntityCb,
                              OnDeletedEntityCb* onDeletedEntityCb,
                              OnCreateRequestCb* onCreateRequestCb,
                              OnUpdateRequestCb* onUpdateRequestCb,
                              OnDeleteRequestCb* onDeleteRequestCb,
                              OnServiceRequestCb* onServiceRequestCb,
                              OnResponseCb* onResponseCb,
                              OnMessageCb* onMessageCb,
                              OnRegisteredCb* onRegisteredCb,
                              OnUnregisteredCb* onUnregisteredCb,
                              OnRevokedRegistrationCb* onRevokedRegistrationCb,
                              OnCompletedRegistrationCb* onCompletedRegistrationCb,
                              OnInjectedNewEntityCb* onInjectedNewEntityCb,
                              OnInjectedUpdatedEntityCb* onInjectedUpdatedEntityCb,
                              OnInjectedDeletedEntityCb* onInjectedDeletedEntityCb,
                              OnInitialInjectionsDoneCb* onInitialInjectionsDoneCb,
                              OnNotRequestOverflowCb* onNotRequestOverflowCb,
                              OnNotMessageOverflowCb* onNotMessageOverflowCb,
                              OnDropReferenceCb* onDropReferenceCb);

        void Disconnect();


        //---------------------------
        // Connection name
        //---------------------------

        // Returns composed connection name
        const char * const GetConnectionName() const;

        const char * const GetConnectionNameCommonPart() const;
        const char * const GetConnectionNameInstancePart() const;

        bool NameIsEqual (const std::string & connectionNameCommonPart,
                          const std::string & connectionNameInstancePart) const;

        void RegisterServiceHandler(const Dob::Typesystem::TypeId       typeId,
                                    const Dob::Typesystem::HandlerId&   handlerId,
                                    const bool                          overrideRegistration,
                                    const ConsumerId&                   consumer);

        void RegisterEntityHandler(const Dob::Typesystem::TypeId            typeId,
                                   const Dob::Typesystem::HandlerId&        handlerId,
                                   const Dob::InstanceIdPolicy::Enumeration instanceIdPolicy,
                                   const bool                               overrideRegistration,
                                   const bool                               isInjectionHandler,
                                   const ConsumerId&                        consumer);

        void UnregisterHandler(const Dob::Typesystem::TypeId typeId,
                               const Dob::Typesystem::HandlerId& handlerId);

        //----------------------------
        // Subscription methods
        //----------------------------
        void SubscribeMessage(const Dob::Typesystem::TypeId typeId,
                              const Dob::Typesystem::ChannelId& channelId,
                              const bool includeSubclasses,
                              const ConsumerId& consumer);

        void UnsubscribeMessage(const Dob::Typesystem::TypeId typeId,
                                const Dob::Typesystem::ChannelId & channelId,
                                const bool includeSubclasses,
                                const ConsumerId & consumer);

        void SubscribeEntity(const Typesystem::EntityId& entityId,
                             const bool allInstances,
                             const bool includeSubclasses,
                             const bool restartSubscription,
                             const SubscriptionOptionsPtr& subscriptionOptions,
                             const ConsumerId& consumer);

        void UnsubscribeEntity(const Typesystem::EntityId& entityId,
                               const bool allInstances,
                               const bool includeSubclasses,
                               const ConsumerId& consumer);

        void SubscribeRegistration(const Safir::Dob::Typesystem::TypeId typeId,
                                   const Dob::Typesystem::HandlerId& handlerId,
                                   const bool includeSubclasses,
                                   const bool restartSubscription,
                                   const SubscriptionOptionsPtr& subscriptionOptions,
                                   const ConsumerId& consumer);


        void UnsubscribeRegistration(const Safir::Dob::Typesystem::TypeId typeId,
                                     const Dob::Typesystem::HandlerId& handlerId,
                                     const bool includeSubclasses,
                                     const ConsumerId& consumer);

        //------------------------------
        // Dispatch method
        //------------------------------
        void Dispatch();

        void ExitDispatch();

        Safir::Dob::CallbackId::Enumeration CurrentCallback() const;

        void Postpone(const bool redispatchCurrent);
        void ResumePostponed();

        void IncompleteInjectionState();

        //------------------------------
        // Request methods
        //------------------------------
        void ServiceRequest(const char * const blob,
                            const Typesystem::HandlerId& handlerId,
                            const ConsumerId& consumer,
                            RequestId& requestId);

        void CreateRequest(const char * const blob,
                           const bool hasInstanceId,
                           const Typesystem::InstanceId instanceId, //not const-ref since we may modify it internally
                           const Typesystem::HandlerId& handlerId,
                           const ConsumerId& consumer,
                           RequestId& requestId);

        void UpdateRequest(const char * const blob,
                           const Typesystem::InstanceId& instanceId,
                           const ConsumerId& consumer,
                           RequestId & requestId);

        void DeleteRequest(const Typesystem::EntityId& entityId,
                           const ConsumerId& consumer,
                           RequestId & requestId);

        //--------------------------------
        // Message methods
        //--------------------------------
        void SendMessage(const char * const blob,
                         const Typesystem::ChannelId & channel,
                         const ConsumerId & consumer);

        /** Attach the response with the given responseId to the given consumers requestIn queue.*/
        void SendResponse(const char * const blob,
                          const ConsumerId & consumer,
                          const ResponseId responseId);

        //------------------------------
        // Entity methods
        //------------------------------
        void SetEntity(const char* const                blob,
                       const Typesystem::InstanceId&    instanceId,
                       const Typesystem::HandlerId&     handlerId,
                       const bool                       considerChangeFlags,
                       const bool                       initialInjection);

        void DeleteEntity(const Dob::Typesystem::EntityId& entityId,
                          const bool                       allInstances,
                          const Typesystem::HandlerId&     handlerId);

        void InjectEntity(const char* const                blob,
                          const Typesystem::InstanceId&    instanceId,
                          const Typesystem::HandlerId&     handlerId,
                          const Dob::Typesystem::Int64     timestamp);

        void InjectDeletedEntity(const Dob::Typesystem::EntityId& entityId,
                                 const Typesystem::HandlerId&     handlerId,
                                 const Dob::Typesystem::Int64     timestamp);

        void ReadEntity(const Typesystem::EntityId& entityId,
                        const char*& currentBlob,
                        const char*& currentState);

        bool IsCreated(const Dob::Typesystem::EntityId& entityId);

        InstanceIdPolicy::Enumeration GetInstanceIdPolicy(const Typesystem::TypeId typeId,
                                                          const Typesystem::HandlerId& handlerId) const;

        Typesystem::Int32 GetQueueCapacity(const ConnectionQueueId::Enumeration queue);

        Typesystem::Int32 GetQueueSize(const ConnectionQueueId::Enumeration queue);

        Typesystem::Int32 EntityIteratorCreate(const Typesystem::TypeId typeId,
                                               const bool includeSubclasses,
                                               bool& end);

        void EntityIteratorDestroy(const Typesystem::Int32 iteratorId);

        Typesystem::Int32 EntityIteratorCopy(const Safir::Dob::Typesystem::Int32 iteratorId);

        void EntityIteratorIncrement(const Safir::Dob::Typesystem::Int32 iteratorId,
                                     bool& end);

        void EntityIteratorDereference(const Safir::Dob::Typesystem::Int32 iteratorId,
                                       const char *& entityBlob,
                                       const char *& entityState);

        bool EntityIteratorEqual(const Safir::Dob::Typesystem::Int32 first,
                                 const Safir::Dob::Typesystem::Int32 second);


        ContextId GetContext() const;

        //-------------------------------
        // Debug
        //-------------------------------
        void SimulateOverflows(const bool inQueues, const bool outQueues);


    private:
        void SendRequest(const DistributionData& request,
                         const ConsumerId& consumer);

        bool m_isConnected;

        //Pointer to our connection instance in shared memory
        ConnectionPtr m_connection;

        // The big dispatcher
        Dispatcher  m_dispatcher;

        // True if some queue is full
        bool m_requestQueueInOverflowState;
        bool m_messageQueueInOverflowState;

        //--------------------------------
        // Compose decorated name from name parts
        //--------------------------------
        static const std::string ComposeName(const ContextId    contextId,
                                             const std::string& commonPart,
                                             const std::string& instancePart);

        //---------------------------------
        // Dispatch requests
        //---------------------------------
        void DispatchRequest(const ConsumerId & consumer, const DistributionData & request, bool & exitDispatch, bool & dontRemove);
        void DispatchRequestInQueue(const ConsumerId& consumer, RequestInQueue & queue);
        void DispatchRequestInQueues();

        //---------------------------------
        // Dispatch responses
        //---------------------------------
        void DispatchResponse(const DistributionData & response,
                              const DistributionData & request,
                              bool & exitDispatch);
        void DispatchResponseInQueue();

        //---------------------------------
        // Dispatch messages
        //---------------------------------
        void DispatchMessage(const ConsumerId & consumer, const DistributionData & msg, bool & exitDispatch, bool & dontRemove);
        void DispatchMessageInQueue(const ConsumerId& consumer, MessageQueue & queue);
        void DispatchMessageInQueues();

        //---------------------------------
        // Dispatch subscriptions
        //---------------------------------
        bool ProcessRegistrationSubscription(const SubscriptionPtr& subscription, bool& exitDispatch);
        bool ProcessEntitySubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove);
        void DispatchRegistrationSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove);
        void DispatchEntitySubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove);
        void DispatchSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove);
        void DispatchSubscriptions();

        //---------------------------------
        // Dispatch entity injection subscriptions
        //---------------------------------
        bool ProcessInjectionSubscription(const SubscriptionPtr& subscription, bool& exitDispatch, bool& dontRemove);
        void InitialInjectionHandled(const Dob::Typesystem::HandlerId&     handlerId,
                                     const ConsumerId&                     consumer,
                                     const Typesystem::TypeId              typeId,
                                     const Dob::Typesystem::InstanceId&    instanceId);
        void DispatchEmptyInitialInjections();
        void CheckAppInjectionAction(const bool postpone,
                                     const bool incompleteInjectionState,
                                     const bool setInjectedEntity,
                                     const bool deleteInjectedEntity) const;

        enum InjectionDispatchAction
        {
            NoAction,
            NewCallback,
            UpdateCallback,
            DeleteCallback
        };

        typedef std::pair<InjectionDispatchAction, DistributionData> InjectionData;

        InjectionData CreateInjectionData(const SubscriptionPtr& subscription);
        bool DispatchInjection(const InjectionData& injection, const SubscriptionPtr& subscription, bool& confirmInjection);

        //---------------------------------
        // Revoked registrations
        //---------------------------------
        void HandleRevokedRegistrations();

        //---------------------------------
        // Pending registrations
        //---------------------------------
        void HandlePendingRegistrations();

        //----------------------------------
        // Identifiers
        //----------------------------------
        //Id of this instance of Controller
        long m_ctrlId;

        int m_contextId;

        std::string m_connectionName;

        // Original name parts
        std::string m_connectionNameCommonPart;
        std::string m_connectionNameInstancePart;

        typedef boost::shared_ptr<DispatchThread> DispatchThreadPtr;
        typedef std::map<DispatchThreadPtr, int> DispatchThreadPtrStopCountMap;

        DispatchThreadPtr  m_dispatchThread;

        bool m_exitDispatch;

        Postponer m_postponedTypes;
        bool m_postponeCurrent;
        bool m_postpone;

        bool m_incompleteInjection;

        // AWI:! Denna var förut ett entiyId. Om inte Distribution data behövs för blob diff jämförelse kan det bytas tillbaka till
        // entityId.
        DistributionData m_dispatchedInjection;

        DistributionData m_originalInjectionState;

        bool m_setInjectedEntity;
        bool m_deleteInjectedEntity;

        class RequestIds
        {
        public:
            RequestIds():m_nextRequestId(0) {}
            InternalRequestId GetNextRequestId() {return m_nextRequestId++;}
        private:
            InternalRequestId m_nextRequestId;
        };

        RequestIds m_requestIds;

        typedef unordered_map<Typesystem::Int32, Internal::EntityTypes::EntityIterator> EntityIteratorTable;

        EntityIteratorTable m_entityIterators;

        // Holds reference counters for garbage collected languages.
        ConsumerReferences m_consumerReferences;

        /*             // For test purposes */
        /*             void DumpDirtySub(std::vector<DirtySub>& ds); */

    };

    typedef boost::shared_ptr<Controller> ControllerPtr;
    typedef boost::shared_ptr<const Controller> ControllerConstPtr;
}
}
}

#endif //_dose_controller_h
