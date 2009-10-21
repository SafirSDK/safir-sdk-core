/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#ifndef __DOSE_CONNECTION_H__
#define __DOSE_CONNECTION_H__

#include <Safir/Dob/Internal/ConnectionId.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/PendingRegistration.h>
#include <Safir/Dob/Internal/RequestInQueue.h>
#include <Safir/Dob/Internal/RequestOutQueue.h>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/MessageQueue.h>
#include <Safir/Dob/Internal/ConsumerQueueContainer.h>
#include <Safir/Dob/Internal/ConsumerId.h>
#include <Safir/Dob/Internal/SubscriptionQueue.h>
#include <Safir/Dob/Internal/State.h>
#include <Safir/Dob/Internal/ShmWrappers.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    typedef boost::interprocess::offset_ptr<AtomicUint32> SignalPtr;
    typedef std::pair<Dob::Typesystem::TypeId, Dob::Typesystem::HandlerId> TypeHandlerPair;

    class DOSE_INTERNAL_API Connection:
        public SharedMemoryObject
    {
    public:
        Connection(const std::string & name,
                   const Typesystem::Int32 counter,
                   const NodeNumber node,
                   const int context,
                   const int pid);

        ~Connection();

        static Identifier CalculateIdentifier(const std::string & name);

        const char * NameWithoutCounter() const {return m_nameWithoutCounter.c_str();}
        const char * NameWithCounter() const {return m_nameWithCounter.c_str();}
        Typesystem::Int32 Counter() const {return m_counter;}

        const ConnectionId & Id() const {return m_id;}
        const int Pid() const {return m_pid;}

        bool IsLocal() const;

        bool operator<(const Connection & other) const {return Id() < other.Id();}

        //---------- Request queues ----------
        RequestOutQueue& GetRequestOutQueue() {return m_requestOutQueue;}

        // Add a request in-queue (if it doesn't already exist it will be created) for the given consumer. A smart pointer to the new
        // (or the existing) queue is returned.
        RequestInQueuePtr AddRequestInQueue(const ConsumerId& consumer);

        // Loop over all request in-queues
        void ForEachRequestInQueue(const RequestInQueueContainer::QueueFunc& queueFunc) const;
        void ForSpecificRequestInQueue(const ConsumerId& consumer, const RequestInQueueContainer::QueueFunc& queueFunc) const;

        //---------- Message queues ----------
        MessageQueue& GetMessageOutQueue() {return m_messageOutQueue;}

        // Add a message in-queue (if it doesn't already exist it will be created) for the given consumer. A smart pointer to the new
        // (or the existing) queue is returned.
        MessageQueuePtr AddMessageInQueue (const ConsumerId& consumer);

        // Loop over all message in-queues
        void ForEachMessageInQueue(const MessageQueueContainer::QueueFunc& queueFunc) const;

        //---------- Dirty subscriptions ----------
        SubscriptionQueue& GetDirtySubscriptionQueue() {return m_dirtySubscriptions;}

        //---------- Subscribed types ----------
        // Note that a subscribed typeId will never be removed. This is due to the fact that
        // it is a bit complicated to implement the removal, and we make the assumption that
        // the set of subscribed typ id for an application is fairly static.
        void AddSubscription(const Typesystem::TypeId   typeId,
                             SubscriptionType           subscriptionType);

        typedef Containers<Typesystem::TypeId>::set TypesSet;
        TypesSet GetSubscriptions(SubscriptionType subscriptionType) const;

        //---------- Registrations ----------
        void AddRegistration(const Typesystem::TypeId              typeId,
                             const Dob::Typesystem::HandlerId&     handlerId,
                             const ConsumerId&                     consumer);
        void RemoveRegistration(const Typesystem::TypeId              typeId,
                                const Dob::Typesystem::HandlerId&     handlerId);

        //---------- Injection handler registration  ----------
        // The Add- and Remove methods are needed to imlement the OnInitialInjectionDone-functionality
        void AddEmptyInitialInjection(const Typesystem::TypeId              typeId,
                                      const Dob::Typesystem::HandlerId&     handlerId);

        // Returns a vector with all empty (has no instances) type/handler pairs. The underlying structure is cleared.
        std::vector<TypeHandlerPair> GetAndClearEmptyInitialInjections();

        void AddInitialInjectionInstance(const Typesystem::TypeId              typeId,
                                         const Dob::Typesystem::HandlerId&     handlerId,
                                         const Dob::Typesystem::InstanceId&    instanceId);
        // Returns true if the instance has been deleted and there are no more instances for the given type.
        bool RemoveInitialInjectionInstance(const Typesystem::TypeId              typeId,
                                            const Dob::Typesystem::HandlerId&     handlerId,
                                            const Dob::Typesystem::InstanceId&    instanceId,
                                            const bool                            allInstances);

        // Injection handler consumers
        void AddInjectionHandler(const Typesystem::TypeId              typeId,
                                 const Dob::Typesystem::HandlerId&     handlerId,
                                 const ConsumerId&                     consumer);
        void RemoveInjectionHandler(const Typesystem::TypeId              typeId,
                                    const Dob::Typesystem::HandlerId&     handlerId);
        const ConsumerId GetInjectionHandlerConsumer(const Typesystem::TypeId              typeId,
                                                     const Dob::Typesystem::HandlerId&     handlerId) const;
        // Get a copy of current injection handlers.
        const RegistrationVector GetInjectionHandlers() const;

        //---------- Revoked registrations ----------
        void AddRevokedRegistration(const Typesystem::TypeId              typeId,
                                    const Dob::Typesystem::HandlerId&     handlerId,
                                    const ConsumerId&                     consumer);
        void RemoveRevokedRegistration(const Typesystem::TypeId              typeId,
                                       const Dob::Typesystem::HandlerId&     handlerId);
        // Get a copy of current revoked registrations.
        const RegistrationVector GetRevokedRegistrations() const;
        // Get a copy of revoked registrations. The underlaying structure is cleared.
        const RegistrationVector GetAndClearRevokedRegistrations();

         /**
         * @name Pending Registrations
         */
        /** @{ */

        //get a snapshot of the pending registrations for the connection
        const PendingRegistrationVector GetPendingRegistrations();
        //used by dose_dll to add new pending registrations to the connection.
        void AddPendingRegistration(const PendingRegistration & reg);

        //Used by dose_main to get new pending regs to process.
        bool GetNextNewPendingRegistration(const long requestId, PendingRegistration & reg);

        //Used by dose_main. The vector contains copies of the PRs. The PRs are removed from the connection.
        const PendingRegistrationVector GetRemovedPendingRegistrations();

        void SetPendingRegistrationAccepted(const long requestId);

        bool IsPendingAccepted(const Dob::Typesystem::TypeId typeId,
                               const Dob::Typesystem::HandlerId & handlerId) const;


        void RetryAcceptedPendingOwnership(const long requestId);
        void RemoveAcceptedPendingOwnership(const long requestId);
        bool RemovePendingRegistrations(const Dob::Typesystem::TypeId typeId,
                                        const Dob::Typesystem::HandlerId & handlerId);
        /** @} */

        void SetOutSignal(const SignalPtr & signal) {m_outSignal = signal;}

        //Signal the event that the application has done something.
        void SignalOut() const;

        //Signal in-event
        void SignalIn() const;

        void SendStopOrder() {m_stopOrderPending = 1; SignalIn();}
        bool StopOrderPending() const {return m_stopOrderPending != 0;}
        void SetStopOrderHandled() {m_stopOrderPending = 0;}

        void Died() {m_died = 1; SignalOut();}

        bool IsDead() const {return m_died != 0;}

        // Flag valid only for non-local connections.
        // Indicates that the remote node, from where the connection originates, is down.
        void SetNodeDown() {m_nodeDown = 1;}
        bool NodeIsDown() const {return m_nodeDown != 0;}

    private:

        const ShmString m_nameWithoutCounter;
        ShmString m_nameWithCounter;
        const Typesystem::Int32 m_counter;
        ConnectionId m_id;
        const int m_pid;

        //set to true if dose_main is to be told of queue changes.
        //Also the dose_main event must be set for it to wake up.
        //Don't ever set this directly, use the SignalDoseMain function in Controller.
        SignalPtr m_outSignal;

        typedef std::pair<Typesystem::TypeId, ShmHandlerId> TypeHandlerKey;

        typedef PairContainers<TypeHandlerKey, ConsumerId>::map RegistrationsMap;

        RegistrationsMap m_injectionHandlers;

        RegistrationsMap m_registrations;
        RegistrationsMap m_revokedRegistrations;

        typedef Containers<PendingRegistration>::vector PendingOwnerships;
        PendingOwnerships m_pendingOwnerships;

        typedef Containers<TypesSet>::vector        TypesSetVector;

        TypesSetVector    m_subscribedTypes;

        typedef Containers<Typesystem::InstanceId::UnderlyingType>::set     InitialInjectionValue;
        typedef PairContainers<TypeHandlerKey, InitialInjectionValue>::map   InitialInjectionInstances;
        InitialInjectionInstances m_initialInjectionInstances;

        SubscriptionQueue m_dirtySubscriptions;

        // Message queues
        // Several in queues, one for each consumer.
        MessageQueueContainer::shared_ptr m_messageInQueues;
        // One common out queue for all consumers.
        MessageQueue    m_messageOutQueue;

        // Request queues
        // Several in queues, one for each consumer.
        RequestInQueueContainer::shared_ptr m_requestInQueues;
        // One common out queue for all consumers.
        RequestOutQueue m_requestOutQueue; //requestOut - responseIn

        AtomicUint32 m_stopOrderPending;
        AtomicUint32 m_died;

        AtomicUint32 m_nodeDown;

        const bool m_isLocal;

        //Locking Policy:
        //This class uses a non-recursive lock, since there should be no
        //recursive locking: This class has no callbacks, and internal calling should
        //not need to do recursive locking.
        //Any attempts to take the lock recursively are to be regarded as
        //programming errors.
        mutable boost::interprocess::interprocess_mutex m_lock;

        void Unsubscribe(const Typesystem::TypeId typeId);

        void Unregister(const std::pair<TypeHandlerKey, ConsumerId>& reg);

        const RegistrationVector GetRegistrations(const RegistrationsMap& registrations) const;
    };
}
}
}
#endif
