/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Anders Wid�n / stawi
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
#ifndef _dose_internal_handler_registrations_h
#define _dose_internal_handler_registrations_h

#include <Safir/Dob/InstanceIdPolicy.h>
#include <Safir/Dob/Internal/ConsumerId.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/ShmWrappers.h>
#include <Safir/Dob/Internal/ConnectionConsumerPair.h>
#include <Safir/Dob/Internal/StateContainer.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Internal/SubscriptionOptions.h>


namespace Safir
{
namespace Dob
{
namespace Internal
{

    class DOSE_INTERNAL_API HandlerRegistrations:
        public SharedMemoryObject
    {
    public:

        explicit HandlerRegistrations(const Typesystem::TypeId typeId);

        HandlerRegistrations(const Typesystem::TypeId typeId,
                             const StateContainerPtr& entityContainerPtr);

        /**
         * @name Handler registrations
         */
        /** @{ */

        // Returns true if a registration has taken place.
        bool Register(const ConnectionPtr&                  connection,
                      const Dob::Typesystem::HandlerId&     handlerId,
                      const InstanceIdPolicy::Enumeration   instanceIdPolicy,
                      const bool                            isInjectionHandler,
                      const RegisterTime                    regTime,
                      const bool                            overrideRegistration,
                      const ConsumerId&                     consumer);

        void Unregister(const ConnectionPtr&                connection,
                        const Dob::Typesystem::HandlerId&   handlerId,
                        const RegisterTime                  regTime);

        void UnregisterAll(const ConnectionPtr&     connection);

        /** New registration state from external node */
        void RemoteSetRegistrationState(const ConnectionPtr& connection,
                                        const DistributionData& registrationState);

        /** Unregistration (an end state) from external node */
        void RemoteSetUnregistrationState(const DistributionData& registrationState);

        bool IsRegistered(const Dob::Typesystem::HandlerId& handlerId) const;

        /**
         * Find the connection/consumer that has registered the given handler
         * If the handler is not registered this method returns (NULL,(NULL,0))
         */
        const ConnectionConsumerPair GetRegisterer(const Dob::Typesystem::HandlerId& handlerId) const;

        /**
         * Find the connection/consumer and registration time for the given handler.
         * Returns true if the handlerId is registered.
         */
        bool GetRegisterer(const Dob::Typesystem::HandlerId& handlerId,
                           ConnectionConsumerPair&           registerer,
                           RegisterTime&                     registrationTime) const;

        /**
         * Find the connection/consumer and instance id policy for the given handler.
         * Returns true if the handlerId is registered.
         */
        bool GetRegisterer(const Dob::Typesystem::HandlerId& handlerId,
                           ConnectionConsumerPair&           registerer,
                           InstanceIdPolicy::Enumeration&    instanceIdPolicy) const;

        // Returns a locked registration state (or NULL if not found).
        const LockedStateResult GetLockedRegistrationState(const Dob::Typesystem::HandlerId& handlerId,
                                                           const bool                        includeReleasedStates) const;


        /** @} */

        /**
         * @name Handler registration subscriptions
         */
        /** @{ */

        void Subscribe(const SubscriptionId&                subscriptionId,
                       const Dob::Typesystem::HandlerId&    handlerId,
                       bool                                 restartSubscription,
                       const SubscriptionOptionsPtr&        subscriptionOptions);

        void Unsubscribe(const SubscriptionId&              subscriptionId,
                         const Dob::Typesystem::HandlerId&  handlerId);

        void UnsubscribeAll(const ConnectionPtr& connection);

        bool HasSubscription(const ConnectionPtr&    connection,
                             const ConsumerId&       consumer) const;

        /** @} */

    private:

        Typesystem::TypeId m_typeId;

        SubscriptionType m_subscriptionType;

        void SetSubscriptionType();

        mutable StateContainer m_registrations;

        StateContainerPtr  m_entityContainerPtr;

        // This aggregation of input paramters exist because the current boost bind implementation
        // can handle a maximum of 9 parameters.
        struct RegisterInternalInput
        {
            ConnectionConsumerPair          registerer;
            Dob::Typesystem::HandlerId      handlerId;
            InstanceIdPolicy::Enumeration   instanceIdPolicy;
            bool                            isInjectionHandler;
            RegisterTime                    regTime;
            bool                            overrideRegistration;
        };

        void RegisterInternal(const UpgradeableStateResult& upgradeableStateResult,
                              const RegisterInternalInput&  inputPar,
                              bool&                         registrationDone);

        void UnregisterInternal(const ConnectionPtr&                connection,
                                const Dob::Typesystem::HandlerId&   handlerId,
                                const RegisterTime                  regTime,
                                const NodeNumber                    nodeNumber,
                                const UpgradeableStateResult&       upgradeableStateResult,
                                StatePtrHandling&                   statePtrHandling);

        void UnregisterAllInternal(const ConnectionPtr&             connection,
                                   const UpgradeableStateResult&    upgradeableStateResult,
                                   StatePtrHandling&                statePtrHandling,
                                   bool&                            exitDispatch);

        void IsRegisteredInternal(const UpgradeableStateResult& upgradeableStateResult,
                                  bool&                         isRegistered) const;


        bool IsRegisteredInternal(const StateSharedPtr& statePtr) const;

        void GetRegistererInternal(const UpgradeableStateResult&        upgradeableStateResult,
                                   ConnectionConsumerPair&              registerer,
                                   InstanceIdPolicy::Enumeration&       instanceIdPolicy,
                                   RegisterTime&                        registrationTime) const;

        void DeleteEntity(const UpgradeableStateResult&        upgradeableStateResult,
                          const ConnectionPtr&                 connection,
                          const Dob::Typesystem::HandlerId&    handlerId,
                          StatePtrHandling&                    statePtrHandling,
                          bool&                                exitDispatch);

        void RegisterInjectionHandler(const ConnectionPtr&                connection,
                                      const Dob::Typesystem::HandlerId&   handlerId,
                                      const ConsumerId&                   consumer);

        void UnregisterInjectionHandler(const ConnectionPtr&                connection,
                                        const Dob::Typesystem::HandlerId&   handlerId);

        friend void StatisticsCollector(HandlerRegistrations&, void*);
    };
}
}
}
#endif

