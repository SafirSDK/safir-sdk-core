/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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

#ifndef _dose_internal_service_types_h
#define _dose_internal_service_types_h

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Internal/ServiceType.h>
#include <Safir/Dob/Internal/ConsumerId.h>
#include <Safir/Dob/Internal/SubscriptionId.h>
#include <Safir/Dob/Internal/LamportClocks.h>
#include <Safir/Dob/Internal/PendingRegistration.h>
#include <Safir/Dob/Internal/SubscriptionOptions.h>
#include <Safir/Dob/Internal/Atomic.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API ServiceTypes:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    private:
        //This is to make sure that only Instance can call the constructor even though the constructor
        //itself has to be public (limitation of boost::interprocess)
        struct private_constructor_t {};
    public:
        static void Initialize(const bool iAmDoseMain = false);
        static bool IsInitialized();

        static ServiceTypes& Instance();


        /**
         * @name Service handler registrations
         */
        /** @{ */

        bool Register(const ConnectionPtr&                  connection,
                      const Dob::Typesystem::TypeId         typeId,
                      const Dob::Typesystem::HandlerId&     handlerId,
                      const bool                            overrideRegistration,
                      const ConsumerId&                     consumer);

        void Unregister(const ConnectionPtr&                connection,
                        const Dob::Typesystem::TypeId       typeId,
                        const Dob::Typesystem::HandlerId&   handlerId);

        // Unregisters all existing registrations for the given connection and type.
        void UnregisterAll(const ConnectionPtr&           connection,
                           const Dob::Typesystem::TypeId  typeId,
                           const bool                     explicitUnregister);

        /** New registration state from external node */
        void RemoteSetRegistrationState(const ConnectionPtr& connection,
                                        const DistributionData& registrationState);

        bool IsRegistered(const Dob::Typesystem::TypeId     typeId,
                          const Dob::Typesystem::HandlerId& handlerId,
                          const ContextId                   contextId) const;

        /**
         * Find the connection/consumer that has registered the given type/handler
         * If no-one owns it this method returns (NULL,(NULL,0))
         */
        const ConnectionConsumerPair GetRegisterer(const Dob::Typesystem::TypeId     typeId,
                                                   const Dob::Typesystem::HandlerId& handlerId,
                                                   const ContextId                   contextId) const;

        /** @} */

        /**
         * @name Handler registration subscriptions
         */
        /** @{ */

        void SubscribeRegistration(const SubscriptionId&                subscriptionId,
                                   const Dob::Typesystem::TypeId        typeId,
                                   const Dob::Typesystem::HandlerId&    handlerId,
                                   const bool                           includeSubclasses,
                                   const bool                           restartSubscription,
                                   const SubscriptionOptionsPtr&        subscriptionOptions);

        void UnsubscribeRegistration(const SubscriptionId&              subscriptionId,
                                     const Dob::Typesystem::TypeId      typeId,
                                     const Dob::Typesystem::HandlerId&  handlerId,
                                     const bool                         includeSubclasses);

        // Removes all existing subscriptions (all consumers) for the given connection and type.
        void UnsubscribeRegistrationAll(const ConnectionPtr&           connection,
                                        const Dob::Typesystem::TypeId  typeId);

        // Returns true if the given connection/consumer has any registration subscription for the given type.
        bool HasRegistrationSubscription(const ConnectionPtr&           connection,
                                         const ConsumerId&              consumer,
                                         const Dob::Typesystem::TypeId  typeId);

        /** @} */

        //also removes them from the PendingOwnerships shared vector.
        void RegisterAcceptedPendingRegistrations(const ConnectionPtr & connection,
                                                  PendingRegistrationVector & prv,
                                                  bool & needKick);

        // This method tries to acquire and release the container lock for the handler registration container
        // in all contexts.
        bool CanAcquireContainerWriterLock(const Typesystem::TypeId             typeId,
                                           const ContextId                      contextId,
                                           const boost::posix_time::seconds&    lockTimeout);

        //Debug and statistics
        void DumpRegistrationSubscriptions() const;
        void DumpRegisteredHandlers() const;

        //The constructor and destructor have to be public for the boost::interprocess internals to be able to call
        //them, but we can make the constructor "fake-private" by making it require a private type as argument.
        explicit ServiceTypes(private_constructor_t);

    private:

        ServiceType& GetType(const Typesystem::TypeId typeId);

        const ServiceType& GetType(const Typesystem::TypeId typeId) const
        {return const_cast<ServiceTypes&>(*this).GetType(typeId);} //just call the nonconst version

        typedef PairContainers<Typesystem::TypeId, ServiceTypePtr>::map ServiceTypeTable;
        ServiceTypeTable m_serviceTypes;

        bool m_iAmDoseMain;
        static ServiceTypes* m_instance;
        static AtomicUint32  m_isInitialized;

        LamportClock m_registrationClock;
    };
}
}
}
#endif

