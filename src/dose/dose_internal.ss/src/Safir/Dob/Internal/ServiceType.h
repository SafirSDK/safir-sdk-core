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

#ifndef _dose_internal_service_type_h
#define _dose_internal_service_type_h

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/HandlerRegistrations.h>
#include <Safir/Dob/Internal/SubscriptionOptions.h>
#include <Safir/Dob/Internal/SubscriptionId.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API ServiceType:
        public SharedMemoryObject
    {
    public:

        explicit ServiceType(const Typesystem::TypeId typeId);

        Dob::Typesystem::TypeId GetTypeId() const {return m_typeId;}

        /**
         * @name Handler registrations
         */
        /** @{ */

        bool Register(const ConnectionPtr&                  connection,
                      const Dob::Typesystem::HandlerId&     handlerId,
                      const RegisterTime                    regTime,
                      const bool                            overrideRegistration,
                      const ConsumerId&                     consumer);

        void Unregister(const ConnectionPtr&                connection,
                        const Dob::Typesystem::HandlerId&   handlerId);

        void UnregisterAll(const ConnectionPtr& connection);

        /** New registration state from external node */
        void RemoteSetRegistrationState(const ConnectionPtr& connection,
                                        const DistributionData& registrationState);

        /** Unregistration (an end state) from external node */
        void RemoteSetUnregistrationState(const DistributionData& registrationState);


        bool IsRegistered(const Dob::Typesystem::HandlerId& handlerId) const;

        const ConnectionConsumerPair GetRegisterer(const Dob::Typesystem::HandlerId& handlerId) const;
        /** @} */

        /**
         * @name Handler registration subscriptions
         */
        /** @{ */
        void SubscribeRegistration(const SubscriptionId&                subscriptionId,
                                   const Dob::Typesystem::HandlerId&    handlerId,
                                   const bool                           restartSubscription,
                                   const SubscriptionOptionsPtr&        subscriptionOptions);

        void UnsubscribeRegistration(const SubscriptionId&              subscriptionId,
                                     const Dob::Typesystem::HandlerId&  handlerId);

        void UnsubscribeRegistrationAll(const ConnectionPtr& connection);

        // Returns true if the given connection/consumer has any service handler registration
        // subscription for this type.
        bool HasRegistrationSubscription(const ConnectionPtr&    connection,
                                         const ConsumerId&       consumer) const;

        /** @} */


    private:
        Typesystem::TypeId m_typeId;

        HandlerRegistrations m_handlerRegistrations;
    };
}
}
}
#endif

