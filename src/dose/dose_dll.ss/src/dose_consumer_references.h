/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

#ifndef __DOSE_CONSUMER_REFERENCES_H__
#define __DOSE_CONSUMER_REFERENCES_H__

#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/ConsumerId.h>

#include <boost/function.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DispatchThread;
    typedef boost::shared_ptr<DispatchThread> DispatchThreadPtr;

    /**
     * Holds reference counters for consumers. This information is needed when
     * supporting garbage collected languages.
     */
    class ConsumerReferences
    {
    public:
        ConsumerReferences() {};
        ~ConsumerReferences() {};

        typedef boost::function<void(const ConsumerId& consumer,
                                     const long        refCounter)> DropReferencesFunc;

        /**
         * @name Dispatcher references
         */
        /** @{ */

        /**
         * Increment the counter for a dispatcher consumer.
         */
        void AddDispatcherReference(const ConsumerId& consumer);

        /**
         * Returns true if there is at least one dispatcher reference for the given consumer.
         */
        bool HasDispatcherReference(const ConsumerId& consumer) const;

        /** @} */

        /**
         * @name Stop handler references
         */
        /** @{ */

        /**
         * Increment the counter for a stop handler consumer.
         */
        void AddStopHandlerReference(const ConsumerId& consumer);

        /** @} */

        /**
         * @name Handler registration references
         */
        /** @{ */

        /**
         * Increment the handler registration counter for the given type, handler, and consumer. 
         */
        void AddHandlerRegistrationReference(const Dob::Typesystem::TypeId      typeId,
                                             const Dob::Typesystem::HandlerId&  handlerId,
                                             const ConsumerId&                  consumer);

        /**
         * To be called after a handler unregistration has been performed.
         */
        void DropAllHandlerRegistrationReferences(const Dob::Typesystem::TypeId       typeId,
                                                  const Dob::Typesystem::HandlerId&   handlerId,
                                                  const DropReferencesFunc&           dropReferencesFunc);

        /**
         * Returns true if there is at least one handler registration reference for
         * the given type, handler, and consumer.
         */
        bool HasHandlerRegistrationReference(const Dob::Typesystem::TypeId      typeId,
                                             const Dob::Typesystem::HandlerId&  handlerId,
                                             const ConsumerId&                  consumer) const;

        /**
         * Returns the current number of references for the given type id, handler id and consumer.
         */
        long GetHandlerRegistrationReferenceCounter(const Dob::Typesystem::TypeId      typeId,
                                                    const Dob::Typesystem::HandlerId&  handlerId,
                                                    const ConsumerId&                  consumer) const;

        /**
         * Drop the given number of refreneces
         */
        void DropHandlerRegistrationReferences(const Dob::Typesystem::TypeId       typeId,
                                               const Dob::Typesystem::HandlerId&   handlerId,
                                               const ConsumerId&                   consumer,
                                               const long                          nbrOfReferences,
                                               const DropReferencesFunc&           dropReferencesFunc);

        /** @} */

        /**
         * @name Message subscription references
         */
        /** @{ */

        /**
         * Increment the message subscription reference counter for the given consumer.
         */
        void AddMessageSubscriptionReference(const ConsumerId& consumer);

        /**
         * To be called after a message unsubscribe has been performed. If the given consumer
         * has no remaining active subscription the callback function will be called and
         * the consumer is then removed.
         */
        void DropMessageSubscriptionReferences(const ConnectionPtr&       connection,
                                               const ConsumerId&          consumer,
                                               const DropReferencesFunc&  dropReferencesFunc);

        /**
         * Returns true if there is at least one message subscription reference for the given consumer.
         */
        bool HasMessageSubscriptionReference(const ConsumerId& consumer) const;

        /** @} */

        /**
         * @name Entity subscription references
         */
        /** @{ */

        /**
         * Increment the entity subscription reference counter for the given consumer.
         */
        void AddEntitySubscriptionReference(const ConsumerId& consumer);

        /**
         * To be called after an entity unsubscribe has been performed. If the given consumer
         * has no remaining active subscription the callback function will be called and
         * the consumer is then removed.
         */
        void DropEntitySubscriptionReferences(const ConnectionPtr&       connection,
                                              const ConsumerId&          consumer,
                                              const DropReferencesFunc&  dropReferencesFunc);

        /**
         * Returns true if there is at least one entity subscription reference for the given consumer.
         */
        bool HasEntitySubscriptionReference(const ConsumerId& consumer) const;

        /** @} */

        /**
         * @name Entity registration subscription references
         */
        /** @{ */

        /**
         * Increment the entity registration subscription reference counter for the given consumer.
         */
        void AddEntityRegistrationSubscriptionReference(const ConsumerId& consumer);

        /**
         * To be called after an entity registration unsubscribe has been performed. If the given consumer
         * has no remaining active subscription the callback function will be called and
         * the consumer is then removed.
         */
        void DropEntityRegistrationSubscriptionReferences(const ConnectionPtr&       connection,
                                                          const ConsumerId&          consumer,
                                                          const DropReferencesFunc&  dropReferencesFunc);

        /**
         * Returns true if there is at least one entity registration subscription reference for the given consumer.
         */
        bool HasEntityRegistrationSubscriptionReference(const ConsumerId& consumer) const;

        /** @} */

        /**
         * @name Service registration subscription references
         */
        /** @{ */

        /**
         * Increment the service registration subscription reference counter for the given consumer.
         */
        void AddServiceRegistrationSubscriptionReference(const ConsumerId& consumer);

        /**
         * To be called after a service registration unsubscribe has been performed. If the given consumer
         * has no remaining active subscription the callback function will be called and
         * the consumer is then removed.
         */
        void DropServiceRegistrationSubscriptionReferences(const ConnectionPtr&       connection,
                                                           const ConsumerId&          consumer,
                                                           const DropReferencesFunc&  dropReferencesFunc);

        /**
         * Returns true if there is at least one service registration subscription reference for the given consumer.
         */
        bool HasServiceRegistrationSubscriptionReference(const ConsumerId& consumer) const;

        /** @} */

        /**
         * @name All references
         */
        /** @{ */
        
        /**
         * Method to be called when disconnect has been called. The callback function will
         * be called for every existing consumer and the consumers will then be removed. 
         */
        void DropAllReferences(const DropReferencesFunc& dropReferencesFunc);

        /** @} */

    private:

        typedef std::map<ConsumerId, long> CounterMap;

        CounterMap m_dispatcherCounters;
        CounterMap m_stopHandlerCounters;
        CounterMap m_messageSubscriptionCounters;
        CounterMap m_entitySubscriptionCounters;
        CounterMap m_entityRegistrationSubscriptionCounters;
        CounterMap m_serviceRegistrationSubscriptionCounters;

        typedef boost::tuple<Dob::Typesystem::TypeId, Dob::Typesystem::HandlerId, ConsumerId> RegistrationKey;
        typedef std::map<RegistrationKey, long> RegistrationCounterMap;

        RegistrationCounterMap m_registrationCounterMap;

        static void AddReference(const ConsumerId& consumer, CounterMap& counterMap);

        typedef boost::function<bool(const ConnectionPtr&           connection,
                                     const ConsumerId&              consumer,
                                     const Dob::Typesystem::TypeId  typeId)> HasSubscriptionFunc;

        static void DropSubscriptionReferences(const ConnectionPtr&           connection,
                                               const ConsumerId&              consumer,
                                               const DropReferencesFunc&      dropReferencesFunc,
                                               SubscriptionType               subscriptionType,
                                               CounterMap&                    counterMap,
                                               const HasSubscriptionFunc&     hasSubscriptionFunc);

        static void DropReferences(CounterMap&                    counterMap,
                                   const DropReferencesFunc&      dropReferencesFunc);

        static void DropReferences(RegistrationCounterMap&        counterMap,
                                   const DropReferencesFunc&      dropReferencesFunc);

        static bool HasReference(const ConsumerId&  consumer,
                                 const CounterMap&  counterMap);
     
    };

}
}
}

#endif
