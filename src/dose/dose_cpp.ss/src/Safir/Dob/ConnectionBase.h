/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#ifndef _SAFIR_DOB_CONNECTION_BASE_H
#define _SAFIR_DOB_CONNECTION_BASE_H

#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/Defs.h>
#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/EntityIterator.h>
#include <Safir/Dob/EntityProxy.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Response.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/InstanceIdPolicy.h>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/EntityId.h>

namespace Safir
{
namespace Dob
{
    /**
     * Common base class for connections to the DOB.
     *
     * There are methods for
     * setting up subscription, register handlers, set and delete entities etc.
     */
    class DOSE_CPP_API ConnectionBase
    {
    public:

        /** Constructor
         */
        ConnectionBase();

        /** Destructor.
         */
        virtual ~ConnectionBase();

        /**
         * Tells if the connection is opened.
         */
        virtual bool IsOpen() const = 0;

        /**
         * @name Non-pending (synchronous) registration of entity handler.
         */
        /** @{ */

        /**
         * Registration of an entity handler for a given type.
         *
         * Used to make a non-pending registration for a handler for a given type. Upon return from this method the given
         * handler is guaranteed to be registered. (Any existing handler with the same id will be revoked.)
         *
         * This is the preferred registration method to be used for a handler that has no need to get
         * external entity injections (for example when there is no redundancy).
         *
         * There can be any number of registered handlers for a type as long as each handler has
         * a unique id.
         *
         * Note that if you have a configuration where more than one application (connection) is registering
         * the same type/handler, your own registration can still be revoked by another application.
         *
         * @param [in] typeId  Entity type to register.
         * @param [in] handlerId Handler id.
         * @param [in] instanceIdPolicy Specifies if the handler expects instance ids in create requests to be
         *                              assigned by the requestor (normal case) or if the handler assigns them by itself.
         * @param [in] entityHandler Callback consumer object.
         */
        void RegisterEntityHandler(const Safir::Dob::Typesystem::TypeId     typeId,
                                   const Dob::Typesystem::HandlerId&        handlerId,
                                   const Dob::InstanceIdPolicy::Enumeration instanceIdPolicy,
                                   Dob::EntityHandler* const                entityHandler) const;

        /**
         * Register an entity handler that also gets informed about injected entities.
         *
         * Used to make a non-pending registration for a handler for a given type when the handler
         * also needs to be informed when an entity instance is about to be injected in the system.
         * Upon return from this method the given handler is guaranteed to be registered.
         *
         * There are two typical cases when entities are injected outside the control of a registered
         * handler:
         *
         * @li Reception of persistent entity instances.
         * @li Reception of entity instances from an external system installation (multi-owned entities).
         *
         * After registration, any persistent data will be transfered to the handler via
         * EntityInjectionBase#OnInjectedNewEntity callbacks. When all persistent data have been transfered
         * the handler is notified by the callback EntityInjectionBase#OnInitialInjectionsDone. This callback
         * is guaranteed to be invoked even when there is no persistent data at all.
         *
         * @param [in] typeId  Entity type to register.
         * @param [in] handlerId Handler id.
         * @param [in] instanceIdPolicy Specifies if the handler expects instance ids in create requests to be
         *                              assigned by the requestor (normal case) or if the handler assigns them by itself.
         * @param [in] entityHandlerInjection Callback consumer object.
         */
        void RegisterEntityHandlerInjection(const Safir::Dob::Typesystem::TypeId      typeId,
                                            const Dob::Typesystem::HandlerId&         handlerId,
                                            const Dob::InstanceIdPolicy::Enumeration  instanceIdPolicy,
                                            Dob::EntityHandlerInjection* const        entityHandlerInjection) const;
        /** @} */

        /**
         * @name Pending (asynchronous) registration of entity handler.
         */
        /** @{ */

        /**
         * Pending registration of a handler for a given entity type.
         *
         * Used to make a pending registration of a handler for a given type. Upon return from this method the given
         * handler is registered as pending. In case an existing handler with the same handler id isn't already
         * registered the Dob will immediately promote this handler to be the registered one.
         *
         * The consumer is informed via callbacks of any change of the registration status (Completed or Revoked).
         *
         * This method is to be used by applications that handles redundancy. The typical scenario
         * is that two or more application instances make a pending registration with the same handler id.
         * The Dob will assure that one of the handlers is promoted to registered and all others are pending. If
         * there are several types involved in the redundancy switch it is often the case that you want
         * just one application (the active one) to have all types registered. To achieve this, all applications make
         * a pending registration for one of the types. The application that receives the Completed status
         * then makes a non-pending registration for the remaining types using method RegisterEntityHandler()
         * (or RegisterEntityHandlerInjection()).
         *
         * @param [in] typeId  Entity type to register.
         * @param [in] handlerId Handler id.
         * @param [in] instanceIdPolicy Specifies if the handler expects instance ids in create requests to be
         *                              assigned by the requestor (normal case) or if the handler assigns them by itself.
         * @param [in] entityHandlerPending Callback consumer object.
         */
        void RegisterEntityHandlerPending(const Dob::Typesystem::TypeId             typeId,
                                          const Dob::Typesystem::HandlerId&         handlerId,
                                          const Dob::InstanceIdPolicy::Enumeration  instanceIdPolicy,
                                          Dob::EntityHandlerPending* const          entityHandlerPending) const;

        /** @} */

        /**
         * @name Non-pending (synchronous) registration of service handler.
         */
        /** @{ */

        /**
         * Register a service handler for a given type.
         *
         * @see RegisterEntityHandler
         *
         * @param [in] typeId  Service type to register.
         * @param [in] handlerId Handler id.
         * @param [in] serviceHandler Callback consumer object.
         */
        void RegisterServiceHandler(const Safir::Dob::Typesystem::TypeId      typeId,
                                    const Dob::Typesystem::HandlerId&         handlerId,
                                    Dob::ServiceHandler* const                serviceHandler) const;

        /** @} */

        /**
         * @name Pending (asynchronous) registration of service handler.
         */
        /** @{ */

        /**
         * Pending registration of a handler for a given service type.
         *
         * @see RegisterEntityHandlerPending
         *
         * @param [in] typeId  Service type to register.
         * @param [in] handlerId Handler id.
         * @param [in] serviceHandlerPending Callback consumer object.
         */
        void RegisterServiceHandlerPending(const Dob::Typesystem::TypeId             typeId,
                                           const Dob::Typesystem::HandlerId&         handlerId,
                                           Dob::ServiceHandlerPending* const         serviceHandlerPending) const;
        /** @} */

        /**
         * @name Unregistration
         */
        /** @{ */


        /**
         * Unregister of an entity handler or a service handler.
         *
         * Any created entity instance owned by the given handler will be deleted.
         *
         * This method can also be used to unregister a pending handler.
         *
         * Using the constant Dob::Typesystem::HandlerId::ALL_HANDLERS means that all handlers
         * for the given type, registered by this connection, will be unregistered.
         *
         * If the given handler isn't registered the call will be ignored.
         *
         * @param [in] typeId Type id of the entity or service to unregister.
         * @param [in] handlerId Handler id.
         */
        void UnregisterHandler(const Dob::Typesystem::TypeId        typeId,
                               const Dob::Typesystem::HandlerId&    handlerId) const;

        /** @} */

        /**
         * @name Message subscriptions
         */
        /** @{ */

        /**
         * Set up subscription for messages of a certain type and its subclasses.
         *
         * The subscriber can subscribe for messages sent on a specific cannel, or by using
         * the constant Dob::Typesystem::ChannelId::ALL_CHANNELS, subscribe for all messages
         * of the given type regardless of the channel id set by the sender.
         *
         * Calling this method is identical to calling the SubscribeMessage below with
         * includeSubclasses = true.
         *
         * @param [in] typeId Type id of the message to subscribe for.
         * @param [in] channelId Channel id.
         * @param [in] messageSubscriber MessageSubscriber that will receive the messages.
         */
        void SubscribeMessage(const Dob::Typesystem::TypeId     typeId,
                              const Dob::Typesystem::ChannelId& channelId,
                              Dob::MessageSubscriber* const     messageSubscriber) const;

        /**
         * Set up subscription for messages of a certain type (additional parameters).
         *
         * Overloaded method that gives the user the ability to determine if the subscription
         * also will include subsclasses.
         *
         * @param [in] typeId Type id of the message or to subscribe for.
         * @param [in] channelId Channel id.
         * @param [in] includeSubclasses True => Subscription for this message type and all its subclasses.
         *                               False => No subclasses will be included.
         * @param [in] messageSubscriber MessageSubscriber that will receive the messages.
         */
        void SubscribeMessage(const Dob::Typesystem::TypeId     typeId,
                              const Dob::Typesystem::ChannelId& channelId,
                              const bool                        includeSubclasses,
                              Dob::MessageSubscriber* const     messageSubscriber) const;

        /**
         * Remove a message subscription made by the given subscriber.
         *
         * Removes the subscription for the given type and its subclasses.
         *
         * Using the constant Dob::Typesystem::ChannelId::ALL_CHANNELS means that all subscriptions
         * for the given type and its subclasses will be removed.
         *
         * If no subscription exists the call will be ignored.
         *
         * Calling this method is identical to calling the UnsubscribeMessage below with
         * includeSubclasses = true.
         *
         * @param [in] typeId Type id of the message to unsubscribe for.
         * @param [in] channelId Channel id.
         * @param [in] messageSubscriber The MessageSubscriber consumer that was used when
         *                               the subscription was initiated.
         */
        void UnsubscribeMessage(const Dob::Typesystem::TypeId     typeId,
                                const Dob::Typesystem::ChannelId& channelId,
                                Dob::MessageSubscriber* const     messageSubscriber) const;

         /**
         * Remove a message subscription made by the given subscriber (additional parameters).
         *
         * Overloaded method that gives the user the ability to determine if unsubscription
         * also will include subsclasses.
         *
         * @param [in] typeId Type id of the message to unsubscribe for.
         * @param [in] channelId Channel id.
         * @param [in] includeSubclasses True => Unsubscribe for this message type and all its subclasses.
         *                               False => No subclasses will be included.
         * @param [in] messageSubscriber The MessageSubscriber consumer that was used when
         *                               the subscription was initiated.
         */
        void UnsubscribeMessage(const Dob::Typesystem::TypeId     typeId,
                                const Dob::Typesystem::ChannelId& channelId,
                                const bool                        includeSubclasses,
                                Dob::MessageSubscriber* const     messageSubscriber) const;


        /** @} */

        /**
         * @name Entity subscriptions
         */
        /** @{ */

        /**
         * Set up subscription for instances of an entity type and its subclasses.
         *
         * The subscriber will receive information about creations, updates and deletes of
         * instances of the given entity type and its subclasses.
         *
         * When setting up a subscription the user will get initial data for existing entity
         * instances in the form of OnNewEntity callbacks. This is true even when
         * setting up a subscription for an already subscribed entity.
         *
         * @param [in] typeId Type id of the entity to subscribe for.
         * @param [in] entitySubscriber EntitySubscriber that will receive the entities.
         */
        void SubscribeEntity(const Dob::Typesystem::TypeId      typeId,
                             Dob::EntitySubscriber* const       entitySubscriber) const;

        /**
         * Subscription for an entity type (additional parameters).
         *
         * Overloaded method that gives the user the ability to a determine more details
         * conscerning the subscription.
         *
         * @param [in] typeId Type id of the entity to subscribe for.
         * @param [in] includeUpdates True => Subscription includes update, as well as create and delete.
         *                            False => Subscription includes no updates, only create and deletion.
         * @param [in] includeSubclasses True => Subscription for this entity type and all its subclasses.
         *                               False => No subclasses will be included.
         * @param [in] restartSubscription True=> OnNewEntity callbacks are generated even if the subscription already exists.
         *                                 False=> OnNewEntity callbacks are generated only for instances that are not previously subscribed.
         * @param [in] entitySubscriber EntitySubscriber that will receive the entities.
         */
        void SubscribeEntity(const Dob::Typesystem::TypeId      typeId,
                             const bool                         includeUpdates,
                             const bool                         includeSubclasses,
                             const bool                         restartSubscription,
                             Dob::EntitySubscriber* const       entitySubscriber) const;

        /**
         * Set up subscription for a specific instance of an entity type.
         *
         * When setting up a subscription the user will get initial data in the form of a OnNewEntity callback
         * with the current state for the subscribed entity (if created).
         *
         * @param [in] entityId Entity id of the entity to subscribe for.
         * @param [in] includeUpdates True => Subscription includes update, as well as create and delete.
         *                            False => Subscription includes no updates, only create and deletion.
         * @param [in] restartSubscription True=> An OnNewEntity callback will be generated even if the subscription already exists.
         *                                 False=> An OnNewEntity callback is generated only if the instance is not previously subscribed.
         * @param [in] entitySubscriber EntitySubscriber that will receive the entities.
         */
        void SubscribeEntity(const Dob::Typesystem::EntityId&    entityId,
                             const bool                          includeUpdates,
                             const bool                          restartSubscription,
                             Dob::EntitySubscriber* const        entitySubscriber) const;

        /**
         * Remove an entity subscription made by the given subscriber.
         *
         * Removes the subscription for the given type and its subclasses.
         *
         * If no subscription exists the call will be ignored.
         *
         * @param [in] typeId Type id of the entity to unsubscribe for.
         * @param [in] entitySubscriber The EntitySubscriber consumer that was used when
         *                              the subscription was initiated.
         */
        void UnsubscribeEntity(const Dob::Typesystem::TypeId        typeId,
                               Dob::EntitySubscriber* const         entitySubscriber) const;

        /**
         * Remove an entity subscription made by the given subscriber (additional parameters).
         *
         * Overloaded method that gives the user the ability to determine if unsubscription
         * also will include subsclasses..
         *
         * @param [in] typeId Type id of the entity to unsubscribe for.
         * @param [in] includeSubclasses True => Unsubscribe for this entity type and all its subclasses.
         *                               False => Unsubscribe for just this type (no subclasses).
         * @param [in] entitySubscriber The EntitySubscriber consumer that was used when
         *                              the subscription was initiated.
         */
        void UnsubscribeEntity(const Dob::Typesystem::TypeId        typeId,
                               const bool                           includeSubclasses,
                               Dob::EntitySubscriber* const         entitySubscriber) const;

        /**
         * Remove an entity instance subscription made by the given subscriber.
         *
         * If no subscription exists the call will be ignored.
         *
         * @param [in] entityId Entity id of the entity instance.
         * @param [in] entitySubscriber The EntitySubscriber consumer that was used when
         *                              the subscription was initiated.
         */
        void UnsubscribeEntity(const Dob::Typesystem::EntityId& entityId,
                               Dob::EntitySubscriber* const     entitySubscriber) const;
        /** @} */

        /**
         * @name Registration subscriptions
         */
        /** @{ */

        /**
         * Set up subscription for notifications about when a specific handler for an entity type or
         * a service type is registered and unregistered.
         *
         * Using the constant Dob::Typesystem::HandlerId::ALL_HANDLERS means that the subscriber will receive
         * registrations/unregistrations of any handler for the given type.
         *
         * Using a specific handler id means that the subscriber will receive only the
         * registrations/unregistrations of the specified handler.
         *
         * When setting up a subscription the user will get initial information about existing handlers via
         * OnRegistered callbacks.
         *
         * @param [in] typeId Type id of entity or service.
         * @param [in] handlerId Handler id.
         * @param [in] includeSubclasses True => Subscription for this entity type or service type and all its subclasses.
         *                               False => No subclasses will be included.
         * @param [in] restartSubscription True=> OnRegistered callbacks are generated even if the subscription already exists.
         *                                 False=> OnRegistered callbacks are generated only for handlers that are not previously subscribed.
         * @param [in] registrationSubscriber RegistrationSubscriber that will receive the subscription
         *                                    response.
         */
        void SubscribeRegistration(const Dob::Typesystem::TypeId            typeId,
                                   const Dob::Typesystem::HandlerId&        handlerId,
                                   const bool                               includeSubclasses,
                                   const bool                               restartSubscription,
                                   Dob::RegistrationSubscriber* const       registrationSubscriber) const;

        /**
         * Removes a registration subscription.
         *
         * Using the constant Dob::Typesystem::HandlerId::ALL_HANDLERS means that all registration subscriptions,
         * for the given type id and consumer, are removed.
         *
         * If no subscription exists the call will be ignored.
         *
         * @param [in] typeId Type id of entity or service.
         * @param [in] handlerId Handler id.
         * @param [in] includeSubclasses True => Unsubscribe for this entity type or service type and all its subclasses.
         *                               False => No subclasses will be included.
         * @param [in] registrationSubscriber The registrationSubscriber consumer that was used when
         *                                    the subscription was initiated.
         */
        void UnsubscribeRegistration(const Dob::Typesystem::TypeId      typeId,
                                     const Dob::Typesystem::HandlerId&  handlerId,
                                     const bool                         includeSubclasses,
                                     Dob::RegistrationSubscriber* const registrationSubscriber) const;
        /** @} */

        /**
         * @name Send messages
         */
        /** @{ */

        /**
         * Send a message on the specified channel.
         *
         * The application must be prepared to handle the situation that the outgoing send queue is full
         * (OverflowException is thrown). In this case the application is responsible for resending
         * the message. When the overflow situation is dissolved, the application is notified by the
         * MessageSender::OnNotMessageOverflow callback, which should trig the resending.
         *
         * @param [in] message Message to send.
         * @param [in] channelId Channel id.
         * @param [in] messageSender MessageSender for notification about overflow status.
         *
         * @throws Safir::Dob::OverflowException There was an overflow when sending.
         */
        void Send(const Dob::MessagePtr&            message,
                  const Dob::Typesystem::ChannelId& channelId,
                  Dob::MessageSender* const         messageSender) const;

        /** @} */

        /**
         * @name Requests
         */
        /** @{ */

        /**
         * Request to a handler to create an entity instance without specifying the instanceId.
         *
         * If the handler is registered as "HandlerDecidesInstanceId" the InstanceId of the
         *   entity to be created is determined by the application, and the requestor is
         *   told which InstanceId will be used by the EntityIdResponse.
         * If the handler is registered as "RequestorDecidesInstanceId" an InstanceId will
         *   be randomly generated and included in the request. This InstanceId *must* be
         *   used by the handler as the new instance number.
         *
         * The application must be prepared to handle the situation that the outgoing send queue is full
         * (OverflowException is thrown). In this case the application is responsible for resending
         * of the create request. When the overflow situation is dissolved, the application is
         * notified by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
         *
         * @param [in] request Entity requested to be created.
         * @param [in] handlerId Handler id.
         * @param [in] requestor Requestor for response and notification about overflow status.
         *
         * @return Request id that can be used to match sent request with the response.
         *
         * @throws Safir::Dob::OverflowException There was an overflow when sending.
         */
        Dob::RequestId CreateRequest(const Dob::EntityPtr&               request,
                                     const Dob::Typesystem::HandlerId&   handlerId,
                                     Dob::Requestor* const               requestor) const;

        /**
         * Request to a handler to create a specific entity instance.
         *
         * If the handler is registered as "RequestorDecidesInstanceId" the Requestor must
         * specify which instance is to be created. (If it doesnt care it can use the method
         * above that will generate one randomly.)
         * If the Requestor wants a random instance, but needs to know which instance will
         * get created it can use Dob:Typesystem:InstanceId::GenerateRandom() to generate
         * an InstanceId to pass to this method
         *
         * Note that it is illegal to call this method if the handler is registered as
         * "HandlerDecidesInstanceId".
         *
         * The application must be prepared to handle the situation that the outgoing send queue is full
         * (OverflowException is thrown). In this case the application is responsible for resending
         * of the entity create request. When the overflow situation is dissolved, the application is
         * notified by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
         *
         * @param [in] request Entity requested to be created.
         * @param [in] instanceId Instance id.
         * @param [in] handlerId Handler id.
         * @param [in] requestor Requestor for response and notification about overflow status.
         *
         * @return Request id that can be used to match sent request with the response.
         *
         * @throws Safir::Dob::OverflowException There was an overflow when sending.
         */
        Dob::RequestId CreateRequest(const Dob::EntityPtr&              request,
                                     const Dob::Typesystem::InstanceId& instanceId,
                                     const Dob::Typesystem::HandlerId&  handlerId,
                                     Dob::Requestor* const              requestor) const;

        /**
         * Send an update request on an existing entity instance.
         *
         * An update request will be sent to the handler that owns (has created) the entity.
         *
         * The application must be prepared to handle the situation that the outgoing send queue is full
         * (OverflowException is thrown). In this case the application is responsible for resending
         * of the entity update request. When the overflow situation is dissolved, the application is
         * notified by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
         *
         * @param [in] request Entity requested to be updated.
         * @param [in] instanceId Instance id.
         * @param [in] requestor Requestor for response and notification about overflow status.
         *
         * @return Request id that can be used to match sent request with the response.
         *
         * @throws Safir::Dob::OverflowException There was an overflow when sending.
         */
        Dob::RequestId UpdateRequest(const Dob::EntityPtr&              request,
                                     const Dob::Typesystem::InstanceId& instanceId,
                                     Dob::Requestor* const              requestor) const;

        /**
         * Send a delete request on an existing entity instance.
         *
         * A delete request will be sent to the handler that owns (has created) the entity.
         *
         * The application must be prepared to handle the situation that the outgoing send queue is full
         * (OverflowException is thrown). In this case the application is responsible for resending
         * of the entity delete request. When the overflow situation is dissolved, the application is
         * notified by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
         *
         * @param [in] entityId EntityId of the entity to be deleted.
         * @param [in] requestor Requestor for response and notification about overflow status.
         *
         * @return Request id that can be used to match sent request with the response.
         *
         * @throws Safir::Dob::OverflowException There was an overflow when sending.
         */
        Dob::RequestId DeleteRequest(const Dob::Typesystem::EntityId& entityId,
                                     Dob::Requestor* const            requestor) const;

        /**
         * Send a request to the given service handler.
         *
         * The application must be prepared to handle the situation that the outgoing send queue is full
         * (OverflowException is thrown). In this case the application is responsible for resending
         * of the service request. When the overflow situation is dissolved, the application is notified
         * by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
         *
         * @param [in] request The service request.
         * @param [in] handlerId Service handler id.
         * @param [in] requestor Requestor for service response and notification about overflow status.
         *
         * @return Request id that can be used to match sent request with the response.
         *
         * @throws Safir::Dob::OverflowException There was an overflow when sending.
         */
        Dob::RequestId ServiceRequest(const Dob::ServicePtr&                  request,
                                      const Dob::Typesystem::HandlerId&       handlerId,
                                      Dob::Requestor* const                   requestor) const;

        /** @} */

        /**
         * @name Entity Owners
         */
        /** @{ */

        /**
         * Merge the changed members of an entity straight into the pool (the given handler must be the owner).
         *
         * All members of the given entity that are marked as changed will be merged into the
         * current object in the pool.
         * If the object is not already set in the pool the entity will be set without any merging.
         *
         * @param [in] entity Entity to create or update.
         * @param [in] instanceId Instance id.
         * @param [in] handlerId Handler id.
         *
         * @throws Safir::Dob::AccessDeniedException The instance is owned by another handler.
         */
        void SetChanges(const Dob::EntityPtr&              entity,
                        const Dob::Typesystem::InstanceId& instanceId,
                        const Dob::Typesystem::HandlerId&  handlerId) const;

        /**
         * Allows an entity handler to create or update an entity.
         *
         * A call to SetAll will replace all members of any existing entity with the given entity. I.e. the DOB
         * will not merge the changes with any existing data, but will instead completely replace the old
         * data. Use the SetChanges() method to make the DOB merge the data into the pool.
         *
         * Special care must be taken when the owner sets an entity that contains pointers (Entity ids)
         * to other entity instances, and the lifetime of the pointed-to instance is completly connected
         * to the lifetime of the referer. In this case the owner must traverse the object-graph
         * and any unreferenced instance must be deleted.
         *
         * @param [in] entity Entity to create or update.
         * @param [in] instanceId Instance id.
         * @param [in] handlerId Handler id.
         *
         * @throws Safir::Dob::AccessDeniedException The instance is owned by another handler.
         */
        void SetAll(const Dob::EntityPtr&              entity,
                    const Dob::Typesystem::InstanceId& instanceId,
                    const Dob::Typesystem::HandlerId&  handlerId) const;

        /**
         * Allows an entity handler to delete a specific owned entity instance.
         *
         * Used to delete a specific owned instance. Does nothing if the instance does not exist.
         *
         * @param [in] entityId Id of the entity to delete.
         * @param [in] handlerId Handler id.
         *
         * @throws Safir::Dob::AccessDeniedException The instance is owned by another handler.
         */
        void Delete(const Dob::Typesystem::EntityId&    entityId,
                    const Dob::Typesystem::HandlerId&   handlerId) const;

        /**
         * Allows an entity handler to delete all owned instances.
         *
         * Used to delete all instances owned by the caller.
         *
         * @param [in] typeId Entity type.
         * @param [in] handlerId Handler id.
         */
        void DeleteAllInstances(const Dob::Typesystem::TypeId       typeId,
                                const Dob::Typesystem::HandlerId&   handlerId) const;

        /** @} */

         /**
         * @name Get iterators
         */
        /** @{ */

        /**
         * Retreives an STL compliant iterator to iterate over created instances.
         *
         * The iterator addresses the first created entity instance of the given type. If there
         * are no created instances an iterator representing "end" will be returned.
         *
         * Example use:
         * for (EntityIterator it = m_connection.GetEntityIterator(Entity::ClassTypeId,true);
         *      it != Safir::Dob::EntityIterator(); ++it)
         * {
         *     do something with it.
         * }
         *
         * @param [in] typeId Entity type.
         * @param [in] includeSubclasses True =>  Iterate over subclass instances
         *                               False => No subclasses will be included.
         */
        Dob::EntityIterator GetEntityIterator(const Dob::Typesystem::TypeId  typeId,
                                              const bool                     includeSubclasses) const;

        /**
         * @name Read operations
         */
        /** @{ */

        /**
         * Read an entity from the distributed object pool.
         *
         * Gets the current version of the entity that matches the given entity id.
         *
         * @param [in] entityId Entity id of the entity to read.
         *
         * @return Entity read from the distributed object pool.
         *
         * @throws Safir::Dob::NotFoundException The specified instance of the entity does not exist.
         */
        const Dob::EntityProxy Read(const Dob::Typesystem::EntityId & entityId) const;

        /**
         * Check if an instance of an entity is created or not.
         *
         * This method will return true if the given entity instance is created.
         *
         * Note that the only time that you can really trust this information is if you're the one that
         * has created the entity instance and no one is overregistering a handler with the same id as yours.
         * Otherwise there is no guarantee that the instance still is created immediately after this call.
         * The owner may be deleting it right after you asked, or your handler may have been revoked but
         * you have not yet received a Revoke status.
         * Use with care!
         *
         * @param [in] entityId Entity instance to check existence of.
         * @return True if the entity instance is created, otherwise false.
         */
        bool IsCreated(const Dob::Typesystem::EntityId & entityId) const;

        /**
         * This method is used to get the number of instances of an entity that exists.
         *
         * @param [in] typeId The type of the class whose instances we're counting.
         * @param [in] handlerId Count only instances owned by this handler (use HandlerId::ALL_HANDLERS
         *                        to get all handlers).
         * @param [in] includeSubclasses Include subclasses when counting instances.
         */
        Dob::Typesystem::Int64 GetNumberOfInstances(const Dob::Typesystem::TypeId typeId,
                                                    const Dob::Typesystem::HandlerId& handlerId,
                                                    const bool includeSubclasses) const;

        /**
        * This method is used to get the instanceIdPolicy for a specific class and handler.
        *
        * @param [in] typeId       The type of the class the handler is registered for.
        * @param [in] handlerId    Get instanceIdPolicy for this handler.
        * @return instanceIdPolicy Specifies if the handler expects instance ids in create requests to be
        *                          assigned by the requestor or if the handler assigns them by itself.        
        *
        * @throws Safir::Dob::NotFoundException The given handlerId has not registered the given class.
        */
        Dob::InstanceIdPolicy::Enumeration GetInstanceIdPolicy(const Dob::Typesystem::TypeId typeId,
                                                               const Dob::Typesystem::HandlerId& handlerId) const;

        /** @} */

        /**
         * @name Exit dispatch
         */
        /** @{ */

        /**
         * Interrupt the ongoing Dispatch even if all data to the application have not been distpatched.
         * The dispatch-event will be automatically set to trigger a new Dispatch again.
         * This can be used to ensure that too much time is not spent dispatching in a time-critical app.
         */
        void ExitDispatch() const;

        /** @} */

    private:
        //Disable copying and assignment
        ConnectionBase(const ConnectionBase& d);
        ConnectionBase& operator=(const ConnectionBase& rhs);

        friend class ConnectionAspectBase;

        virtual long GetControllerId() const = 0;

        void Set(const Dob::EntityPtr&              entity,
                 const Dob::Typesystem::InstanceId& instanceId,
                 const Dob::Typesystem::HandlerId&  handlerId,
                 const bool                         considerChangeFlags) const;
    };

}
}

#endif
