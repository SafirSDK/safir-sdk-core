/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#ifndef _SAFIR_DOB_CONSUMER_H
#define _SAFIR_DOB_CONSUMER_H

#include <Safir/Dob/ConsumerBase.h>
#include <Safir/Dob/Defs.h>
#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/EntityProxy.h>
#include <Safir/Dob/EntityRequestProxy.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/MessageProxy.h>
#include <Safir/Dob/Response.h>
#include <Safir/Dob/ResponseProxy.h>
#include <Safir/Dob/ResponseSender.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/ServiceRequestProxy.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>

namespace Safir
{
namespace Dob
{

    /**
     * Interface for reception of a stop order
     */
    class DOSE_CPP_API StopHandler:
        public virtual Internal::ConsumerBase
    {
    public:
        /** Virtual destructor is needed since we have virtual member functions. */
        virtual ~StopHandler() {}

        /**
         * When called the application owning the connection shall stop its execution.
         */
        virtual void OnStopOrder() = 0;

    private:
        virtual StopHandler * ToStopHandler() {return this;}
    };

    /**
     * Interface for reception of a dispatch order.
     */
    class DOSE_CPP_API Dispatcher:
        public virtual Internal::ConsumerBase
    {
    public:
        /** Virtual destructor is needed since we have virtual member functions. */
        virtual ~Dispatcher() {}

        /**
         * Indicates that there is incoming data for the connection so the application shall
         * call Dispatch().
         *
         * When this method is called the application MUST call the Dispatch() method for the connection.
         * Note that Dispatch() is NOT to be called directly from within this method. Instead the application
         * shall set an event or similar and then call Dispatch() from the thread that owns (has called Open)
         * the connection.
         */
        virtual void OnDoDispatch() = 0;

    private:
        virtual Dispatcher * ToDispatcher() {return this;}
    };

    /**
     * Interface to be implemented by an entity handler that makes a non-pending registration
     * and that doesn't handle injected entities.
     *
     * The following methods needs to be implemented:
     *
     * virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
     *                                    const Safir::Dob::Typesystem::HandlerId& handlerId);
     *
     * virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
     *                              Safir::Dob::ResponseSenderPtr        responseSender);
     *
     * virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
     *                              Safir::Dob::ResponseSenderPtr        responseSender);
     *
     * virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
     *                              Safir::Dob::ResponseSenderPtr        responseSender);
     */
    class DOSE_CPP_API EntityHandler
        : public virtual RevokedRegistrationBase,
          public virtual EntityRequestBase
    {

    private:
        virtual EntityHandler * ToEntityHandler() {return this;}
    };

    /**
     * Interface to be implemented by an entity handler that makes a non-pending registration
     * for a type that can potentially be injected.
     *
     * The following methods NEED to be implemented:
     * ============================================
     *
     * virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
     *                                    const Safir::Dob::Typesystem::HandlerId& handlerId);
     *
     * virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
     *                              Safir::Dob::ResponseSenderPtr        responseSender);
     *
     * virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
     *                              Safir::Dob::ResponseSenderPtr        responseSender);
     *
     * virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
     *                              Safir::Dob::ResponseSenderPtr        responseSender);
     *
     * The following methods MAY be overridden:
     * =========================================
     *
     * virtual void OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy);
     *
     * virtual void OnInjectedUpdatedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy);
     *
     * virtual void OnInjectedDeletedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy);
     *
     * virtual void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId,
                                            const Safir::Dob::Typesystem::HandlerId& handlerId);
     */
    class DOSE_CPP_API EntityHandlerInjection
        : public virtual RevokedRegistrationBase,
          public virtual EntityInjectionBase
    {

    private:
        virtual EntityHandlerInjection * ToEntityHandlerInjection() {return this;}
    };

    /**
     * Interface to be implemented by an entity handler that makes a pending registration.
     *
     * The following methods NEED to be implemented:
     * ============================================
     *
     * virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
     *                                    const Safir::Dob::Typesystem::HandlerId& handlerId);
     *
     * virtual void OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
     *                                      const Safir::Dob::Typesystem::HandlerId& handlerId);
     *
     * virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
     *                              Safir::Dob::ResponseSenderPtr        responseSender);
     *
     * virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
     *                              Safir::Dob::ResponseSenderPtr        responseSender);
     *
     * virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
     *                              Safir::Dob::ResponseSenderPtr        responseSender);
     *
     * The following methods MAY be overridden:
     * =========================================
     *
     * virtual void OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy);
     *
     * virtual void OnInjectedUpdatedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy);
     *
     * virtual void OnInjectedDeletedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy);
     *
     * virtual void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId,
                                            const Safir::Dob::Typesystem::HandlerId&     handlerId);
     */
    class DOSE_CPP_API EntityHandlerPending
        : public virtual CompletedRegistrationBase,
          public virtual EntityInjectionBase
    {

    private:
        virtual EntityHandlerPending * ToEntityHandlerPending() {return this;}
    };

    /**
     * Interface to be implemented by a service handler that makes a non-pending registration.
     *
     * The following methods NEED to be implemented:
     * ============================================
     *
     * virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
     *                                    const Safir::Dob::Typesystem::HandlerId& handlerId);
     *
     * virtual void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
     *                               Safir::Dob::ResponseSenderPtr         responseSender);
     */
    class DOSE_CPP_API ServiceHandler
        : public virtual RevokedRegistrationBase,
          public virtual ServiceRequestBase
    {

    private:
        virtual ServiceHandler * ToServiceHandler() {return this;}
    };

    /**
     * Interface to be implemented by a service handler that makes a pending registration.
     *
     * The following methods NEED to be implemented:
     * ============================================
     *
     * virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
     *                                    const Safir::Dob::Typesystem::HandlerId& handlerId);
     *
     * virtual void OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
     *                                      const Safir::Dob::Typesystem::HandlerId& handlerId);
     *
     * virtual void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
     *                               Safir::Dob::ResponseSenderPtr       responseSender);
     */
    class DOSE_CPP_API ServiceHandlerPending
        : public virtual CompletedRegistrationBase,
          public virtual ServiceRequestBase
    {

    private:
        virtual ServiceHandlerPending * ToServiceHandlerPending() {return this;}
    };

    /**
     * Interface to be implemented by an application that sends requests (Request on entities or
     * service requests).
     */
    class DOSE_CPP_API Requestor:
        public virtual Internal::ConsumerBase
    {
    public:
        /** Virtual destructor is needed since we have virtual member functions. */
        virtual ~Requestor() {}

        /**
         * Called when a response is received on a sent request.
         *
         * @param [in] responseProxy - Response proxy.
         */
        virtual void OnResponse(const Safir::Dob::ResponseProxy responseProxy) = 0;

        /**
         * Called to indicate that it is meningful to make a retry after an overflow situation.
         */
        virtual void OnNotRequestOverflow() = 0;

    private:
        virtual Requestor * ToRequestor() {return this;}
    };

    /**
     * Interface to be implemented by senders of messages.
     */
    class DOSE_CPP_API MessageSender:
        public virtual Internal::ConsumerBase
    {
    public:
        /** Virtual destructor is needed since we have virtual member functions. */
        virtual ~MessageSender() {}

        /**
         * Called to indicate that it is meningful to make a retry after an overflow situation.
         */
        virtual void OnNotMessageOverflow() = 0;

    private:
        virtual MessageSender * ToMessageSender() {return this;}
    };

    /**
     * Interface to be implemented by subscribers of handler registrations.
     */
    class DOSE_CPP_API RegistrationSubscriber:
        public virtual Internal::ConsumerBase
    {
    public:
        /** Virtual destructor is needed since we have virtual member functions. */
        virtual ~RegistrationSubscriber() {}

        /**
         * Called when a handler for an entity or service has been registered.
         *
         * @param [in] typeId - Type id of the registered entity or service.
         * @param [in] handlerId - HandlerId of the registered handler.
         */
        virtual void OnRegistered(const Safir::Dob::Typesystem::TypeId      typeId,
                                  const Safir::Dob::Typesystem::HandlerId&  handlerId) = 0;

        /**
         * Called when a handler for an entity or service has been unregistered.
         *
         * @param [in] typeId - Type id of the unregistered entity or service.
         * @param [in] handlerId - HandlerId of the unregistered handler.
         */
        virtual void OnUnregistered(const Safir::Dob::Typesystem::TypeId      typeId,
                                    const Safir::Dob::Typesystem::HandlerId&  handlerId) = 0;

    private:
        virtual RegistrationSubscriber * ToRegistrationSubscriber() {return this;}
    };

    /**
     * Interface to be implemented by subscribers of messages.
     */
    class DOSE_CPP_API MessageSubscriber:
        public virtual Internal::ConsumerBase
    {
    public:
        /** Virtual destructor is needed since we have virtual member functions. */
        virtual ~MessageSubscriber() {}

        /**
         * Called when a message is received.
         *
         * @param [in] messageProxy - Proxy object containing received message and meta information.
         */
        virtual void OnMessage(const Safir::Dob::MessageProxy messageProxy) = 0;

    private:
        virtual MessageSubscriber * ToMessageSubscriber() {return this;}
    };

    /**
     * Interface to be implemented by subscribers of entities.
     */
    class DOSE_CPP_API EntitySubscriber:
        public virtual Internal::ConsumerBase
    {
    public:
        /** Virtual destructor is needed since we have virtual member functions. */
        virtual ~EntitySubscriber() {}

        /**
         * Called when a new entity is available.
         *
         * @param [in] entityProxy - Proxy object containing new entity and meta information.
         */
        virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy) = 0;

        /**
         * Called when an entity is updated.
         *
         * If Change Information is enabled for the subscription those entity members
         * that are changed, compared to the previous received entity, will be marked as
         * changed.
         *
         * The entity owner handler id can be retreived by calling GetCallbackInfo.
         *
         * @param [in] entityProxy - Proxy object containing updated entity and meta information.
         */
        virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy) = 0;

        /**
         * Called when an entity is deleted.
         *
         * @param [in] entityProxy - Proxy object containing deleted entity information.
         * @param [in] deprecated - Deprecated flag.
         */
        virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy,
                                     const bool                    deprecated) = 0;

    private:
        virtual EntitySubscriber * ToEntitySubscriber() {return this;}
    };

}
}


#endif
