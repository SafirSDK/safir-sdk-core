/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#ifndef _SAFIR_DOB_CONSUMER_BASE_H
#define _SAFIR_DOB_CONSUMER_BASE_H

#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/Defs.h>
#include <Safir/Dob/ResponseSender.h>
#include <Safir/Dob/EntityProxy.h>
#include <Safir/Dob/InjectedEntityProxy.h>
#include <Safir/Dob/EntityRequestProxy.h>
#include <Safir/Dob/ServiceRequestProxy.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/Exceptions.h>

#ifdef _MSC_VER
//we need to disable the warning about inheriting by dominance, it is intended to be this way,
//and there is no way to produce code that does not give this warning, unfortunately.
//see also the comment on ConsumerBase
#pragma warning (disable: 4250)
#endif

namespace Safir
{
namespace Dob
{
    //Forward declarations:
    class RevokedRegistrationBase;
    class CompletedRegistrationBase;
    class EntityRequestBase;
    class EntityInjectionBase;
    class ServiceRequestBase;
    class StopHandler;
    class Dispatcher;
    class EntityHandler;
    class EntityHandlerInjection;
    class EntityHandlerPending;
    class ServiceHandler;
    class ServiceHandlerPending;
    class Requestor;
    class MessageSender;
    class RegistrationSubscriber;
    class MessageSubscriber;
    class EntitySubscriber;

    namespace Internal
    {
        //forward declaration
        class Callbacks;

        /**
        * Base class used when composing more elaborated interfaces.
        *
        * This class is used to avoid dynamic_casts. Instead of using a dynamic_cast, the Callbacks
        * can call the corresponding To* function, which will convert the pointer to a consumer of
        * the desired type. This is only a virtual function call, instead of a dynamic_cast, ie
        * potentially much quicker.
        *
        * @headerfile ConsumerBase.h <Safir/Dob/Consumer.h>
        */
        class DOSE_CPP_API ConsumerBase
        {
        public:
            virtual ~ConsumerBase() {}

        private:
            friend class Safir::Dob::Internal::Callbacks;

            virtual RevokedRegistrationBase * ToRevokedRegistrationBase()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToRevokedRegistrationBase is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual CompletedRegistrationBase * ToCompletedRegistrationBase()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToCompletedRegistrationBase is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual EntityRequestBase * ToEntityRequestBase()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToEntityRequestBase is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual EntityInjectionBase * ToEntityInjectionBase()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToEntityInjectionBase is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual ServiceRequestBase * ToServiceRequestBase()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToServiceRequestBase is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual StopHandler * ToStopHandler()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToStopHandler is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual Dispatcher * ToDispatcher()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToDispatcher is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual EntityHandler * ToEntityHandler()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToEntityHandler is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual EntityHandlerInjection * ToEntityHandlerInjection()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToEntityHandlerInjection is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual EntityHandlerPending * ToEntityHandlerPending()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToEntityHandlerPending is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual ServiceHandler * ToServiceHandler()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToServiceHandler is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual ServiceHandlerPending * ToServiceHandlerPending()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToServiceHandlerPending is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual Requestor * ToRequestor()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToRequestor is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual MessageSender * ToMessageSender()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToMessageSender is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual RegistrationSubscriber * ToRegistrationSubscriber()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToRegistrationSubscriber is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual MessageSubscriber * ToMessageSubscriber()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToMessageSubscriber is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}
            virtual EntitySubscriber * ToEntitySubscriber()
            {throw Dob::Typesystem::SoftwareViolationException
                    (L"Call to ConsumerBase::ToEntitySubscriber is illegal. Method must be overridden!",
                     __WFILE__,__LINE__);}


            //These functions take a void ptr and call the desired version of the above methods
            static RevokedRegistrationBase * ToRevokedRegistrationBase(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToRevokedRegistrationBase();}
            static CompletedRegistrationBase * ToCompletedRegistrationBase(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToCompletedRegistrationBase();}
            static EntityRequestBase * ToEntityRequestBase(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToEntityRequestBase();}
            static EntityInjectionBase * ToEntityInjectionBase(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToEntityInjectionBase();}
            static ServiceRequestBase * ToServiceRequestBase(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToServiceRequestBase();}
            static StopHandler * ToStopHandler(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToStopHandler();}
            static Dispatcher * ToDispatcher(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToDispatcher();}
            static EntityHandler * ToEntityHandler(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToEntityHandler();}
            static EntityHandlerInjection * ToEntityHandlerInjection(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToEntityHandlerInjection();}
            static EntityHandlerPending * ToEntityHandlerPending(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToEntityHandlerPending();}
            static ServiceHandler * ToServiceHandler(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToServiceHandler();}
            static ServiceHandlerPending * ToServiceHandlerPending(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToServiceHandlerPending();}
            static Requestor * ToRequestor(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToRequestor();}
            static MessageSender * ToMessageSender(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToMessageSender();}
            static RegistrationSubscriber * ToRegistrationSubscriber(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToRegistrationSubscriber();}
            static MessageSubscriber * ToMessageSubscriber(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToMessageSubscriber();}
            static EntitySubscriber * ToEntitySubscriber(void * consumerBase)
            {return static_cast<ConsumerBase*>(consumerBase)->ToEntitySubscriber();}
        };
    }

    /**
     * Base class used when composing more elaborated interfaces.
     *
     * @headerfile ConsumerBase.h <Safir/Dob/Consumer.h>
     */
    class DOSE_CPP_API RevokedRegistrationBase:
        public virtual Internal::ConsumerBase
    {
    public:
        /** Virtual destructor is needed since we have virtual member functions. */
        virtual ~RevokedRegistrationBase() {}

        /**
         * Indicates that the handler is no longer registered for the given type.
         *
         * @param [in] typeId - Type Id of the entity or service.
         * @param [in] handlerId - Id of the revoked handler.
         */
        virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                           const Safir::Dob::Typesystem::HandlerId& handlerId) = 0;

    private:
        virtual RevokedRegistrationBase * ToRevokedRegistrationBase() {return this;}
    };

    /**
     * Base class used when composing more elaborated interfaces.
     *
     * @headerfile ConsumerBase.h <Safir/Dob/Consumer.h>
     */
    class DOSE_CPP_API CompletedRegistrationBase : public virtual RevokedRegistrationBase
    {
    public:
        /**
         * Indicates that a pending registration has completed and the handler is now registered.
         *
         * @param [in] typeId - Type Id of the entity or service.
         * @param [in] handlerId - Id of the registered handler.
         */
        virtual void OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                             const Safir::Dob::Typesystem::HandlerId& handlerId) = 0;

    private:
        virtual CompletedRegistrationBase * ToCompletedRegistrationBase() {return this;}
    };

    /**
     * Interface to receive entity requests.
     *
     * @headerfile ConsumerBase.h <Safir/Dob/Consumer.h>
     */
    class DOSE_CPP_API EntityRequestBase:
        public virtual Internal::ConsumerBase
    {
    public:
        /** Virtual destructor is needed since we have virtual member functions. */
        virtual ~EntityRequestBase() {}

        /**
         * Called when someone requests an entity to be created.
         *
         * If the handler is registered as "HandlerDecidesInstanceId" the request does not
         *   contain an instance id, and the handler must decide which instance to use and
         *   must send this back to the requestor using an EntityIdResponse.
         *
         * If the handler is registered as "RequestorDecidesInstanceId" the request contains
         *   an instance id, which the handler *must* use if is going to accept the request.
         *   If the instance cannot be used an error response must be sent.
         *   The handler must not send EntityIdResponse on successful create requests when
         *   it is registered as "RequestorDecidesInstanceId".
         *
         * The receiver of the callback must send a response using the responseSender.
         * It is possible to store the responseSender and send the response later after this
         * method has returned. The responseSender is a smart-pointer which means that it
         * will handle deletion of the underlaying object on its own.
         *
         * @param [in] entityRequestProxy - Proxy object containing request and meta information.
         * @param [in] responseSender - Used to send the response for the received request.
         */
        virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                     Safir::Dob::ResponseSenderPtr        responseSender) = 0;

        /**
         * Called when someone requests an entity to be updated.
         *
         * The receiver of the callback must send a response using the responseSender.
         * It is possible to store the responseSender and send the response later after this
         * method has returned. The responseSender is a smart-pointer which means that it
         * will handle deletion of the underlaying object on its own.
         *
         * @param [in] entityRequestProxy - Proxy object containing request and meta information.
         * @param [in] responseSender - Used to send the response for the received request.
         */
        virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                     Safir::Dob::ResponseSenderPtr        responseSender) = 0;

        /**
         * Called when someone requests an entity to be deleted.
         *
         * The receiver of the callback must send a response using the responseSender.
         * It is possible to store the responseSender and send the response later after this
         * method has returned. The responseSender is a smart-pointer which means that it
         * will handle deletion of the underlaying object on its own.
         *
         * @param [in] entityRequestProxy - Proxy object containing request information.
         * @param [in] responseSender - Used to send the response for the received request.
         */
        virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                     Safir::Dob::ResponseSenderPtr        responseSender) = 0;

    private:
        virtual EntityRequestBase * ToEntityRequestBase() {return this;}
    };

    /**
     * This Consumer Base class contains callback methods that can be overridden by an entity 
     * handler that registers a handler for an entity type that can potentially be injected 
     * outside the control of the handler itself.
     *
     * Examples of when an entity is injected outside the control of a handler includes
     * persistent data at startup time and entities received from other system installations
     * (multi-owned entities).
     *
     * The handler can reject the injection by invoking Delete for the received entity.
     *
     * In case the handler has no need to be informed when an entity is about to be injected
     * it can rely on the default implementation which just accepts the injected entity.
     *
     * An example of a situation where a handler needs to act on injected data is when the entity
     * has members that are references (EntityIds) to other entity instances, and the lifetime
     * of the pointed-to instance is completly connected to the lifetime of the referer.
     * If an entity has been injected it is the responsibility of the local handler to
     * traverse the entity instance graph, find changed references, and delete the
     * unreferenced entity instances. To support this task there are methods in the InjectedEntityProxy
     * class to read the previous entity state and find invalid refrences.
     *
     * Another example is when an injection relates only to some of the members (might be the case
     * when updates are received from external systems) and the handler wants to maintain some sort
     * of consistency between different members. In this case the handler can call
     * ConnectionAspectPostpone#IncompleteInjectionState to wait for more updates before the new state
     * is set.
     *
     * @headerfile ConsumerBase.h <Safir/Dob/Consumer.h>
     */
    class DOSE_CPP_API EntityInjectionBase : public virtual EntityRequestBase
    {
    public:
        /**
         * Called when a new entity is about to be injected in the system.
         *
         * @param [in] injectedEntityProxy - Proxy object containing entity and meta information 
         *                                   about the new entity that is about to be injected.
         */
        virtual void OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy) {}

        /**
         * Called when an updated entity is about to be injected in the system.
         *
         * @param [in] injectedEntityProxy - Proxy object containing entity and meta information 
         *                                   about the updated entity that is about to be injected.
         */
        virtual void OnInjectedUpdatedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy) {}

        /**
         * Called when an entity delete is about to be injected in the system.
         *
         * @param [in] injectedEntityProxy - Proxy object containing information about the entity about to
         *                                   be deleted.
         */
        virtual void OnInjectedDeletedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy) {}

        /**
         * Indicates that all initial injection data has been transfered to the handler.
         *
         * In connection to the completion of an entity registration, the Dob will transfer
         * initial injection data to the handler. This method indicates that all such
         * data has been transfered, and thus, enables an effective handling of
         * the initial injection data by the handler.
         *
         * This method is guaranteed to be called once for each invocation of the following methods:
         * (This is true even when there is no initial injection data at all)
         * @li ConnectionBase#RegisterEntityHandlerInjection
         * @li ConnectionBase#RegisterEntityHandlerPending
         *
         * @param [in] typeId - Type id in corresponding registration.
         * @param [in] handlerId - Handler id in corresponding registration.
         */
         virtual void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId,
                                              const Dob::Typesystem::HandlerId& handlerId)
         { //This is an empty implementation, but we have this little bit to avoid MSVC++ warnings
#ifdef _MSC_VER
             typeId;handlerId;
#endif
         }


    private:
        virtual EntityInjectionBase * ToEntityInjectionBase() {return this;}
    };

    /**
     * Interface to receive service requests
     *
     * @headerfile ConsumerBase.h <Safir/Dob/Consumer.h>
     */
    class DOSE_CPP_API ServiceRequestBase:
        public virtual Internal::ConsumerBase
    {
    public:
        /** Virtual destructor is needed since we have virtual member functions. */
        virtual ~ServiceRequestBase() {}

        /**
         * Called when a service request is received.
         *
         * The receiver of the callback must send a response using the responseSender.
         * It is allowed to store the responseSender and send the response later after this
         * method has returned. The responseSender is a smart-pointer which means that it
         * will handle deletion of the underlaying object on its own.
         *
         * @param [in] serviceRequestProxy - Proxy object containing request object and meta information.
         * @param [in] responseSender - Used to send the response for the received request.
         */
        virtual void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                      Safir::Dob::ResponseSenderPtr         responseSender ) = 0;

    private:
        virtual ServiceRequestBase * ToServiceRequestBase() {return this;}
    };

}
}


#endif

