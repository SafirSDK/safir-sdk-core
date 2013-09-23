/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
* 
* Created by: Lars Hagström / stlrha
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

namespace Safir.Dob
{
    namespace Internal
    {
        /// <summary>
        /// Internal base class used when composing more elaborated interfaces.
        /// </summary>
        public interface ConsumerBase
        {

        }
    }

    /// <summary>
    /// Base class used when composing more elaborated interfaces.
    /// </summary>
    public interface RevokedRegistrationBase : Internal.ConsumerBase
    {
        /// <summary>
        /// Indicates that the handler is no longer registered for the given type.
        /// </summary>
        /// <param name="typeId">Type Id of the entity or service.</param>
        /// <param name="handlerId">Id of the revoked handler.</param>
        void OnRevokedRegistration(System.Int64 typeId,
                                   Safir.Dob.Typesystem.HandlerId handlerId);
    }

    /// <summary>
    /// Base class used when composing more elaborated interfaces.
    /// </summary>
    public interface CompletedRegistrationBase : RevokedRegistrationBase
    {
        /// <summary>
        /// Indicates that a pending registration has completed and the handler is now registered.
        /// </summary>
        /// <param name="typeId">Type Id of the entity or service.</param>
        /// <param name="handlerId">Id of the registered handler.</param>
        void OnCompletedRegistration(System.Int64 typeId,
                                     Safir.Dob.Typesystem.HandlerId handlerId);
    }

    /// <summary>
    /// Interface to receive entity requests.
    /// </summary>
    public interface EntityRequestBase : Internal.ConsumerBase
    {
        /// <summary>
        /// Called when someone requests an entity to be created.
        /// <para/>
        /// If the handler is registered as "HandlerDecidesInstanceId" the request does not
        ///   contain an instance id, and the handler must decide which instance to use and
        ///   must send this back to the requestor using an EntityIdResponse.
        /// <para/>
        /// If the handler is registered as "RequestorDecidesInstanceId" the request contains
        ///   an instance id, which the handler *must* use if is going to accept the request.
        ///   If the instance cannot be used an error response must be sent.
        ///   The handler must not send EntityIdResponse on successful create requests when
        ///   it is registered as "RequestorDecidesInstanceId".
        /// <para/>
        /// The receiver of the callback must send a response using the responseSender.
        /// It is possible to store the responseSender and send the response later after this
        /// method has returned. The responseSender is a smart-pointer which means that it
        /// will handle deletion of the underlaying object on its own.
        /// </summary>
        /// <param name="entityRequestProxy">Proxy object containing request and meta information.</param>
        /// <param name="responseSender">Used to send the response for the received request.</param>
        void OnCreateRequest(Safir.Dob.EntityRequestProxy entityRequestProxy,
                             Safir.Dob.ResponseSender responseSender);

        /// <summary>
        /// Called when someone requests an entity to be updated.
        /// <para/>
        /// The receiver of the callback must send a response using the responseSender.
        /// It is possible to store the responseSender and send the response later after this
        /// method has returned. The responseSender is a smart-pointer which means that it
        /// will handle deletion of the underlaying object on its own.
        /// </summary>
        /// <param name="entityRequestProxy">Proxy object containing request and meta information.</param>
        /// <param name="responseSender">Used to send the response for the received request.</param>
        void OnUpdateRequest(Safir.Dob.EntityRequestProxy entityRequestProxy,
                             Safir.Dob.ResponseSender responseSender);

        /// <summary>
        /// Called when someone requests an entity to be updated.
        /// <para/>
        /// The receiver of the callback must send a response using the responseSender.
        /// It is possible to store the responseSender and send the response later after this
        /// method has returned. The responseSender is a smart-pointer which means that it
        /// will handle deletion of the underlaying object on its own.
        /// </summary>
        /// <param name="entityRequestProxy">Proxy object containing request information.</param>
        /// <param name="responseSender">Used to send the response for the received request.</param>
        void OnDeleteRequest(Safir.Dob.EntityRequestProxy entityRequestProxy,
                             Safir.Dob.ResponseSender responseSender);
    }

    /// <summary>
    /// This Consumer Base class contains callback methods that can be overridden by an entity 
    /// handler that registers a handler for an entity type that can potentially be injected 
    /// outside the control of the handler itself.
    /// <para/>
    /// Examples of when an entity is injected outside the control of a handler includes
    /// persistent data at startup time and entities received from other system installations
    /// (multi-owned entities).
    /// <para/>
    /// The handler can reject the injection by invoking Delete for the received entity.
    /// <para/>
    /// In case the handler has no need to be informed when an entity is about to be injected
    /// it can rely on the default implementation which just accepts the injected entity.
    /// <para/>
    /// An example of a situation where a handler needs to act on injected data is when the entity
    /// has members that are references (EntityIds) to other entity instances, and the lifetime
    /// of the pointed-to instance is completly connected to the lifetime of the referer.
    /// If an entity has been injected it is the responsibility of the local handler to
    /// traverse the entity instance graph, find changed references, and delete the
    /// unreferenced entity instances. To support this task there are methods in the InjectedEntityProxy
    /// class to read the previous entity state and find invalid refrences.
    /// <para/>
    /// Another example is when an injection relates only to some of the members (might be the case
    /// when updates are received from external systems) and the handler wants to maintain some sort
    /// of consistency between different members. In this case the handler can call
    /// ConnectionAspectPostpone.IncompleteInjectionState to wait for more updates before the new state
    /// is set.
    /// </summary>
    public interface EntityInjectionBase : EntityRequestBase
    {
        /// <summary>
        /// Called when a new entity is about to be injected in the system.
        /// </summary>
        /// <param name="injectedEntityProxy">Proxy object containing entity and meta information 
        ///                                   about the entity that is about to be injected.</param>
        void OnInjectedNewEntity(Safir.Dob.InjectedEntityProxy injectedEntityProxy);

        /// <summary>
        /// Called when an updated entity is about to be injected in the system.
        /// </summary>
        /// <param name="injectedEntityProxy">Proxy object containing entity and meta information 
        ///                                   about the updated entity that is about to be injected.</param>
        void OnInjectedUpdatedEntity(Safir.Dob.InjectedEntityProxy injectedEntityProxy);

        /// <summary>
        /// Called when an entity delete is about to be injected in the system.
        /// </summary>
        /// <param name="injectedEntityProxy">Proxy object containing information about the entity about to
        ///                                   be deleted.</param>
        void OnInjectedDeletedEntity(Safir.Dob.InjectedEntityProxy injectedEntityProxy);

        /// <summary>
        /// Indicates that all initial injection data has been transfered to the handler.
        /// <para/>
        /// In connection to the completion of an entity registration, the Dob will transfer
        /// initial injection data to the handler. This method indicates that all such
        /// data has been transfered, and thus, enables an effective handling of
        /// the initial injection data by the handler.
        /// <para/>
        /// This method is guaranteed to be called once for each invocation of the following methods:
        /// (This is true even when there is no initial injection data at all) <para/>
        /// <list type="bullet">
        /// <item><description>ConnectionBase.RegisterEntityHandlerInjection</description></item>
        /// <item><description>ConnectionBase.RegisterEntityHandlerPending</description></item>
        /// </list>
        /// </summary>
        /// <param name="typeId">Type id in corresponding registration.</param>
        /// <param name="handlerId">Handler id in corresponding registration.</param>
        void OnInitialInjectionsDone(System.Int64 typeId,
                                     Safir.Dob.Typesystem.HandlerId handlerId);
    }

    /// <summary>
    /// Interface to receive service requests.
    /// </summary>
    public interface ServiceRequestBase : Internal.ConsumerBase
    {
        /// <summary>
        /// Called when a service request is received.
        /// <para/>
        /// The receiver of the callback must send a response using the responseSender.
        /// It is allowed to store the responseSender and send the response later after this
        /// method has returned. The responseSender is a smart-pointer which means that it
        /// will handle deletion of the underlaying object on its own.
        /// </summary>
        /// <param name="serviceRequestProxy">Proxy object containing request object and meta information.</param>
        /// <param name="responseSender">Used to send the response for the received request.</param>
        void OnServiceRequest(Safir.Dob.ServiceRequestProxy serviceRequestProxy,
                              Safir.Dob.ResponseSender responseSender);
    }
}
