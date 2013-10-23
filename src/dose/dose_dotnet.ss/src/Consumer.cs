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

using System;

namespace Safir.Dob
{
    /// <summary>
    /// Interface for reception of a stop order
    /// </summary>
    public interface StopHandler : Internal.ConsumerBase
    {
        /// <summary>
        /// When called the application owning the connection shall stop its execution.
        /// </summary>
        void OnStopOrder();
    }

    /// <summary>
    /// Interface for reception of a dispatch order.
    /// </summary>
    public interface Dispatcher : Internal.ConsumerBase
    {
        /// <summary>
        /// Indicates that there is incoming data for the connection so the application shall
        /// call Dispatch().
        /// <para/>
        /// When this method is called the application MUST call the Dispatch() method for the connection.
        /// Note that Dispatch() is NOT to be called directly from within this method. Instead the application
        /// shall set an event or similar and then call Dispatch() from the thread that owns (has called Open)
        /// the connection.
        /// </summary>
        void OnDoDispatch();
    }

    /// <summary>
    /// Interface to be implemented by an entity handler that makes a non-pending registration
    /// and that doesn't handle injected entities.
    /// </summary>
    public interface EntityHandler :
        RevokedRegistrationBase,
        EntityRequestBase
    {

    }

    /// <summary>
    /// Interface to be implemented by an entity handler that makes a non-pending registration
    /// for a type that can potentially be injected.
    /// </summary>
    public interface EntityHandlerInjection :
        RevokedRegistrationBase,
        EntityInjectionBase
    {

    }

    /// <summary>
    /// Interface to be implemented by an entity handler that makes a pending registration.
    /// </summary>
    public interface EntityHandlerPending :
        CompletedRegistrationBase,
        EntityInjectionBase
    {

    }

    /// <summary>
    /// Interface to be implemented by a service handler that makes a non-pending registration.
    /// </summary>
    public interface ServiceHandler :
        RevokedRegistrationBase,
        ServiceRequestBase
    {

    }

    /// <summary>
    /// Interface to be implemented by a service handler that makes a pending registration.
    /// </summary>
    public interface ServiceHandlerPending :
        CompletedRegistrationBase,
        ServiceRequestBase
    {

    }

    /// <summary>
    /// Interface to be implemented by an application that sends requests (Request on entities or
    /// service requests).
    /// </summary>
    public interface Requestor : Internal.ConsumerBase
    {
        /// <summary>
        /// Called when a response is received on a sent request.
        /// </summary>
        /// <param name="responseProxy">Response proxy.</param>
        void OnResponse(Safir.Dob.ResponseProxy responseProxy);

        /// <summary>
        /// Called to indicate that it is meningful to make a retry after an overflow situation.
        /// </summary>
        void OnNotRequestOverflow();
    }

    /// <summary>
    /// Interface to be implemented by senders of messages.
    /// </summary>
    public interface MessageSender: Internal.ConsumerBase
    {
        /// <summary>
        /// Called to indicate that it is meningful to make a retry after an overflow situation.
        /// </summary>
        void OnNotMessageOverflow();
    }

    /// <summary>
    /// Interface to be implemented by subscribers of handler registrations.
    /// </summary>
    public interface RegistrationSubscriber: Internal.ConsumerBase
    {
        /// <summary>
        /// Called when a handler for an entity or service has been registered.
        /// </summary>
        /// <param name="typeId">Type id of the registered entity or service.</param>
        /// <param name="handlerId">HandlerId of the registered handler.</param>
        void OnRegistered(System.Int64 typeId,
                          Safir.Dob.Typesystem.HandlerId handlerId);

        /// <summary>
        /// Called when a handler for an entity or service has been unregistered.
        /// </summary>
        /// <param name="typeId">Type id of the unregistered entity or service.</param>
        /// <param name="handlerId">HandlerId of the unregistered handler.</param>
        void OnUnregistered(System.Int64 typeId,
                            Safir.Dob.Typesystem.HandlerId handlerId);
    }

    /// <summary>
    /// Interface to be implemented by subscribers of messages.
    /// </summary>
    public interface MessageSubscriber: Internal.ConsumerBase
    {
        /// <summary>
        /// Called when a message is received.
        /// </summary>
        /// <param name="messageProxy">Proxy object containing received message and meta information.</param>
        void OnMessage(Safir.Dob.MessageProxy messageProxy);
    }

    /// <summary>
    /// Interface to be implemented by subscribers of entities.
    /// </summary>
    public interface EntitySubscriber: Internal.ConsumerBase
    {
        /// <summary>
        /// Called when a new entity is available.
        /// </summary>
        /// <param name="entityProxy">Proxy object containing new entity and meta information.</param>
        void OnNewEntity(Safir.Dob.EntityProxy entityProxy);

        /// <summary>
        /// Called when an entity is updated.
        /// <para/>
        /// If Change Information is enabled for the subscription those entity members
        /// that are changed, compared to the previous received entity, will be marked as
        /// changed.
        /// <para/>
        /// The entity owner handler id can be retreived by calling GetCallbackInfo.
        /// </summary>
        /// <param name="entityProxy">Proxy object containing updated entity and meta information.</param>
        void OnUpdatedEntity(Safir.Dob.EntityProxy entityProxy);

        /// <summary>
        /// Called when an entity is deleted.
        /// </summary>
        /// <param name="entityProxy">Proxy object containing deleted entity information.</param>
        /// <param name="deprecated">This flag is deprecated.</param>
        void OnDeletedEntity(Safir.Dob.EntityProxy entityProxy, bool deprecated);
    }
}
