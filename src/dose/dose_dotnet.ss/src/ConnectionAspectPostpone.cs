/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
    /// Class that provides a methods to postpone the reception of data from the Dob.
    /// <para/>
    /// Sometimes it can by handy for an application to postpone the reception of data
    /// received in callbacks from the Dob.
    /// <para/>
    /// For instance, setting up a subscription can potentially give a lot of initial OnNewEntity callbacks.
    /// If the application, for each subscription response, sends a request, the out-queue will probably fill up
    /// giving an Overflow exception. In this situation the application can postpone further callbacks.
    /// </summary>
    public sealed class ConnectionAspectPostpone : ConnectionAspectBase
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="connection">The connection that you want to operate through</param>
        public ConnectionAspectPostpone(ConnectionBase connection) : base(connection) { }

        /// <summary>
        /// Postpone dispatching of current callback method for the dispatched type and its subclasses.
        /// <para/>
        /// Used by a consumer to postpone dispatching of current callback method for the dispatched type and its subclasses.
        /// <para/>
        /// This method can be called from within the following callbacks:
        /// <para/>
        /// <list type="bullet">
        /// <item><description>EntitySubscriber.OnNewEntity</description></item>
        /// <item><description>EntitySubscriber.OnUpdatedEntity</description></item>
        /// <item><description>EntitySubscriber.OnDeletedEntity</description></item>
        /// <item><description>EntityInjectionBase.OnInjectedNewEntity</description></item>
        /// <item><description>EntityInjectionBase.OnInjectedUpdatedEntity</description></item>
        /// <item><description>EntityInjectionBase.OnInjectedDeletedEntity</description></item>
        /// <item><description>EntityRequestBase.OnCreateRequest</description></item>
        /// <item><description>EntityRequestBase.OnUpdateRequest</description></item>
        /// <item><description>EntityRequestBase.OnDeleteRequest</description></item>
        /// <item><description>ServiceRequestBase.OnServiceRequest</description></item>
        /// <item><description>MessageSubscriber.OnMessage</description></item>
        /// </list>
        /// <para/>
        /// but NOT from the following callbacks:
        /// <para/>
        /// <list type="bullet">
        /// <item><description>RevokedRegistrationBase.OnRevokedRegistration</description></item>
        /// <item><description>CompletedRegistrationBase.OnCompletedRegistration</description></item>
        /// <item><description>EntityInjectionBase.OnInitialInjectionsDone</description></item>
        /// <item><description>StopHandler.OnStopOrder</description></item>
        /// <item><description>Dispatcher.OnDoDispatch</description></item>
        /// <item><description>Requestor.OnResponse</description></item>
        /// <item><description>Requestor.OnNotRequestOverflow</description></item>
        /// <item><description>MessageSender.OnNotMessageOverflow</description></item>
        /// <item><description>RegistrationSubscriber.OnRegistered</description></item>
        /// <item><description>RegistrationSubscriber.OnUnregistered</description></item>
        /// </list>
        /// <para/>
        /// The dispatching is automatically resumed when an OnNotRequestOverflow or OnNotMessageOverflow is
        /// dispatched to the application which means that the application doesn't need to invoke ResumePostponed()
        /// by itself when the original postpone has been made because of an overflow situation.
        /// <para/>
        /// Note that the postpone only applies to the currently dispatching consumer, other consumers will not
        /// be affected by the postpone.
        /// <para/>
        /// If you are postponing a request (OnCreateRequest, OnUpdateRequest, OnDeleteRequest or OnServiceRequest),
        /// special care must be taken to the handling of the ResponseSender object. If redispatchCurrent is to True
        /// the current ResponseSender must be discarded (you will get a new ResponseSender). If redispatchCurrent
        /// is set to False a response for the current request must be sent.
        /// </summary>
        /// <param name="redispatchCurrent">True indicates that the currently dispatched object shall be dispatched again
        ///                                 once the dispatching is resumed.</param>
        public void Postpone(bool redispatchCurrent)
        {
            byte success;
            Interface.DoseC_Postpone(ControllerId, Interface.ByteOf(redispatchCurrent), out success);
            if (!Interface.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Resume dispatching of postponed objects.
        /// <para/>
        /// Allows the application to explicitly resume dispatching of postponed objects.
        /// <para/>
        /// Needs to be inoked only if the original postpone is not related to an overflow towards the Dob. <para/>
        /// See also <seealso>Postpone</seealso>
        /// </summary>
        public void ResumePostponed()
        {
            byte success;
            Interface.DoseC_ResumePostponed(ControllerId, out success);
            if (!Interface.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Discard the currently dispatched injected entity instance and wait for an update.
        /// <para/>
        /// A create or update of an object from an external source can have produced an inconsistent entity state
        /// (from the perspective of the application, not the Dob) and this method gives the local owner the
        /// possibility to wait for the entity instance to be completly updated.
        /// <para/>
        /// This method can be called from within the following callbacks:
        /// <para/>
        /// <list type="bullet">
        /// <item><description>EntityInjectionBase.OnInjectedNewEntity</description></item>
        /// <item><description>EntityInjectionBase.OnInjectedUpdatedEntity</description></item>
        /// </list>
        /// </summary>
        public void IncompleteInjectionState()
        {
            byte success;
            Interface.DoseC_IncompleteInjectionState(ControllerId, out success);
            if (!Interface.BoolOf(success))
            {
                Safir.Dob.Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

    }
}
