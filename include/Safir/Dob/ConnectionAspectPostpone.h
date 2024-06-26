/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

#ifndef _SAFIR_DOB_CONNECTION_ASPECT_POSTPONE_H
#define _SAFIR_DOB_CONNECTION_ASPECT_POSTPONE_H

#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/ConnectionAspectBase.h>
#include <Safir/Dob/ConnectionBase.h>
#include <Safir/Dob/Typesystem/InstanceId.h>

#include <string>

namespace Safir
{
namespace Dob
{
    /**
     * Class that provides a methods to postpone the reception of data from the Dob.
     *
     * Sometimes it can by handy for an application to postpone the reception of data
     * received in callbacks from the Dob.
     *
     * For instance, setting up a subscription can potentially give a lot of initial OnNewEntity callbacks.
     * If the application, for each subscription response, sends a request, the out-queue will probably fill up
     * giving an Overflow exception. In this situation the application can postpone further callbacks.
     *
     */
    class DOSE_CPP_API ConnectionAspectPostpone : public ConnectionAspectBase
    {
    public:
        /**
         * Constructor
         *
         * @param connection The connection that you want to operate through.
         */
        ConnectionAspectPostpone(const ConnectionBase& connection) : ConnectionAspectBase(connection) {}

        /**
         * Postpone dispatching of current callback method for the dispatched type and its subclasses.
         *
         * Used by a consumer to postpone dispatching of current callback method for the dispatched type and its subclasses.
         *
         * This method can be called from within the following callbacks:
         * @li EntitySubscriber#OnNewEntity
         * @li EntitySubscriber#OnUpdatedEntity
         * @li EntitySubscriber#OnDeletedEntity
         * @li EntityInjectionBase#OnInjectedNewEntity
         * @li EntityInjectionBase#OnInjectedUpdatedEntity
         * @li EntityInjectionBase#OnInjectedDeletedEntity
         * @li EntityRequestBase#OnCreateRequest
         * @li EntityRequestBase#OnUpdateRequest
         * @li EntityRequestBase#OnDeleteRequest
         * @li ServiceRequestBase#OnServiceRequest
         * @li MessageSubscriber#OnMessage
         *
         * but NOT from the following callbacks:
         * @li RevokedRegistrationBase#OnRevokedRegistration
         * @li CompletedRegistrationBase#OnCompletedRegistration
         * @li EntityInjectionBase#OnInitialInjectionsDone
         * @li StopHandler#OnStopOrder
         * @li Dispatcher#OnDoDispatch
         * @li Requestor#OnResponse
         * @li Requestor#OnNotRequestOverflow
         * @li MessageSender#OnNotMessageOverflow
         * @li RegistrationSubscriber#OnRegistered
         * @li RegistrationSubscriber#OnUnregistered
         *
         * The dispatching is automatically resumed when an OnNotRequestOverflow or OnNotMessageOverflow is
         * dispatched to the application which means that the application doesn't need to invoke ResumePostponed()
         * by itself when the original postpone has been made because of an overflow situation.
         *
         * Note that the postpone only applies to the currently dispatching consumer, other consumers will not
         * be affected by the postpone.
         *
         * If you are postponing a request (OnCreateRequest, OnUpdateRequest, OnDeleteRequest or OnServiceRequest),
         * special care must be taken to the handling of the ResponseSender object. If redispatchCurrent is set to
         * True the current ResponseSender must be discarded (you will get a new ResponseSender). If redispatchCurrent
         * is set to False a response for the current request must be sent.
         *
         * @param [in] redispatchCurrent True indicates that the currently dispatched object shall be dispatched again
         *                               once the dispatching is resumed.
         */
        void Postpone(const bool redispatchCurrent) const;

        /**
         * Resume dispatching of postponed objects.
         *
         * Allows the application to explicitly resume dispatching of postponed objects.
         *
         * Needs to be inoked only if the original postpone is not related to an overflow towards the Dob.
         * @see Postpone
         */
        void ResumePostponed() const;

        /**
         * Discard the currently dispatched injected entity instance and wait for an update.
         *
         * A create or update of an object from an external source can have produced an inconsistent entity state
         * (from the perspective of the application, not the Dob) and this method gives the local owner the
         * possibility to wait for the entity instance to be completly updated.
         *
         * This method can be called from within the following callbacks:
         * @li EntityInjectionBase#OnInjectedNewEntity
         * @li EntityInjectionBase#OnInjectedUpdatedEntity
         *
         * The dispatching of the injected entity instance is resumed when it is updated by the external source.
         */
        void IncompleteInjectionState() const;

    };

}
}

#endif
