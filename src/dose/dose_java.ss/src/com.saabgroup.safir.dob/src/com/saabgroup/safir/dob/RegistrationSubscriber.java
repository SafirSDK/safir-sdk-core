// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safirsdkcore.com)
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
package com.saabgroup.safir.dob;


/**
 * Interface to be implemented by subscribers of handler registrations.
 */
public interface RegistrationSubscriber extends com.saabgroup.safir.dob.internal.ConsumerBase
{
    /**
     * Called when a handler for an entity or service has been registered.
     *
     * @param typeId Type id of the registered entity or service.
     * @param handlerId HandlerId of the registered handler.
     */
    void onRegistered(long typeId,
                      com.saabgroup.safir.dob.typesystem.HandlerId handlerId);

    /**
     * Called when a handler for an entity or service has been unregistered.
     *
     * @param typeId Type id of the unregistered entity or service.
     * @param handlerId HandlerId of the unregistered handler.
     */
    void onUnregistered(long typeId,
                        com.saabgroup.safir.dob.typesystem.HandlerId handlerId);
}

