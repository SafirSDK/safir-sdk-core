// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
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
 * Interface to be implemented by subscribers of entities.
 */
public interface EntitySubscriber extends com.saabgroup.safir.dob.internal.ConsumerBase
{
    /**
     * Called when a new entity is available.
     *
     * @param entityProxy Proxy object containing new entity and meta information.
     */
    void onNewEntity(com.saabgroup.safir.dob.EntityProxy entityProxy);

    /**
     * Called when an entity is updated.
     *
     * If Change Information is enabled for the subscription those entity members
     * that are changed, compared to the previous received entity, will be marked as
     * changed.
     *
     * The entity owner handler id can be retreived by calling GetCallbackInfo.
     *
     * @param entityProxy Proxy object containing updated entity and meta information.
     */
    void onUpdatedEntity(com.saabgroup.safir.dob.EntityProxy entityProxy);

    /**
     * Called when an entity is deleted.
     *
     * @param entityProxy Proxy object containing deleted entity information.
     * @param deprecated This flag is deprecated
     */
    void onDeletedEntity(com.saabgroup.safir.dob.EntityProxy entityProxy, boolean deprecated);
}

