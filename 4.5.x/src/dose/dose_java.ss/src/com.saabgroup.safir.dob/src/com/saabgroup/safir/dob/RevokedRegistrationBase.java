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
 * Base class used when composing more elaborated interfaces.
 */
public interface RevokedRegistrationBase extends com.saabgroup.safir.dob.internal.ConsumerBase
{
    /**
     * Indicates that the handler is no longer registered for the given type.
     *
     * @param typeId Type Id of the entity or service.
     * @param handlerId Id of the revoked handler.
     */
    void onRevokedRegistration(long typeId,
                               com.saabgroup.safir.dob.typesystem.HandlerId handlerId);
}

