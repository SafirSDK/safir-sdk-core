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
 * Interface to receive entity requests.
*/
public interface EntityRequestBase extends com.saabgroup.safir.dob.internal.ConsumerBase {
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
     * @param entityRequestProxy Proxy object containing request and meta information.
     * @param responseSender Used to send the response for the received request.
     */
    void onCreateRequest(com.saabgroup.safir.dob.EntityRequestProxy entityRequestProxy,
                         com.saabgroup.safir.dob.ResponseSender responseSender);

    /**
     * Called when someone requests an entity to be updated.
     *
     * The receiver of the callback must send a response using the responseSender.
     * It is possible to store the responseSender and send the response later after this
     * method has returned. The responseSender is a smart-pointer which means that it
     * will handle deletion of the underlaying object on its own.
     *
     * @param entityRequestProxy Proxy object containing request and meta information.
     * @param responseSender Used to send the response for the received request.
     */
    void onUpdateRequest(com.saabgroup.safir.dob.EntityRequestProxy entityRequestProxy,
                         com.saabgroup.safir.dob.ResponseSender responseSender);

    /**
     * Called when someone requests an entity to be deleted.
     *
     * The receiver of the callback must send a response using the responseSender.
     * It is possible to store the responseSender and send the response later after this
     * method has returned. The responseSender is a smart-pointer which means that it
     * will handle deletion of the underlaying object on its own.
     *
     * @param entityRequestProxy Proxy object containing request information.
     * @param responseSender Used to send the response for the received request.
     */
    void onDeleteRequest(com.saabgroup.safir.dob.EntityRequestProxy entityRequestProxy,
                         com.saabgroup.safir.dob.ResponseSender responseSender);
}
