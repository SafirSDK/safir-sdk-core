// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009 (http://www.safirsdk.com)
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
 * Interface to receive service requests
 */
public interface ServiceRequestBase extends com.saabgroup.safir.dob.internal.ConsumerBase
{
    /**
     * Called when a service request is received.
     *
     * The receiver of the callback must send a response using the responseSender.
     * It is allowed to store the responseSender and send the response later after this
     * method has returned. The responseSender is a smart-pointer which means that it
     * will handle deletion of the underlaying object on its own.
     *
     * @param serviceRequestProxy Proxy object containing request object and meta information.
     * @param responseSender Used to send the response for the received request.
     */
    void onServiceRequest(com.saabgroup.safir.dob.ServiceRequestProxy serviceRequestProxy,
                          com.saabgroup.safir.dob.ResponseSender responseSender);
}


