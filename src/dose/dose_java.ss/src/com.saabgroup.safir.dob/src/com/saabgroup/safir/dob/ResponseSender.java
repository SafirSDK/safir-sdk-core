// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2013, 20221 (http://safirsdkcore.com)
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
 * Class used for responding to received requests.
 *
 * The purpose of this class is to allow responses to be sent either from
 * within the request callback, or at a later time (in which case you have
 * to keep the responseSender you received in the callback "for later").
 *
 * Note that you still have to send the response within the timout period,
 * or the response will not be delivered to the requestor (who will have
 * received a timeout response instead).
 *
 * Not using a ResponseSender is considered a programming error, if it not
 * used a log will be emitted when the object is being garbage collected.
 * Since the this check is called by the Garbage Collector this may happen 
 * "long after" you've dropped your reference to the response sender.
 */
public class ResponseSender
{
    ResponseSender(int ctrl,
                   com.saabgroup.safir.dob.internal.ConsumerBase consumer,
                   int responseId)
    {
        m_state = new State();
        m_cleanable = ResourceHelper.register(this,m_state);

        m_state.valid=true;
        m_ctrl = ctrl;
        m_consumer = consumer;
        m_responseId = responseId;
    }

    /**
     * Sends a response for the request that this instance was obtained with.
     *
     * This method may only be called once on any given instance! Calling it twice
     * amounts to trying to send two responses to one request, which is considered
     * a programming error.
     *
     * @param response The response to be sent.
     * @throws NotOpenException If the connection is not open.
     * @throws LowMemoryException Not enough shared memory available to complete this operation.
     */
    public void send(Response response)
    {

        if (!m_state.valid)
        {
            throw new AccessDeniedException("ResponseSender object has already been used once.");
        }

        boolean [] success = new boolean [1];

        java.nio.ByteBuffer blob = com.saabgroup.safir.dob.typesystem.BlobOperations.writeToBlob(response);

        Interface.SendResponse(m_ctrl,
                               blob,
                               m_consumer,
                               m_responseId,
                               success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().Throw();
        }

        m_state.valid = false;
    }

    /**
     * Check if an instance is still waiting for a response to be sent.
     *
     * @return True if a response has been sent using this instance
     *         (instance is consumed), otherwise false.
     */
    public boolean isDone() {
        return !m_state.valid;
    }

    /**
     * Discard this ResponseSender.
     *
     * Calling this function means that you forfeit your chance to send a response
     * to the request. It will disable the checks in the cleanup.
     *
     * The typical case when you must discard the ResponseSender is when calling
     * Postpone with redispatchCurrent set to True. In this case you will get
     * the request again together with a new ResponseSender.
     */
    public void discard() {
        if (!m_state.valid) {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException
                ("ResponseSender object has already been used once.");
        }
        m_state.valid = false;
    }


    private static class State implements Runnable {
        public boolean valid;

        /**
         * Clean up code
         *
         * Will check that the ResponseSender has been used, and if it hasn't a log will be emitted.
         * Since the cleanup is called by the Garbage Collector this may happen
         * "long after" you've dropped your reference to the response sender.
         *
         * Not using a ResponseSender is considered a programming error.
         */
        public void run() {
            if (valid) {
                com.saabgroup.safir.Logging.sendSystemLog
                    (com.saabgroup.safir.Logging.Severity.CRITICAL,
                     "Programming Error! A ResponseSender was discarded without having been used!");
            }
        }
    }

    private final int m_ctrl;
    private final com.saabgroup.safir.dob.internal.ConsumerBase m_consumer;
    private final int m_responseId;
    private final State m_state;
    private final java.lang.ref.Cleaner.Cleanable m_cleanable;

}
