// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
 */
public class ResponseSender
{
    ResponseSender(int ctrl,
                   com.saabgroup.safir.dob.internal.ConsumerBase consumer,
                   int responseId)
    {
        m_valid=true;
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
     * @exception NotOpenException If the connection is not open.
     */
    public void send(Response response)
    {

        if (!m_valid)
        {
            throw new AccessDeniedException("ResponseSender object has already been used once.");
        }

        boolean [] success = new boolean [1];


        //TODO: serialize directly to shared memory
        int blobSize = response.calculateBlobSize();
        java.nio.ByteBuffer blob = java.nio.ByteBuffer.allocateDirect(blobSize); //allocate blob
        int beginningOfUnused = com.saabgroup.safir.dob.typesystem.InternalOperations.formatBlob(blob, blobSize, response.getTypeId());
        beginningOfUnused = response.writeToBlob(blob, beginningOfUnused);

        Interface.SendResponse(m_ctrl,
                               blob,
                               m_consumer,
                               m_responseId,
                               success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        m_valid = false;
    }

    /**
     * Check if an instance is still waiting for a response to be sent.
     *
     * @return True if a response has been sent using this instance
     *         (instance is consumed), otherwise false.
     */
    public boolean isDone() {
        return !m_valid;
    }

    /**
     * Discard this ResponseSender.
     *
     * Calling this function means that you forfeit your chance to send a response
     * to the request. It will disable the checks in the destructor (see above).
     *
     * The typical case when you must discard the ResponseSender is when calling
     * Postpone with redispatchCurrent set to True. In this case you will get
     * the request again together with a new ResponseSender.
     */
    public void discard() {
        if (!m_valid) {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException
                ("ResponseSender object has already been used once.");
        }
        m_valid = false;
    }



    protected class MessageBox
        extends java.awt.Dialog
        implements java.awt.event.ActionListener
    {
        private static final long serialVersionUID = 0;

        MessageBox(String msg)
        {
            super(new java.awt.Frame(""),"Programming Error!",true);
            setLayout(new java.awt.BorderLayout());
            java.awt.TextArea textArea = new java.awt.TextArea(msg,4,70,java.awt.TextArea.SCROLLBARS_NONE);
            textArea.setEditable(false);
            add("Center",textArea);
            java.awt.Panel panel = new java.awt.Panel();
            panel.setLayout(new java.awt.FlowLayout());
            panel.add(m_ok = new java.awt.Button(" OK "));
            add("South",panel);
            m_ok.addActionListener(this);
            java.awt.Dimension d = getToolkit().getScreenSize();
            setLocation(d.width/3,d.height/3);
            pack();
            setAlwaysOnTop(true);
            setVisible(true);
        }

        public void actionPerformed(java.awt.event.ActionEvent ae)
        {
            if(ae.getSource() == m_ok)
            {
                setVisible(false);
            }
        }

        private java.awt.Button m_ok;
    };

    protected void finalize() throws java.lang.Throwable
    {
        try
        {
            if (m_valid)
            {
                System.out.println ("Programming Error! A ResponseSender was discarded " +
                                    "without having been used! (A timeout response will be " +
                                    "automatically sent). The program will now exit!");

                new MessageBox("Programming Error! A ResponseSender was discarded "
                               + "without having been used! (A timeout response was "
                               + "automatically sent). The program will now exit!");

                System.exit(101010);
            }
        }
        finally
        {
            super.finalize();
        }
    }

    private boolean m_valid;
    private int m_ctrl;
    private com.saabgroup.safir.dob.internal.ConsumerBase m_consumer;
    private int m_responseId;

}
