/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safirsdkcore.com)
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
package com.saabgroup.safir.samples.vehicleappjava;

import com.saabgroup.safir.dob.SecondaryConnection;

/**
 * This class sends the message. Make this class a singleton.
 * The reason is that it is used from several classes in this
 * application.
 */
public class VehicleMessageSender implements com.saabgroup.safir.dob.MessageSender {

     // This class uses this secondary connection for Dob calls.
    private SecondaryConnection connection;
    
    // Private constructor prevents instantiation from other classes
    private VehicleMessageSender() { 
        connection = new SecondaryConnection();
    }

    /**
    * SingletonHolder is loaded on the first execution of Singleton.getInstance() 
    * or the first access to SingletonHolder.instance, not before.
    */
    private static class SingletonHolder { 
        public static final VehicleMessageSender instance = new VehicleMessageSender();
    }

    public static VehicleMessageSender getInstance() {
        return SingletonHolder.instance;
    }
    
    /**
     * Initiates this class. Creates a secondary Dob connection.
     */
    public void init() {
        connection.attach();
    }
    
    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.dob.MessageSender#onNotMessageOverflow()
     */
    @Override
    public void onNotMessageOverflow() {
        // Retry to send the message.
        sendMaxNofVehicleMsg();
    }

    void sendMaxNofVehicleMsg() {
        // This is just a test to see how you create messages.
        // This is not part of the design pattern.
        capabilities.vehicles.VehicleMsg msg =
            new capabilities.vehicles.VehicleMsg();
        
        msg.messageText().setVal("Number of vehicles reached defined limit.");

        try
        {
            connection.send(msg, new com.saabgroup.safir.dob.typesystem.ChannelId(), this);
        }
        catch (com.saabgroup.safir.dob.OverflowException e)
        {
            // Do nothing OnNotMessageOverflow() will be called when 
            // overflow situation solved.
        }
    }
}
