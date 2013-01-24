/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
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
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
package com.saabgroup.safir.samples.vehicleappjava;

import com.saabgroup.safir.dob.Connection;
import com.saabgroup.safir.dob.StopHandler;

public class VehicleAppJava implements StopHandler {

    /**
     * The main loop.
     */
    private IMainLoop mainLoop;
    
    /**
     * Primary DOB connection.
     */
    private Connection dobConnection;
    
    /**
     * The DOB dispatcher
     */
    private DobDispatcher dobDispatcher;
    
    /**
     *  Object handlers.
     */
    VehicleEntityHandler entityHandler;
    VehicleServiceHandler serviceHandler;
    
    public VehicleAppJava() {

        dobConnection = new Connection();
        mainLoop = new MainLoop();
        dobDispatcher = new DobDispatcher(dobConnection, mainLoop); 
        
        entityHandler = new VehicleEntityHandler();
        serviceHandler = new VehicleServiceHandler();
    }
    
    /**
     * Start this application.
     */
    public void startup()
    {
        // Open DOB connection
        dobDispatcher.open("VehicleAppJava", "0", 0, this);
        
        entityHandler.init();
        serviceHandler.init();
        VehicleMessageSender.getInstance().init();
        
        // Start the one and only thread.
        mainLoop.start();
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        VehicleAppJava app = new VehicleAppJava();
        app.startup();
    }

    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.dob.StopHandler#onStopOrder()
     */
    @Override
    public void onStopOrder() {
        mainLoop.stop();
    }
}
