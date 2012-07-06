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

/**
 * Interface for the main loop to which methods can be set to be executed in 
 * the main loops thread
 */
public interface IMainLoop {

    /**
     * Callback interface for the MainLoop
     */
    public interface Callback {
        /**
         * Execute method in the MainLoop Thread
         */
        public void onInvoke();
    }
    
    /**
     * Starts the main loop
     */
    public void start();
    
    /**
     * Starts the main loop with callback method
     * 
     * @param callback The callback interface
     */
    public void start(Callback initMethod);
    
    /**
     * Check if the main loop is started
     * 
     * @return true if the main loop is started otherwise false
     */
    public boolean isStarted();
    
    /**
     * Stops the main loop.
     */
    public void stop();
    
    /**
     * Add method to be executed in the MainLoop Thread
     * 
     * @param callback The method to be executed
     */
    public void invokeLater(Callback callback);
    
    /**
     * Release resources allocated by the main loop.
     */
    public void dispose();
    
}
