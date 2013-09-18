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

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import com.saabgroup.safir.Logging;


public class MainLoop implements IMainLoop {

    private volatile boolean running = false;
    private volatile boolean disposed = false; // Track whether Dispose has been called.
    
    private BlockingQueue<IMainLoop.Callback> methods = new LinkedBlockingQueue<IMainLoop.Callback>();
    
    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.application.MainLoop#start()
     */
    public void start() {
        start(null);
    }
    
    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.application.MainLoop#start(com.saabgroup.safir.application.MainLoop.Callback)
     */
    public void start(IMainLoop.Callback initMethod) {
        
        // run init method
        if(initMethod != null) {
            initMethod.onInvoke();
        }
        
        // Start the main loop and continue until it's stopped
        running = true;
        while(running) {
            try {
                IMainLoop.Callback method = methods.take();
                method.onInvoke();
            } catch (InterruptedException e) {
                com.saabgroup.safir.Logging.sendSystemLog
                    (com.saabgroup.safir.Logging.Severity.ERROR,
                     "Error in MainLoop when taking method from the queue");

            } catch (Exception e) {
                com.saabgroup.safir.Logging.sendSystemLog
                    (com.saabgroup.safir.Logging.Severity.CRITICAL,
                     "Unhandled exception in MainLoop when invoking method. " + e.getMessage());
            }
        }
    }
    
    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.application.MainLoop#isStarted()
     */
    public boolean isStarted() {
        return running;
    }
    
    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.application.MainLoop#stop()
     */
    public void stop() {
        running = false;
    }
    
    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.application.IMainLoop#invokeLater(com.saabgroup.safir.application.IMainLoop.Callback)
     */
    @Override
    public void invokeLater(IMainLoop.Callback callback) {
        try {
            methods.put(callback);
        } catch (InterruptedException e) {
           com.saabgroup.safir.Logging.sendSystemLog
               (com.saabgroup.safir.Logging.Severity.ERROR,
                "Error in MainLoop when adding method to the queue ");
        }
    }
    
    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.application.MainLoop#dispose()
     */
    public void dispose()
    {
        if (!disposed)
        {
            methods.clear();
            disposed = true;
        }
    }

    /*
     * (non-Javadoc)
     * @see java.lang.Object#finalize()
     */
    @Override
    protected void finalize() throws Throwable {
        dispose();
        super.finalize();
    }
}
