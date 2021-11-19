// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2011-2013, 2021 (http://safirsdkcore.com)
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
    MainLoop()
    {
        m_state = new State();
        m_cleanable = cleaner.register(this,m_state);
    }

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
        m_state.running = true;
        while(m_state.running) {
            try {
                IMainLoop.Callback method = m_state.methods.take();
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
        return m_state.running;
    }

    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.application.MainLoop#stop()
     */
    public void stop() {
        m_state.running = false;
    }

    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.application.IMainLoop#invokeLater(com.saabgroup.safir.application.IMainLoop.Callback)
     */
    @Override
    public void invokeLater(IMainLoop.Callback callback) {
        try {
            m_state.methods.put(callback);
        }
        catch (InterruptedException e) {}
    }

    /*
     * (non-Javadoc)
     * @see com.saabgroup.safir.application.MainLoop#dispose()
     */
    @Override
    public void close()
    {
        m_cleanable.clean();
    }


    private static class State implements Runnable {
        public volatile boolean running = false;
        public volatile boolean disposed = false; // Track whether Dispose has been called.

        public BlockingQueue<IMainLoop.Callback> methods = new LinkedBlockingQueue<IMainLoop.Callback>();

        //-------------------
        //Clean up code
        //-------------------
        public void run() {
            if (!disposed)
            {
                methods.clear();
                disposed = true;
            }
        }
    }

    private static final java.lang.ref.Cleaner cleaner = java.lang.ref.Cleaner.create();
    private final State m_state;
    private final java.lang.ref.Cleaner.Cleanable m_cleanable;
}
