/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / stjyo
*
******************************************************************************/
package com.saabgroup.safir.samples.vehicleappjava;

import java.util.TimerTask;

import com.saabgroup.safir.dob.Connection;
import com.saabgroup.safir.dob.StopHandler;

/**
 * Class that handles the dispatching of a DOB-connection. It also handles the necessary thread switch.
 * @author stjyo
 *
 */
public class DobDispatcher implements com.saabgroup.safir.dob.Dispatcher {
    
    private Connection dobConnection;
    private IMainLoop mainLoop;
    private DoDispatch dispatcher = new DoDispatch();
    private long delay = 0;
    private volatile long lastDispatchTime = 0;
    
    /**
     * Constructor
     * 
     * @param dobConnection The Dob connection
     * @param mainLoop The main loop
     */
    public DobDispatcher(Connection dobConnection, IMainLoop mainLoop) {
        this.dobConnection = dobConnection;
        this.mainLoop = mainLoop;
    }
    
    /**
     * Get Dob connection
     * 
     * @return The Dob connection
     */
    public Connection getDobConnection() {
        return dobConnection;
    }
    
    /**
     * Get main loop
     * 
     * @return The main loop
     */
    public IMainLoop getMainLoop() {
        return mainLoop;
    }
    
    /**
     * Get minimum time between each dispatch call
     * 
     * @return the time between dispatch calls in milliseconds
     */
    public long getDelay() {
        return delay;
    }

    /**
     * Set a minimum time between each dispatch call
     * 
     * @param delay The minimum time between each dispatch call in milliseconds
     */
    public void setDelay(long delay) {
        this.delay = delay;
    }
    
    /**
     * Open a connection to the DOB.
     * <p>
     * There can be a number of contexts in the DOB. A connection is linked to the context specified in Open.
     * All operations using a connection is affecting only the context linked to that connection.
     * The intended primary usage is for recording/replay functionality. 0 is defined as the default
     * context.
     * </p>
     * 
     * @note connectionNameCommonPart together with connectionNameInstancePart must be unique in the node.
     * 
     * @throws com.saabgroup.safir.dob.NotOpenException The connection name is already used by someone else. Try another!
     * 
     * @see Connection#open
     * 
     * @param connectionNameCommonPart Name that identifies the program but not any particular program instance.
     * @param connectionNameInstancePart Name that identifies a particular program instance.
     * @param context Context
     * @param stopHandler Object that implements the StopHandler interface.
     */
    public void open(String connectionNameCommonPart,
            String connectionNameInstancePart,
            int context,
            StopHandler stopHandler) {
        dobConnection.open(connectionNameCommonPart, connectionNameInstancePart, context, stopHandler, this);
    }
    
    /**
     * Not to be called. Used by the framework.
     */
    @Override
    public void onDoDispatch() {
        if(delay != 0) {
            long currentTime = System.currentTimeMillis();
            long period = (currentTime - lastDispatchTime);
            if(period >= delay) {
                lastDispatchTime = currentTime;
                mainLoop.invokeLater(dispatcher);
            }
            else {
                // Start timer
                java.util.Timer timer = new java.util.Timer();
                timer.schedule(new TimerTask() {
                    @Override
                    public void run() {
                        lastDispatchTime = System.currentTimeMillis();
                        mainLoop.invokeLater(dispatcher);
                    }}, delay - period);
            }
        }
        else {
            mainLoop.invokeLater(dispatcher);
        }
    }

    private class DoDispatch implements IMainLoop.Callback {
        /*
         * (non-Javadoc)
         * @see com.saabgroup.safir.application.IMainLoop.Callback#onInvoke()
         */
        @Override
        public void onInvoke() {
            dobConnection.dispatch();
        }
    }
}
