/******************************************************************************
*
* Copyright Saab AB, 2008-2009 (http://www.safirsdk.com)
*
* Created by: Petter Lönnstedt / stpeln
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

using System;

namespace VehicleAppCs
{
    public class MainLoop : IDisposable
    {
        private event MethodDelegate m_StopEvent;

        private static System.Threading.AutoResetEvent m_syncEvent;
        private System.Collections.Queue m_notSyncdmethodQueue = new System.Collections.Queue();
        private System.Collections.Queue m_methodQueue;

        private bool m_running = false; // True while main loop is running, otherwise false    
        private bool m_disposed = false; // Track whether Dispose has been called.

        /// <summary>
        /// Constructor.
        /// </summary>
        public MainLoop()
        {
            // Create the wait event
            m_syncEvent = new System.Threading.AutoResetEvent( false );
            // Create a synchronized thread safe queue
            m_methodQueue = System.Collections.Queue.Synchronized( m_notSyncdmethodQueue );
        }

        /// <summary>
        /// Destructor.
        /// </summary>
        ~MainLoop()
        {
            Dispose();
        }

        /// <summary>
        /// Starts the main loop.
        /// </summary>
        public void Run()
        {
            // Start the main loop and continue until it's stopped
            m_running = true;
            while (m_running)
            {
                // Wait on Invoke or Stop methods to be called
                m_syncEvent.WaitOne();

                while (m_methodQueue.Count != 0)
                {
                    MethodDelegate method = (MethodDelegate)m_methodQueue.Dequeue();

                    try
                    {
                        // Perform the action
                        method();
                    }
                    catch (Exception e)
                    {
                        String msg = e.Message + " " + e.InnerException + " " + e.StackTrace;

                        // Send event
                        Safir.Logging.SendSystemLog(Safir.Logging.Severity.Critical,
                                                    "Unhandled Exception in MainLoop: " + msg);

                        // Terminate the application
                        //Instances.SelfInstance.MainModule.FatalError();
                    }
                }

                m_syncEvent.Reset();
            }
        }

        /// <summary>
        /// True if the main loop is started
        /// </summary>
        public bool IsStarted() { return m_running; }

        /// <summary>
        /// Stops the main loop.
        /// </summary>
        public void Stop()
        {
            m_running = false;
            m_methodQueue.Clear();
            m_syncEvent.Set();

            // Send a stop message to subscriber (Invoke the call back)
            if (m_StopEvent != null)
            {
                m_StopEvent();
            }
        }

        /// <summary>
        /// Register a callback to be called when the main loop terminates.
        /// </summary>
        /*public void OnStop( MethodDelegate method )
        {
            if (method != null)
            {
                m_StopEvent += method;
            }
        }*/

        /// <summary>
        /// Invokes a method in the main loop
        /// </summary>
        /// <param name='method'>
        /// The method to be executed in the main loop 
        /// </param>
        /// <remarks>
        /// The call is asynchronous, i.e. the request will be queued and the call  
        /// returns immediately to the calling thread.
        /// </remarks>
        public void Invoke( MethodDelegate method )
        {
            if (method != null)
            {
                m_methodQueue.Enqueue( method );
                m_syncEvent.Set();
            }
        }

        /// <summary>
        /// Release resources allocated by the main loop.
        /// </summary>
        public void Dispose()
        {
            if (!m_disposed)
            {
                m_methodQueue.Clear();
            }
            m_disposed = true;
        }
    }
}
