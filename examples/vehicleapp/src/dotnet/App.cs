/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
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
    public delegate void MethodDelegate();

    /// <summary>
    /// This application is an example of how to use DOB producer mechanisms. 
    /// The code is written to show the possibilities of the interfaces 
    /// rather than showing a real world example. The application can be run together
    /// with VehicleMmi (.Net) or VehicleMmiQt application that implements DOB 
    /// consumer mechanisms.
    /// 
    /// The application will show how to :
    /// 1) Register as handler of entity 'Vehicle' and create, update and delete vehicles 
    ///    in DOB (shared memory) on create, update and delete requests from users.
    /// 2) Handle service request 'CalculateSpeedDifference' and calculate speed difference 
    ///    and send service response 'CalculateSpeedDifferenceResponse' to service requestors.
    /// 3) Use property 'SpeedObjectProperty' to get a property value from an object.
    /// 4) Send message 'VehicleMsg'.
    /// </summary>

    /// <summary>
    /// Main class, controls startup, closedown, DOB connection and main window.
    /// </summary>
    public class App :
        // Allows this class to receive a stop order.
        Safir.Dob.StopHandler,
        // Allows this class to receive a dispatch order from DOB.
        Safir.Dob.Dispatcher 
    {
        /// <summary>
        /// The main loop.
        /// </summary>
        private MainLoop mainLoop;

        /// <summary>
        /// Primary DOB connection.
        /// </summary>
        private Safir.Dob.Connection m_dobConnection;

        /// <summary>
        /// Object handlers.
        /// </summary>
        EntityHandler entityHandler;
        ServiceHandler serviceHandler;

        /// <summary>
        /// DOB dispatch method.
        /// </summary>
        private MethodDelegate m_callDispatch;

        /// <summary>
        /// Open DOB connection and initiates dispatch
        /// </summary>
        public App()
        {
            m_dobConnection = new Safir.Dob.Connection();
            m_callDispatch = new MethodDelegate(m_dobConnection.Dispatch);

            // Create the main loop.
            mainLoop = new MainLoop();

            entityHandler = new EntityHandler();
            serviceHandler = new ServiceHandler();
        }

        /// <summary>
        /// Destructor.
        /// </summary>
        ~App()
        {
            mainLoop.Dispose();
        }

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        static void Main(string[] args)
        {
            try
            {
                App app = new App();
                app.Startup();
            }
            catch (Exception e)
            {
                String msg = e.Message + " " + e.InnerException + " " + e.StackTrace;
                Safir.Logging.SendSystemLog(Safir.Logging.Severity.Critical,
                                            "Unhandled Exception in Main: " + msg);
            }
        }

        /// <summary>
        /// Start this application.
        /// </summary>
        public void Startup()
        {
            Safir.Application.CrashReporter.Start();
            try
            {
                // Open the Dob connection.
                m_dobConnection.Open("VehicleAppCs", "0", 0, this, this);

                entityHandler.Init();
                serviceHandler.Init();
                MessageSender.Instance.Init();

                // Start the one and only thread.
                mainLoop.Run();
            }
            finally
            {
                Safir.Application.CrashReporter.Stop();
            }
        }

        /// <summary>
        /// Overrides Safir.Dob.StopHandler. Terminate application.
        /// </summary>
        public void OnStopOrder()
        {
            mainLoop.Stop();
        }

        /// <summary>
        /// Overrides Safir.Dob.Dispatcher. Indicates that there is incoming 
        /// data for the connection so the application shall call Dispatch().
        /// Go through all DOB entries in inqueue.
        /// </summary>
        public void OnDoDispatch()
        {
            // Switch to application thread to not "hangup" calling Dob thread while
            // going through all DOB entries in inqueue.
            try
            {
                mainLoop.Invoke(m_callDispatch);
            }
            catch (Exception e)
            {
                String msg = e.Message + " " + e.InnerException + " " + e.StackTrace;
                Safir.Logging.SendSystemLog(Safir.Logging.Severity.Critical,
                                            "Unhandled Exception in OnDoDispatch: " + msg);
            }
        }
    }
}
