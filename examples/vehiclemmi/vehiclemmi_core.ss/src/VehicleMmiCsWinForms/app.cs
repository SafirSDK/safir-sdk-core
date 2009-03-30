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

namespace VehicleMmiCsWinForms
{
    public delegate void MethodDelegate();

    /// <summary>
    /// This presentation application is an example of how to use Dob consumer
    /// mechanisms. The code is written to show the possibilities of the interfaces 
    /// rather than showing a real world example. The application can be run together
    /// with VehicleApp application that implements Dob producer mechanisms.
    /// 
    /// The application will show how to:
    /// 1) Subscribe for entity 'Vehicle' and display data in a listview and a dialog. 
    ///    It is possible for the operator to create, update and delete vehicles.
    /// 2) Send service request 'CalculateSpeedDifference' to calculate speed difference 
    ///    and receive service response 'CalculateSpeedDifferenceResponse' and
    ///    present the result in a dialog.
    /// 3) Use property 'SpeedObjectProperty' to get a property value from an object.
    /// 4) Susbcribe for message 'VehicleMsg' and present recived message text in a dialog.
    /// 5) Send service requests 'GetVehicleCategoryService' and 'SetVehicleCategoryService to
    ///    get and set category information in a database. The result is received through a
    ///    'VehicleCategoryServiceResponse' and presented in a dialog.
    /// </summary>


    /// <summary>
    /// Main class, controls startup, closedown, Dob connection and main window.
    /// </summary>
    public class App :
            // Allows this class to receive a stop order.
            Safir.Dob.StopHandler,
            // Allows this class to receive a dispatch order from Dob.
            Safir.Dob.Dispatcher 
    {
        ///////////////////////////////////
        // Private members and functions    

        /// <summary>
        /// Primary Dob connection
        /// </summary>
        private Safir.Dob.Connection m_DobConnection;


        /// <summary>
        /// Dob dispatch method
        /// </summary>
        /// 
        private MethodDelegate m_callDispatch;


        /// <summary>
        /// Main window
        /// </summary>
        private EntityFrame m_vehicleFrame;


        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        static void Main()
        {
            try
            {
                App app = new App();
                app.Startup();
            }
            catch (Exception e)
            {
                String msg = e.Message + " " + e.InnerException + " " + e.StackTrace;
                Safir.SwReports.SwReport.SendFatalErrorReport("Unhandled Exception", "Main", msg);
                Console.WriteLine(msg);           
            }
        }

        ///////////////////////////////////
        // Public members and functions    

        /// <summary>
        /// Open Dob connection and initiates dispatch
        /// </summary>
        public App()
        {
            m_DobConnection = new Safir.Dob.Connection();
            m_DobConnection.Open("VehicleMmiCsWinForms", "Vehicle", 0, this, this); 
            m_callDispatch = new MethodDelegate(m_DobConnection.Dispatch);            
        }


        /// <summary>
        /// Start application and show main window.
        /// </summary>
        public void Startup()
        {
            System.Windows.Forms.Application.EnableVisualStyles();
            System.Windows.Forms.Application.SetUnhandledExceptionMode(System.Windows.Forms.UnhandledExceptionMode.ThrowException);
            m_vehicleFrame = new EntityFrame();    
            System.Windows.Forms.Application.Run(m_vehicleFrame);
        }

      
        /// <summary>
        /// Overrides Safir.Dob.StopHandler. Terminate application.
        /// </summary>
        public void OnStopOrder()
        {
            m_vehicleFrame.Close();      
        }

     
        /// <summary>
        /// Overrides Safir.Dob.Dispatcher. Indicates that there is incoming 
        /// data for the connection so the application shall call Dispatch().
        /// Go through all Dob subscriptions & requests in inqueue.
        /// </summary>
        public void OnDoDispatch()
        {
            // Switch to applicationo thread to not "hangup" calling Dob thread while
            // going through all Dob subscriptions & requests in inqueue.
            try
            {
                m_vehicleFrame.Invoke(m_callDispatch);
            }
            catch (Exception e)
            {
                String msg = e.Message + " " + e.InnerException + " " + e.StackTrace;
                Safir.SwReports.SwReport.SendFatalErrorReport("Unhandled Exception", "OnDoDispatch", msg);
                Console.WriteLine(msg);
            }
        }
    }
}
