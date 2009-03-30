/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: Erik Adolfsson / sterad
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
using Safir.Dob;
using System;
using System.Windows.Forms;
using System.Data;

namespace douf_time_test
{

    /// <summary>
    /// Summary description for DobHandler.
    /// </summary>
    public class DobHandler : System.Windows.Forms.Form, Safir.Dob.StopHandler, Safir.Dob.Dispatcher
    {

        delegate void CallDispatchDelegate();
        private CallDispatchDelegate callDispatch;

        private Safir.Dob.Connection m_Dob = new Safir.Dob.Connection();

        public DobHandler()
        {
            callDispatch = new CallDispatchDelegate( m_Dob.Dispatch );
        }

        public Safir.Dob.Connection Dob
        {
            get
            {
                return m_Dob;
            }
        }

       
        //------------------------------------------
        // Connect/disconnect Dob
        //------------------------------------------
        public void Start()
        {
            int inst = 0;
            for (;;)
            {
                try
                {
                    // Open the DOB connection.
                    Dob.Open("Time", string.Format("{0}", ++inst), 0, this, this);
                    break;
                }
                catch (Safir.Dob.NotOpenException)
                {
                }
            }
            
        }

        public void Stop()
        {
            Dob.Close();
        }

        public void OnStopOrder()
        {
            Stop();
        }

        public void OnDoDispatch()
        {
            System.IntPtr h = this.Handle; // Will force the handle to be created
         
            // Invoke will force a thread switch! 
            this.Invoke( callDispatch );
        }
    }
}
