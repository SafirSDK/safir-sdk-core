/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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
using System.Threading;
using Safir.Application;
using Safir.Dob;
using Safir.SwReports;

namespace swreTestAppDotnet
{
    /// <summary>
    /// SWRE test application
    /// </summary>
    public class App : Safir.Application.BusinessApplicationContext, 
        Safir.Application.Backdoor, Safir.Dob.StopHandler, Safir.Dob.Dispatcher 
    {


        private void TimerHandler(object parameter)
        {
            SwReport.SendFatalErrorReport("FatalErrorCode99", "C#TestApp", "Fatal Error text99");
            SwReport.SendFatalErrorReport("FatalErrorCode100", "C#TestApp", "Fatal Error text100");

            SwReport.SendErrorReport("ErrorCode44", "C#TestApp", "Error text44");
            SwReport.SendErrorReport("ErrorCode55", "C#TestApp", "Error text55");

            SwReport.SendResourceReport("Resource77", true, "Resource text77");
            SwReport.SendResourceReport("Resource88", true, "Resource text88");

            SwReport.SendProgramInfoReport("Important information from the C# application");
            SwReport.SendProgramInfoReport("More Important information from the C# application");

            SwReport.SendProgrammingErrorReport("ProgrammingErrorCode33", "C#TestApp", "C# programming error33");
            SwReport.SendProgrammingErrorReport("ProgrammingErrorCode34", "C#TestApp", "C# programming error34");

            SwReport.SendProgrammingErrorReport("ProgrammingErrorCode35", "C#TestApp", "C# programming error35");
            SwReport.SendProgrammingErrorReport("ProgrammingErrorCode36", "C#TestApp", "C# programming error36");

            Console.WriteLine("**** (" + (++m_batch).ToString() + ") Reports sent");

            for (int i = 0; i < 100; ++i)
            {
                m_tracer.WriteLine("Testing logging to tracer " + i);
            }
        }

        public App()
        {
            m_connection.Open("C#T1", "", 0, this, this);
            m_tracer = new Safir.Application.Tracer("CSharpTest");
            m_timerHandler = new TimerCallbackDelegate(TimerHandler);
            m_tracer.WriteLine("Hello");
        }

        public Version Version
        {
            get
            {
                return new Version(0,0,0,1);
            }
        }

        public void OnStopOrder()
        {
            Safir.Application.BusinessApplication.Terminate();
        }

        /// <summary>
        ///   Is called by Dob when it's time to dispatch.
        /// </summary>
        public void OnDoDispatch()
        {
            m_callDispatch = new Safir.Application.MethodDelegate(m_connection.Dispatch);
            Safir.Application.BusinessApplication.Invoke(this.m_callDispatch);
        }

        public void Startup()
        {
            Console.WriteLine("");
            Console.WriteLine("**** Application " + "SWRE C# TEST" + " version " + Version.ToString() + " is started*** ");
            Console.WriteLine("");


            m_doorkeeper = new BackdoorKeeper();
            m_doorkeeper.Start(this);

            m_sendReportsTimer = new Safir.Application.Timer(m_timerHandler, 1000, 20000, null);
        }

        public void OnAddResources()
        {
        }

        public void CloseDown()
        {
        }


        #region IProgramInfoHandler Members

        public string GetHelpText()
        {
            return "\tSHOW_STATUS [1]\tShow dotnet application status (handler 1)\n" +
                   "\tSHOW_SWITCHES [2]\tShow dotnet status for switches (handler 1)\n";
        }

        public void HandleCommand(string[] cmdTokens)
        {
            System.Text.StringBuilder ostr = new System.Text.StringBuilder();

            foreach (string str in cmdTokens)
            {
                ostr.Append(' ');
                ostr.Append(str);
            }

            SwReport.SendProgramInfoReport(ostr.ToString());
        }

        #endregion

        private TimerCallbackDelegate m_timerHandler;

        private Safir.Application.Timer m_sendReportsTimer;

        private Safir.Dob.Connection m_connection = new Safir.Dob.Connection();
        private Safir.Application.MethodDelegate m_callDispatch;
        private int m_batch = 0;

        private Safir.Application.BackdoorKeeper m_doorkeeper;

        private Safir.Application.Tracer m_tracer;

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main(string [] args) 
        {
            Thread.CurrentThread.Name = "MainThread";
            Console.WriteLine(".... Starting App");
            Console.WriteLine("");

            App app = new App();
            Safir.Application.BusinessApplication.Run(app);
        }
    }
}
