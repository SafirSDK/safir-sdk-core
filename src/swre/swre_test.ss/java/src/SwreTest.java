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
import safir.dob.*;
import safir.swreports.*;
public class SwreTest 
{

    public SwreTest()
    {
        m_connection.open("swreTestJava", "", 0, m_stopper, m_dispatcher);

    }

    private enum SynchReason {None, Dispatch, StopOrder, Timeout}

    private class Synchronizer 
    {
        public Synchronizer() {reason = SynchReason.None;}
        public SynchReason reason;
    }

    private Synchronizer m_synchronizer = new Synchronizer();

    private class Dispatcher implements safir.dob.Dispatcher
    {
        public Dispatcher(Synchronizer synchronizer)
        {
            m_synchronizer = synchronizer;
        }

        public void onDoDispatch()
        {
            //System.out.println("Got dispatch callback for " + m_reason.toString());
            synchronized(m_synchronizer)
            {
                m_synchronizer.reason = SynchReason.Dispatch;
                m_synchronizer.notify();
            }
        }

        private Synchronizer m_synchronizer;
    }



    private class Stopper implements StopHandler
    {
        public Stopper(Synchronizer synchronizer)
        {
            m_synchronizer = synchronizer;
        }

        public void onStopOrder()
        {
            synchronized(m_synchronizer)
            {
                m_synchronizer.reason = SynchReason.StopOrder;
                m_synchronizer.notify();
            }
        }

        private Synchronizer m_synchronizer;
    }

    private class Timer extends java.util.TimerTask
    {
        public Timer(Synchronizer synchronizer)
        {
            m_synchronizer = synchronizer;
        }

        public void run()
        {
            synchronized(m_synchronizer)
            {
                m_synchronizer.reason = SynchReason.Timeout;
                m_synchronizer.notify();
            }
        }

        private Synchronizer m_synchronizer;
    }

    private class Backdoor 
        implements safir.application.Backdoor
    {
        public String getHelpText()
        {
            System.out.println("getHelpText()");
            return "There is no help to get!";
        }
        
        public void handleCommand(String [] cmdTokens)
        {
            System.out.println("handleCommand");
            m_tracer.println("This is the command line I got");
            System.out.println("This is the command line I got");
            for (String str : cmdTokens)
            {
                m_tracer.println("  '" + str + "'");
                System.out.println("  '" + str + "'");
            }
        }
    }

    private void HandleTimeout()
    {
        System.out.println("HandleTimeout");
        SwReport.SendFatalErrorReport("FatalErrorCode99", "JavaTestApp", "Fatal Error text99");
        SwReport.SendFatalErrorReport("FatalErrorCode100", "JavaTestApp", "Fatal Error text100");
        
        SwReport.SendErrorReport("ErrorCode44", "JavaTestApp", "Error text44");
        SwReport.SendErrorReport("ErrorCode55", "JavaTestApp", "Error text55");
        
        SwReport.SendResourceReport("Resource77", true, "Resource text77");
        SwReport.SendResourceReport("Resource88", true, "Resource text88");
        
        SwReport.SendProgramInfoReport("Important information from the Java application");
        SwReport.SendProgramInfoReport("More Important information from the Java application");
        
        SwReport.SendProgrammingErrorReport("ProgrammingErrorCode33", "JavaTestApp", "Java programming error33");
        SwReport.SendProgrammingErrorReport("ProgrammingErrorCode34", "JavaTestApp", "Java programming error34");
        
        SwReport.SendProgrammingErrorReport("ProgrammingErrorCode35", "JavaTestApp", "Java programming error35");
        SwReport.SendProgrammingErrorReport("ProgrammingErrorCode36", "JavaTestApp", "Java programming error36");
        
        
        for (int i = 0; i < 100; ++i)
        {
            m_tracer.println("Testing logging to tracer " + i);
        }
    }

    

    private void run()
    {
        m_keeper.start(m_backdoor);
        java.util.Timer timer = new java.util.Timer(true);
        timer.schedule(new Timer(m_synchronizer),10000);
        while (!m_isDone)
        {
            try
            {
                SynchReason reason;
                synchronized(m_synchronizer)
                {
                    //a new event may have been signalled while we were handling the last one
                    //but we only care about the Dispatches
                    if (m_synchronizer.reason == SynchReason.None)
                    {
                        while (m_synchronizer.reason == SynchReason.None) //guard against spurious wakeups
                        {
                            m_synchronizer.wait();
                            //System.out.println ("Got out of wait: " + m_synchronizer.reason.toString());
                            
                        }
                        //System.out.println ("Got notified of a " + m_synchronizer.reason.toString());
                    }
                    else
                    {
                        //System.out.println ("Something happened while we were handling something else "+ m_synchronizer.reason.toString());
                    }
          
                    reason = m_synchronizer.reason;
                    m_synchronizer.reason = SynchReason.None;
                }

                switch(reason)
                {
                case Dispatch:
                    {
                        m_connection.dispatch();
                    }
                    break;

                case StopOrder:
                    {
                        m_isDone = true;
                    }
                    break;

                case Timeout:
                    {
                        timer.schedule(new Timer(m_synchronizer),10000);
                        HandleTimeout();
                    }
                    break;
                    
                default:
                    {
                        System.out.println ("Spurious notification!!!!");
                    }
                }
        
            }
            catch(InterruptedException e)
            {
                System.out.println ("Got interrupted");
            }
        }

    }

    public static void main(String[] args)
    {
        SwreTest app = new SwreTest();
        app.run();
    }
    private safir.application.BackdoorKeeper m_keeper = new safir.application.BackdoorKeeper();
    private Backdoor m_backdoor = new Backdoor();
    private boolean m_isDone = false;
    private safir.dob.Connection m_connection = new safir.dob.Connection();
    private Dispatcher m_dispatcher = new Dispatcher(m_synchronizer);
    
    private Stopper m_stopper = new Stopper(m_synchronizer);
    private safir.application.Tracer m_tracer = new safir.application.Tracer("javaTracer");
}
