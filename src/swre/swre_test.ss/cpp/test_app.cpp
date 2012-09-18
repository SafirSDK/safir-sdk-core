/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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
#include <iostream>
#include <Safir/SwReports/SwReport.h>
#include <ace/Thread.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Utilities/Internal/PanicLogging.h> 
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <ace/Time_Value.h>
#include <sstream>
extern bool cyclic;
extern bool noTracer;
extern bool panicLog;

#include "test_app.h"

namespace reports = Safir::SwReports;

namespace SwreTest
{
    // Thread 1 timers
    const int FATAL_ERROR_TIMER_T1 = 1;
    const int ERROR_TIMER_T1 = 2;
    const int PROGRAMMING_ERROR_TIMER_T1 = 3;
    const int RESOURCE_TIMER_T1 = 4;
    const int PROGRAM_INFO_TIMER_T1 = 5;
    const int TRACE_TIMER_T1 = 6;
    const int SWITCH_CONTEXT_TIMER_T1 = 7;


    const ACE_Time_Value FATAL_ERROR_TIMEOUT_T1 = ACE_Time_Value(0,500000);
    const ACE_Time_Value ERROR_TIMEOUT_T1 = ACE_Time_Value(1);
    const ACE_Time_Value PROGRAMMING_ERROR_TIMEOUT_T1 = ACE_Time_Value(1,500000);
    const ACE_Time_Value RESOURCE_TIMEOUT_T1 = ACE_Time_Value(2);
    const ACE_Time_Value PROGRAM_INFO_TIMEOUT_T1 = ACE_Time_Value(0,200000);
    const ACE_Time_Value TRACE_TIMEOUT_T1 = ACE_Time_Value(2);
    const ACE_Time_Value SWITCH_CONTEXT_TIMEOUT_T1 = ACE_Time_Value(5);

    /*
    // Thread 2 timers
    const int FATAL_ERROR_TIMER_T2 = 7;
    const int ERROR_TIMER_T2 = 8;
    const int PROGRAMMING_ERROR_TIMER_T2 = 9;
    const int RESOURCE_TIMER_T2 = 10;
    const int PROGRAM_INFO_TIMER_T2 = 11;
    const int TRACE_TIMER_T2 = 12;

    const ACE_Time_Value FATAL_ERROR_TIMEOUT_T2 = ACE_Time_Value(20);
    const ACE_Time_Value ERROR_TIMEOUT_T2 = ACE_Time_Value(16);
    const ACE_Time_Value PROGRAMMING_ERROR_TIMEOUT_T2 = ACE_Time_Value(11);
    const ACE_Time_Value RESOURCE_TIMEOUT_T2 = ACE_Time_Value(10);
    const ACE_Time_Value PROGRAM_INFO_TIMEOUT_T2 = ACE_Time_Value(6);
    const ACE_Time_Value TRACE_TIMEOUT_T2 = ACE_Time_Value(3);

    //    const ACE_Time_Value now = boost::posix_time::milliseconds(0);

    ACE_THR_FUNC_RETURN ThreadFunc(void* _this)
    {
        static_cast<TestApp*>(_this)->ThreadImpl();
        return 0;
    }
    */
    TestApp::TestApp():
        ACE_Event_Handler(ACE_Reactor::instance()),
        m_dispatcher1(m_connection),
        m_tracer1Inst1(L"TRACER1"),
        m_tracer1Inst2(L"TRACER1"),
        m_tracer2(L"TRACER2")
    {
        std::wcout << "**** Application " << "SWRE_CPLUSPLUS_TEST is started *** " << std::endl << std::endl;

        try
        {
            m_connection.Open (L"SWRE_CPLUSPLUS_TEST", L"0", 0, this, &m_dispatcher1);
        }
        catch (const Safir::Dob::NotOpenException&)
        {
            exit(-1);
        }

        m_backdoorKeeper.Start(*this);
        //    m_piHandler1.Start(connection);

        reactor()->schedule_timer(this,(void*)FATAL_ERROR_TIMER_T1,ACE_Time_Value(0));
        reactor()->schedule_timer(this, (void*)FATAL_ERROR_TIMER_T1,ACE_Time_Value(0));
        reactor()->schedule_timer(this, (void*)ERROR_TIMER_T1,ACE_Time_Value(0));
        reactor()->schedule_timer(this, (void*)PROGRAMMING_ERROR_TIMER_T1,ACE_Time_Value(0));
        reactor()->schedule_timer(this, (void*)RESOURCE_TIMER_T1,ACE_Time_Value(0));
        reactor()->schedule_timer(this, (void*)PROGRAM_INFO_TIMER_T1,ACE_Time_Value(0));
        reactor()->schedule_timer(this, (void*)SWITCH_CONTEXT_TIMER_T1,ACE_Time_Value(0));
        if (!noTracer)
        {
            reactor()->schedule_timer(this, (void*)TRACE_TIMER_T1,ACE_Time_Value(0));
        }

        if (panicLog)
        {
            std::wostringstream ostr; 
            ostr << "Panic log test" << std::endl;
            Safir::Utilities::Internal::PanicLogging::Log(Safir::Dob::Typesystem::Utilities::ToUtf8(ostr.str())); 
        }

        // Start a second thread

        //ACE_Thread::spawn(ThreadFunc, this);
    }

    void TestApp::OnStopOrder()
    {
        ACE_Reactor::instance()->end_reactor_event_loop();
    }


    TestApp::~TestApp()
    {
        m_connection.Close();
    }

    void TestApp::Run()
    {
        reactor()->run_reactor_event_loop();
        std::cout << std::endl << "**** Application is shutting down*** " << std::endl << std::endl;
    }





    int TestApp::handle_timeout (const ACE_Time_Value & /*current_time*/,
                                 const void * act)
    {
        switch (reinterpret_cast<long>(act))
        {
        case FATAL_ERROR_TIMER_T1:
            {
                reports::SendFatalErrorReport(L"FatalErrorCode1", L"SwreTest::TestApp", L"This is a fatal error text from THREAD1");
                if (cyclic)
                {
                    reactor()->schedule_timer(this, (void*)FATAL_ERROR_TIMER_T1,FATAL_ERROR_TIMEOUT_T1);
                }
            }
            break;

        case ERROR_TIMER_T1:
            {
                reports::SendErrorReport(L"ErrorCode", L"SwreTest::TestApp", L"Error text from THREAD1");
                if (cyclic)
                {
                    reactor()->schedule_timer(this, (void*)ERROR_TIMER_T1,ERROR_TIMEOUT_T1);
                }
            }
            break;

        case PROGRAMMING_ERROR_TIMER_T1:
            {
                reports::SendProgrammingErrorReport(L"ProgrammingErrorCode", L"SwreTest::TestApp", L"This is a programming error text from THREAD1");
                if (cyclic)
                {
                    reactor()->schedule_timer(this, (void*)PROGRAMMING_ERROR_TIMER_T1,PROGRAMMING_ERROR_TIMEOUT_T1);
                }
            }
            break;

        case RESOURCE_TIMER_T1:
            {
                reports::SendResourceReport(L"ResourceId", false, L"Resource report text from THREAD1");
                if (cyclic)
                {
                    reactor()->schedule_timer(this, (void*)RESOURCE_TIMER_T1,RESOURCE_TIMEOUT_T1);
                }
            }
            break;

        case PROGRAM_INFO_TIMER_T1:
            {
                reports::SendProgramInfoReport(L"PI information text from THREAD1");
                if (cyclic)
                {
                    reactor()->schedule_timer(this, (void*)PROGRAM_INFO_TIMER_T1,PROGRAM_INFO_TIMEOUT_T1);
                }
            }
            break;

        case TRACE_TIMER_T1:
            {
                m_tracer1Inst1 << L"This is logging via TRACER1 inst 1" << L" in thread 1" << std::endl;
                m_tracer1Inst2 << L"This is logging via TRACER1 inst 2" << L" in thread 1" << std::endl;

                m_tracer2 << L"This is logging via TRACER2" << L" in thread 1"<< std::endl;
                
              //  if (cyclic)
                {
                    reactor()->schedule_timer(this, (void*)TRACE_TIMER_T1,TRACE_TIMEOUT_T1);
                }

            }
            break;

        case SWITCH_CONTEXT_TIMER_T1:
            {
                std::wcout << "Switching context" << std::endl;

                Safir::Dob::ConnectionAspectMisc c = Safir::Dob::ConnectionAspectMisc(m_connection);

                Safir::Dob::Typesystem::Int32 newContext = 0;
                if (c.GetContext() == 0)
                {
                    newContext = 1;
                }

                m_connection.Close();
                m_connection.Open (L"SWRE_CPLUSPLUS_TEST", L"0", newContext, this, &m_dispatcher1);


                if (cyclic)
                {
                    reactor()->schedule_timer(this, (void*)SWITCH_CONTEXT_TIMER_T1, SWITCH_CONTEXT_TIMEOUT_T1);
                }

            }
            break;
            /*
        case FATAL_ERROR_TIMER_T2:
            {
                reports::SendFatalErrorReport(L"Code2", L"SwreTest::TestApp", L"This is a fatal error text from THREAD2");
                if (cyclic)
                {
                    Safir::Application::TimerHandler::Instance().SetTimer(FATAL_ERROR_TIMER_T2, FATAL_ERROR_TIMEOUT_T2, this);
                }
            }
            break;

        case ERROR_TIMER_T2:
            {
                reports::SendErrorReport(L"Code22", L"SwreTest::TestApp", L"Error text from THREAD2");
                if (cyclic)
                {
                    Safir::Application::TimerHandler::Instance().SetTimer(ERROR_TIMER_T2, ERROR_TIMEOUT_T2, this);
                }
            }
            break;

        case PROGRAMMING_ERROR_TIMER_T2:
            {
                reports::SendProgrammingErrorReport(L"Code2222", L"SwreTest::TestApp", L"This is a programming error text from THREAD2");
                if (cyclic)
                {
                    Safir::Application::TimerHandler::Instance().SetTimer(PROGRAMMING_ERROR_TIMER_T2, PROGRAMMING_ERROR_TIMEOUT_T2, this);
                }
            }
            break;

        case RESOURCE_TIMER_T2:
            {
                reports::SendResourceReport(L"Resource222", true, L"Resource report text from THREAD2");
                if (cyclic)
                {
                    Safir::Application::TimerHandler::Instance().SetTimer(RESOURCE_TIMER_T2, RESOURCE_TIMEOUT_T2, this);
                }
            }
            break;

        case PROGRAM_INFO_TIMER_T2:
            {
                reports::SendProgramInfoReport(L"PI information text from THREAD2");
                if (cyclic)
                {
                    Safir::Application::TimerHandler::Instance().SetTimer(PROGRAM_INFO_TIMER_T2, PROGRAM_INFO_TIMEOUT_T2, this);
                }
            }
            break;

        case TRACE_TIMER_T2:
            {
                m_tracer1Inst1 << L"This is logging via TRACER1 inst 1" << L" in thread 2" << std::endl;
                m_tracer1Inst2 << L"This is logging via TRACER1 inst 2" << L" in thread 2" << std::endl;

                m_tracer2 << L"This is logging via TRACER2" << L" in thread 2"<< std::endl;

                Safir::Application::TimerHandler::Instance().SetTimer(TRACE_TIMER_T2, TRACE_TIMEOUT_T2, this);

            }
            break;
            */
        default:
            {
                std::cout << std::endl << "ERROR: Unexpected timer" << std::endl << std::endl;
            }


        }
        return 0;
    }
    /*
      void TestApp::OnEvent(const osin::EventId& ev)
      {
      if (ev == m_dispatchEvent.IdOf())
      {
      m_connection.Dispatch();
      }
      else if (ev == m_dispatchEvent2.IdOf())
      m_connection2.Dispatch();
      }
    */
    /*void TestApp::ThreadImpl()
    {
        try
        {
            m_connection2.Open (L"SWRE_CPLUSPLUS_TEST", L"T2", 0, NULL, m_dispatchEvent2);
            Safir::Application::EventHandler::Instance().AddEvent(m_dispatchEvent2, this);
        }
        catch (const Safir::Dob::NotOpenException&)
        {
            exit(-1);
        }

        //m_connection.Open(L"SWRE_TEST_T2",L"",0, NULL);
        //The NULL above will likely cause a crash if the app were to receive a stop order,
        //but this is just a test program, so who cares...

        //Safir::Dob::Connection & connection = Safir::Application::DobConnection::Instance().Get();
        m_thread2PiHandler.Start(m_thread2PiHandler);

        Safir::Application::TimerHandler::Instance().SetTimer(FATAL_ERROR_TIMER_T2, now, this);
        Safir::Application::TimerHandler::Instance().SetTimer(ERROR_TIMER_T2, now, this);
        Safir::Application::TimerHandler::Instance().SetTimer(PROGRAMMING_ERROR_TIMER_T2, now, this);
        Safir::Application::TimerHandler::Instance().SetTimer(RESOURCE_TIMER_T2, now, this);
        Safir::Application::TimerHandler::Instance().SetTimer(PROGRAM_INFO_TIMER_T2, now, this);
        Safir::Application::TimerHandler::Instance().SetTimer(TRACE_TIMER_T2, now, this);

        m_t2MainLoop.Start(false);

        return 0;
    }
    */
    void TestApp::HandleCommand(const std::vector<std::wstring>& cmdTokens)
    {
        std::wostringstream ostr;
        ostr << "Received command + parameters:";

        for (std::vector<std::wstring>::const_iterator it = cmdTokens.begin();
             it != cmdTokens.end();
             ++it)
        {
            ostr << ' ';
            ostr << (*it);
        }

        Safir::SwReports::SendProgramInfoReport(ostr.str());
    }

    std::wstring TestApp::GetHelpText()
    {
        return L"\tSHOW_STATUS [1]\tShow application status (handler 1)\n"
            L"\tSHOW_SWITCHES [2]\tShow status for switches (handler 1)\n";
    }

}
