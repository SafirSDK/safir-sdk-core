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
#ifndef TEST_APP_H
#define TEST_APP_H

#include <Safir/Application/Tracer.h>
#include <Safir/Utilities/AceDispatcher.h>
#include "pi_handler.h"
#include <ace/Recursive_Thread_Mutex.h>

namespace SwreTest
{

    //class TestThread;

    class TestApp :
        public Safir::Application::Backdoor,
        public Safir::Dob::StopHandler,
        public ACE_Event_Handler
    {

    public:
        unsigned ThreadImpl();

        TestApp();
        virtual ~TestApp();

        void Run();

    private:

        virtual int handle_timeout (const ACE_Time_Value & current_time,
                                    const void * act);

        virtual void OnStopOrder();

        void SendReports();

        virtual void HandleCommand(const std::vector<std::wstring>& cmdTokens);

        virtual std::wstring GetHelpText();

        Safir::Dob::Connection m_connection;
        Safir::Utilities::AceDispatcher m_dispatcher1;
        /*
        Safir::Dob::Connection m_connection2;
        Safir::Utilities::AceDispatcher m_dispatcher2;
        */
        Safir::Application::BackdoorKeeper m_backdoorKeeper;

        ACE_Recursive_Thread_Mutex m_lock;

        SeparatePiHandler m_thread2PiHandler;

        Safir::Application::Tracer m_tracer1Inst1;
        Safir::Application::Tracer m_tracer1Inst2;
        Safir::Application::Tracer m_tracer2;


    };

}
#endif
