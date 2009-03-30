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

//
// Test program for Connection class.
//

#include <DoseTest/GlobalMessage.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ConnectionMiscAspect.h>
#include <Safir/Dob/ConnectionPostponeAspect.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

#include <iostream>

#include <ace/Event_Handler.h>
#include <ace/OS_NS_unistd.h>
#include <ace/Reactor.h>
#include <ace/Thread.h>

#ifdef GetMessage
#undef GetMessage
#endif

class TestReceiveMsg :
   public Safir::Dob::StopHandler,
   public Safir::Dob::Dispatcher,
   public Safir::Dob::MessageSubscriber,
   public ACE_Event_Handler
{
public:
   TestReceiveMsg(ACE_Reactor* reactor) :
      ACE_Event_Handler(reactor) {};
   ~TestReceiveMsg() {};

   // Overrides Safir::Dob::StopHandler
   void  OnStopOrder();

   // Overrides Safir::Dob::Dispatcher
   void OnDoDispatch();

   // Overrides Safir::Dob::MessageSubscriber
   void OnMessage(Safir::Dob::MessageProxy messageProxy);

   // Overrides ACE_Event_Handler
   int handle_timeout (const ACE_Time_Value &current_time, const void *act=0);
   int handle_input (ACE_HANDLE fd);


   // Tests
   void Connect();
   void Status();


   void Subscribe();
   void Unsubscribe();

private:
   Safir::Dob::Connection m_connection;

};

// ----------------------------------------------------------------------

void
TestReceiveMsg::OnStopOrder()
{
   std::wcout << "TestReceiveMsg::OnStopOrder() - called... tid:" << ACE_Thread::self() << std::endl;
   reactor()->end_reactor_event_loop();
}

void
TestReceiveMsg::OnDoDispatch()
{
   std::wcout << "TestReceiveMsg::OnDoDispatch() - called. tid: " << ACE_Thread::self() << std::endl;
   reactor()->notify(this,ACE_Event_Handler::READ_MASK);
}

void
TestReceiveMsg::OnMessage(Safir::Dob::MessageProxy messageProxy)
{
    static int no = 0;
    DoseTest::GlobalMessagePtr gmp = boost::static_pointer_cast<DoseTest::GlobalMessage>(messageProxy.GetMessage());

    std::wcout << "TestReceiveMsg::OnMessage() - msg: " << gmp->Info().GetVal() << std::endl;
    ++no;
    if (no == 2)
    {
        std::wcout << "TestReceiveMsg::OnMessage() - Postponing(with redispatch)" << std::endl;
        Safir::Dob::ConnectionPostponeAspect(m_connection).Postpone(true);
    }
    if (no == 6)
    {
        std::wcout << "TestReceiveMsg::OnMessage() - Postponing (without redispatch)" << std::endl;
        Safir::Dob::ConnectionPostponeAspect(m_connection).Postpone(false);
    }
}

int
TestReceiveMsg::handle_timeout (const ACE_Time_Value &current_time,
                                const void* arg)
{
   std::wcout << "TestReceiveMsg::handle_timeout() - ACE_Thread::self(): " << ACE_Thread::self()
              << ". current_time: " << current_time.sec()
              << ". arg: " << arg
              << std::endl;

   return 0;
}

int
TestReceiveMsg::handle_input (ACE_HANDLE)
{

//    std::wcout << "TestReceiveMsg::handle_input() - ACE_Thread::self(): " << ACE_Thread::self()
//               << ". handle: " << handle
//               << std::endl;
   static int i = 0;
   ++i;
   if (i == 4 || i == 8)
   {
       Safir::Dob::ConnectionPostponeAspect(m_connection).ResumePostponed();
   }
   m_connection.Dispatch();

   return 0;
}


void
TestReceiveMsg::Connect()
{
   std::wcout << "TestReceiveMsg::Connect() - called. tid:" << ACE_Thread::self() << std::endl;

   m_connection.Open(L"TestReceiveMsg",
                     L"0",
                     0,
                     this,
                     this);
}


void
TestReceiveMsg::Status()
{
   std::wcout << "TestReceiveMsg::Status() - Open: " << m_connection.IsOpen() <<  std::endl;

   if (m_connection.IsOpen())
   {
       std::wcout << "TestReceiveMsg::Status() - Name: "
           << Safir::Dob::ConnectionMiscAspect(m_connection).GetConnectionName() << std::endl;
   }

}

void
TestReceiveMsg::Subscribe()
{
   m_connection.SubscribeMessage(DoseTest::GlobalMessage::ClassTypeId,
                                 Safir::Dob::Typesystem::ChannelId::ALL_CHANNELS,
                                 this);

}

void
TestReceiveMsg::Unsubscribe()
{
   m_connection.UnsubscribeMessage(DoseTest::GlobalMessage::ClassTypeId,
                                   Safir::Dob::Typesystem::ChannelId::ALL_CHANNELS,
                                   this);

}

// ----------------------------------------------------------------------

// Main entry
int main(int /*argc*/, char** /*argv*/)
{
   std::wcout << "main() - TestReceiveMsg starting..." << std::endl;

   ACE_Reactor reactor;
   TestReceiveMsg c(&reactor);

   // Setup Notify event handler
   reactor.register_handler(&c, ACE_Event_Handler::READ_MASK);

   // Make connection
   c.Connect();

   c.Status();

   c.Subscribe();

   // Loop "forever"...
   reactor.run_reactor_event_loop();

   std::wcout << "main() - TestReceiveMsg stopping..." << std::endl;

   return 0;
}
