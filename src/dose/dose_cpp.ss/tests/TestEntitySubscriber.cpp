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
// Test program for Entity class.
//

#include <DoseTest/GlobalEntity.h>
#include <DoseTest/GlobalMessage.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Typesystem/Serialization.h>

#include <iostream>

#include <ace/Event_Handler.h>
#include <ace/OS_NS_unistd.h>
#include <ace/Reactor.h>
#include <ace/Thread.h>

class TestEntitySubscriber :
   public Safir::Dob::StopHandler,
   public Safir::Dob::Dispatcher,
   public Safir::Dob::EntitySubscriber,
   public ACE_Event_Handler
{
public:
   TestEntitySubscriber(ACE_Reactor* reactor) :
      ACE_Event_Handler(reactor) {};
   ~TestEntitySubscriber() {};

   // Overrides Safir::Dob::StopHandler
   void  OnStopOrder();

   // Overrides Safir::Dob::Dispatcher
   void OnDoDispatch();

   // Overrides Safir::Dob::EntitySubscriber
   void OnNewEntity (Safir::Dob::EntityPtr entity, bool created);
   void OnUpdatedEntity (Safir::Dob::EntityPtr entity);
   void OnRemovedEntity (const Safir::Dob::Typesystem::ObjectId &objectId, bool deleted);

   // Overrides ACE_Event_Handler
   int handle_timeout (const ACE_Time_Value &current_time, const void *act=0);
   int handle_input (ACE_HANDLE fd);


   // Tests
   void Connect();
   void Status();
   void Subscribe();


private:
   Safir::Dob::Connection m_connection;

};

// ----------------------------------------------------------------------

void
TestEntitySubscriber::OnStopOrder()
{
   std::wcout << "TestEntitySubscriber::OnStopOrder() - called... tid:" << ACE_Thread::self() << std::endl;
   reactor()->end_reactor_event_loop();
}

void
TestEntitySubscriber::OnDoDispatch()
{
   //std::wcout << "TestEntitySubscriber::OnDoDispatch() - called. tid: " << ACE_Thread::self() << std::endl;
   reactor()->notify(this,ACE_Event_Handler::READ_MASK);
}

void
TestEntitySubscriber::OnNewEntity (Safir::Dob::EntityPtr entity, bool created)
{
   //std::wcout << "TestEntitySubscriber::OnNewEntity() - called..." << std::endl;

   if(entity->GetTypeId() == DoseTest::GlobalEntity::ClassTypeId)
   {
      const DoseTest::GlobalEntityPtr ge = boost::static_pointer_cast<DoseTest::GlobalEntity>(entity);

      //std::wcout << "TestEntitySubscriber::OnNewEntity() - info: " << ge->Info().GetVal() << std::endl;
      std::wcout << "TestEntitySubscriber::OnNewEntity() - info: " << Safir::Dob::Typesystem::Serialization::ToXml(ge) << std::endl;   }
}

void
TestEntitySubscriber::OnUpdatedEntity (Safir::Dob::EntityPtr entity)
{
   if(entity->GetTypeId() == DoseTest::GlobalEntity::ClassTypeId)
   {
      const DoseTest::GlobalEntityPtr ge = boost::static_pointer_cast<DoseTest::GlobalEntity>(entity);

      //std::wcout << "TestEntitySubscriber::OnUpdatedEntity() - info: " << ge->Info().GetVal() << std::endl;
      std::wcout << "TestEntitySubscriber::OnUpdatedEntity() - info: " << Safir::Dob::Typesystem::Serialization::ToXml(ge) << std::endl;
   }
}

void
TestEntitySubscriber::OnRemovedEntity (const Safir::Dob::Typesystem::ObjectId &objectId, bool deleted)
{
   std::wcout << "TestEntitySubscriber() - objectId.GetInstance(): " << objectId.GetInstance()
              << ". deleted: " << std::boolalpha << deleted << std::endl;
}


int
TestEntitySubscriber::handle_timeout (const ACE_Time_Value &current_time,
                                const void* arg)
{
   std::wcout << "TestEntitySubscriber::handle_timeout() - ACE_Thread::self(): " << ACE_Thread::self()
              << ". current_time: " << current_time.sec()
              << ". arg: " << arg
              << std::endl;

   return 0;
}

int
TestEntitySubscriber::handle_input (ACE_HANDLE handle)
{

//    std::wcout << "TestEntitySubscriber::handle_input() - ACE_Thread::self(): " << ACE_Thread::self()
//               << ". handle: " << handle
//               << std::endl;

   m_connection.Dispatch();

   return 0;
}


void
TestEntitySubscriber::Connect()
{
   std::wcout << "TestEntitySubscriber::Connect() - called. tid:" << ACE_Thread::self() << std::endl;

   m_connection.Open(L"TestEntitySubscriber",
                     L"0",
                     0,
                     this,
                     this);
}


void
TestEntitySubscriber::Status()
{
   std::wcout << "TestEntitySubscriber::Status() - Open: " << m_connection.IsOpen() <<  std::endl;

   if (m_connection.IsOpen())
      std::wcout << "TestEntitySubscriber::Status() - Name: " << m_connection.GetConnectionName() << std::endl;

}

void
TestEntitySubscriber::Subscribe()
{
   m_connection.SubscribeEntity(Safir::Dob::Typesystem::ObjectId(DoseTest::GlobalEntity::ClassTypeId,
                                                                 Safir::Dob::Typesystem::WHOLE_CLASS),
                                true, // enableChange
                                true, // enableUpdate
                                this);

}



// ----------------------------------------------------------------------

// Main entry
int main(int /*argc*/, char** /*argv*/)
{
   std::wcout << "main() - TestEntitySubscriber starting..." << std::endl;

   ACE_Reactor reactor;
   TestEntitySubscriber c(&reactor);

   // Setup Notify event handler
   reactor.register_handler(&c, ACE_Event_Handler::READ_MASK);

   // Make connection
   c.Connect();

   c.Status();
   c.Subscribe();

   // Loop "forever"...
   reactor.run_reactor_event_loop();

   std::wcout << "main() - TestEntitySubscriber stopping..." << std::endl;

   return 0;
}
