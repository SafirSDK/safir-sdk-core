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
#include <Safir/Dob/Typesystem/HandlerId.h>

#include <iostream>

#include <ace/Event_Handler.h>
#include <ace/OS_NS_unistd.h>
#include <ace/Reactor.h>
#include <ace/Thread.h>

class TestEntityOwner :
   public Safir::Dob::StopHandler,
   public Safir::Dob::Dispatcher,
   public Safir::Dob::MessageSubscriber,
   public Safir::Dob::EntityOwner,
   public ACE_Event_Handler
{
public:
   TestEntityOwner(ACE_Reactor* reactor) :
      ACE_Event_Handler(reactor),
      m_state(0) {};
   ~TestEntityOwner() {};

   // Overrides Safir::Dob::StopHandler
   void  OnStopOrder();

   // Overrides Safir::Dob::Dispatcher
   void OnDoDispatch();

   // Overrides Safir::Dob::MessageSubscriber
   void OnMessage(Safir::Dob::MessagePtr msg);

   // Overrides Safir::Dob::EntityOwner
   void OnRegistrationStatus(const Safir::Dob::Typesystem::ObjectId&, Safir::Dob::RegistrationStatus::Enumeration);
   void OnPersistentData (Safir::Dob::EntityPtr entity);
   void OnCreateRequest (Safir::Dob::EntityPtr entity, Safir::Dob::ResponseSenderPtr responseSender);
   void OnUpdateRequest (Safir::Dob::EntityPtr entity, Safir::Dob::ResponseSenderPtr responseSender);
   void OnDeleteRequest (const Safir::Dob::Typesystem::ObjectId &objectId, Safir::Dob::ResponseSenderPtr responseSender);

   // Overrides ACE_Event_Handler
   int handle_timeout (const ACE_Time_Value &current_time, const void *act=0);
   int handle_input (ACE_HANDLE fd);


   // Tests
   void Connect();
   void Status();
   void RegisterOwner();
   void Subscribe();


private:
   Safir::Dob::Connection m_connection;

   int m_state;
   DoseTest::GlobalEntityPtr m_ent;
};

// ----------------------------------------------------------------------

void
TestEntityOwner::OnStopOrder()
{
   std::wcout << "TestEntityOwner::OnStopOrder() - called... tid:" << ACE_Thread::self() << std::endl;
   reactor()->end_reactor_event_loop();
}

void
TestEntityOwner::OnDoDispatch()
{
   //std::wcout << "TestEntityOwner::OnDoDispatch() - called. tid: " << ACE_Thread::self() << std::endl;
   reactor()->notify(this,ACE_Event_Handler::READ_MASK);
}

void
TestEntityOwner::OnMessage(Safir::Dob::MessagePtr msg)
{
   DoseTest::GlobalMessagePtr gmp = boost::static_pointer_cast<DoseTest::GlobalMessage>(msg);

   std::wcout << "TestEntityOwner::OnMessage() - msg: " << Safir::Dob::Typesystem::Serialization::ToXml(gmp) << std::endl;

   if (m_state == 0)
   {
      // Create entity
      m_ent = DoseTest::GlobalEntity::Create();

      m_ent->SetInstanceNumber(0);
      m_ent->Info().SetVal(gmp->Info().GetVal());

      m_connection.Set(m_ent);

      ++m_state;
   }
   else if (m_state < 4)
   {
      // Update entity
      m_ent->Info().SetVal(gmp->Info().GetVal());
      m_connection.Set(m_ent);

      ++m_state;
   }
   else
   {
      // Delete entity
      const Safir::Dob::Typesystem::ObjectId objId(DoseTest::GlobalEntity::ClassTypeId, 0);
      m_connection.Delete(objId);

      m_state = 0;
   }

}


void
TestEntityOwner::OnRegistrationStatus(const Safir::Dob::Typesystem::ObjectId& objId, Safir::Dob::RegistrationStatus::Enumeration status)
{
   std::wcout << "TestEntityOwner::OnRegistrationStatus() - called.. ObjId: " << objId << ". status: " << status << std::endl;
}

void
TestEntityOwner::OnPersistentData (Safir::Dob::EntityPtr entity)
{
   std::wcout << "TestEntityOwner::OnPersistentData() - called.. " << std::endl;
}

void
TestEntityOwner::OnCreateRequest (Safir::Dob::EntityPtr entity, Safir::Dob::ResponseSenderPtr responseSender)
{
   std::wcout << "TestEntityOwner::OnCreateRequest() - called.. " << std::endl;
}

void
TestEntityOwner::OnUpdateRequest (Safir::Dob::EntityPtr entity, Safir::Dob::ResponseSenderPtr responseSender)
{
   std::wcout << "TestEntityOwner::OnUpdateRequest() - called.. " << std::endl;
}

void
TestEntityOwner::OnDeleteRequest (const Safir::Dob::Typesystem::ObjectId &objectId, Safir::Dob::ResponseSenderPtr responseSender)
{
   std::wcout << "TestEntityOwner::OnDeleteRequest() - called.. " << std::endl;
}


int
TestEntityOwner::handle_timeout (const ACE_Time_Value &current_time,
                                const void* arg)
{
   std::wcout << "TestEntityOwner::handle_timeout() - ACE_Thread::self(): " << ACE_Thread::self()
              << ". current_time: " << current_time.sec()
              << ". arg: " << arg
              << std::endl;

   return 0;
}

int
TestEntityOwner::handle_input (ACE_HANDLE handle)
{

//    std::wcout << "TestEntityOwner::handle_input() - ACE_Thread::self(): " << ACE_Thread::self()
//               << ". handle: " << handle
//               << std::endl;

   m_connection.Dispatch();

   return 0;
}


void
TestEntityOwner::Connect()
{
   std::wcout << "TestEntityOwner::Connect() - called. tid:" << ACE_Thread::self() << std::endl;

   m_connection.Open(L"TestEntityOwner",
                     L"0",
                     0,
                     this,
                     this);
}


void
TestEntityOwner::Status()
{
   std::wcout << "TestEntityOwner::Status() - Open: " << m_connection.IsOpen() <<  std::endl;

   if (m_connection.IsOpen())
      std::wcout << "TestEntityOwner::Status() - Name: " << m_connection.GetConnectionName() << std::endl;

}

void
TestEntityOwner::RegisterOwner()
{
   m_connection.RegisterEntity(Safir::Dob::Typesystem::ObjectId(DoseTest::GlobalEntity::ClassTypeId,
                                                                Safir::Dob::Typesystem::WHOLE_CLASS),
                               true,
                               this);
}


void
TestEntityOwner::Subscribe()
{
   m_connection.SubscribeMessage(Safir::Dob::Typesystem::ObjectId(DoseTest::GlobalMessage::ClassTypeId,
                                                                  Safir::Dob::Typesystem::WHOLE_CLASS),
                                 this);

}



// ----------------------------------------------------------------------

// Main entry
int main(int /*argc*/, char** /*argv*/)
{
   std::wcout << "main() - TestEntityOwner starting..." << std::endl;

   ACE_Reactor reactor;
   TestEntityOwner c(&reactor);

   // Setup Notify event handler
   reactor.register_handler(&c, ACE_Event_Handler::READ_MASK);

   // Make connection
   c.Connect();

   c.Status();
   c.RegisterOwner();
   c.Subscribe();

   // Loop "forever"...
   reactor.run_reactor_event_loop();

   std::wcout << "main() - TestEntityOwner stopping..." << std::endl;

   return 0;
}
