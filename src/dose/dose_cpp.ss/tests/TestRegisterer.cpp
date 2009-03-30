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
// Test program for a registerer
//

#include <DoseTest/GlobalMessage.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ConnectionMiscAspect.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Typesystem/Serialization.h>

#include <iostream>
#include <sstream>

#include <ace/OS_NS_unistd.h>
#include <ace/Event_Handler.h>
#include <ace/Reactor.h>
#include <ace/Thread.h>
#include <boost/lexical_cast.hpp>

class Registerer :
   public Safir::Dob::StopHandler,
   public Safir::Dob::Dispatcher,
   public Safir::Dob::ServiceHandler,
   public ACE_Event_Handler
{
public:
    Registerer(ACE_Reactor* reactor) : ACE_Event_Handler(reactor) {};
   ~Registerer();


   // Overrides Safir::Dob::StopHandler
   void  OnStopOrder() {reactor()->end_reactor_event_loop();}

   // Overrides Safir::Dob::Dispatcher
   void OnDoDispatch() {reactor()->notify(this,ACE_Event_Handler::READ_MASK);}

   virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId&    typeId,
                                      const Safir::Dob::Typesystem::HandlerId& handlerId);

   virtual void OnServiceRequest(Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                 Safir::Dob::ResponseSenderPtr   responseSender);


   // Tests
   void Connect();
   void Register(const Safir::Dob::Typesystem::TypeId&     typeId,
                 const Safir::Dob::Typesystem::HandlerId&  handlerId);

   int handle_input (ACE_HANDLE /*fd*/) {m_connection.Dispatch(); return 0;}

private:
   Safir::Dob::Connection m_connection;

};

// ----------------------------------------------------------------------

Registerer::~Registerer()
{
    if (m_connection.IsOpen())
    {
        m_connection.Close();
    }
}



void
Registerer::Connect()
{
   m_connection.Open(L"TestRegisterer",
                     L"0",
                     0,
                     this,
                     this);
}

void
Registerer::Register(const Safir::Dob::Typesystem::TypeId&     typeId,
                     const Safir::Dob::Typesystem::HandlerId&  handlerId)
{
    std::wcout << "Register handler: " << handlerId << " for type: " << Safir::Dob::Typesystem::Operations::GetName(typeId) << std::endl;

    m_connection.RegisterServiceHandler(typeId,
                                        handlerId,
                                        this);
}

void Registerer::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId&     typeId,
                                       const Safir::Dob::Typesystem::HandlerId&  handlerId)
{
    std::wcout << "OnRevokedRegistration received for type: " << Safir::Dob::Typesystem::Operations::GetName(typeId)
               << "; handlerId: " << handlerId << std::endl;
}

void Registerer::OnServiceRequest(Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                  Safir::Dob::ResponseSenderPtr   responseSender)
{
    std::wcout << "OnServiceRequest received " << std::endl;
}

// ----------------------------------------------------------------------

// Main entry
int main(int /*argc*/, char** /*argv*/)
{
   std::wcout << "TestRegisterer starting..." << std::endl;

   ACE_Reactor reactor;
   Registerer registerer(&reactor);

   // Setup Notify event handler
   reactor.register_handler(&registerer, ACE_Event_Handler::READ_MASK);

   registerer.Connect();

   registerer.Register(Safir::Dob::Typesystem::Operations::GetTypeId(L"DoseTest.GlobalService"), Safir::Dob::Typesystem::HandlerId(L"GLOBAL_SERVICE_HANDLER_0"));
   registerer.Register(Safir::Dob::Typesystem::Operations::GetTypeId(L"DoseTest.GlobalService"), Safir::Dob::Typesystem::HandlerId(L"GLOBAL_SERVICE_HANDLER_1"));
   registerer.Register(Safir::Dob::Typesystem::Operations::GetTypeId(L"DoseTest.GlobalService"), Safir::Dob::Typesystem::HandlerId(L"GLOBAL_SERVICE_HANDLER_2"));

   // Loop "forever"...
   reactor.run_reactor_event_loop();

   return 0;

}

