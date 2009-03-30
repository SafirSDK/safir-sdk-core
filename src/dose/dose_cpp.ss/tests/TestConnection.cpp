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

#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/lexical_cast.hpp>
#include <iostream>
#include <ace/OS_NS_unistd.h>
#pragma warning (disable: 4100)
class Owner :
   public Safir::Dob::StopHandler,
   public Safir::Dob::Dispatcher,
   public Safir::Dob::EntityHandlerInjection,
   public Safir::Dob::MessageSubscriber,
   public Safir::Dob::ServiceHandler
{
public:
    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                       const Safir::Dob::Typesystem::HandlerId& handlerId) {}

    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender){}

    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender){}

    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender){}

    virtual void OnMessage(const Safir::Dob::MessageProxy messageProxy) {}


    virtual void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
        Safir::Dob::ResponseSenderPtr         responseSender) {}

   // Overrides Safir::Dob::StopHandler
   void  OnStopOrder() {}

   // Overrides Safir::Dob::Dispatcher
   void OnDoDispatch() {}

};

// ----------------------------------------------------------------------



// ----------------------------------------------------------------------

// Main entry
int main(int /*argc*/, char** /*argv*/)
{
   std::wcout << "TestConnection starting..." << std::endl;
   srand((unsigned int)time(NULL));
   std::list<Safir::Dob::Connection*> conns;
   std::list<Owner*> owners;
   for(int i = 0; i < 20; ++i)
   {
       conns.push_back(new Safir::Dob::Connection());
       owners.push_back(new Owner());
       conns.back()->Open(L"TestConnection",boost::lexical_cast<std::wstring>(rand()),0,owners.back(),owners.back());
       Safir::Dob::Typesystem::HandlerId h(rand());
       std::wcout << "Using handlerId = " << h << std::endl;

       conns.back()->RegisterEntityHandlerInjection(Safir::Dob::Entity::ClassTypeId, Safir::Dob::Typesystem::HandlerId(rand()),
           Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,owners.back());
if (i == 19)
{
    terminate();
}

       conns.back()->SubscribeMessage(Safir::Dob::Message::ClassTypeId, Safir::Dob::Typesystem::ChannelId(rand()),
                                      owners.back());

       conns.back()->RegisterServiceHandler(Safir::Dob::Service::ClassTypeId, h,
                                            owners.back());

       std::wcout << "TestConnection sleeping..." << std::endl;
       ACE_OS::sleep(0);
   }
   ACE_OS::sleep(500);
   return 0;
}
