/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
#ifndef __AFWK_TEST_APP_H
#define __AFWK_TEST_APP_H

#include <Safir/Application/AceReactorEventLoop.h>
#include <Safir/Application/BusinessApplication.h>
#include <Safir/Utilities/AceDispatcher.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/Connection.h>

#include "States.h"

class App : public Safir::Application::BusinessApplication, 
            public Safir::Dob::EntityHandler,
            public Safir::Dob::EntitySubscriber,
            public ACE_Event_Handler,
            public Safir::Dob::StopHandler
{
private:
    States m_State;
    Safir::Dob::Connection m_dobConnection;
    Safir::Utilities::AceDispatcher m_dispatcher;

public:
    App(boost::shared_ptr<Safir::Application::AceReactorEventLoop> eventLoop);

    void Startup();
    void CloseDown();
    void OnAddResources();
    States State() {return m_State;};
    void OnStopOrder() {};

    int handle_timeout (const ACE_Time_Value&, const void *);


void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                         Safir::Dob::ResponseSenderPtr responseSender);
void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                         Safir::Dob::ResponseSenderPtr responseSender);

void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityReqeustProxy,
                                         Safir::Dob::ResponseSenderPtr responseSender);
void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId,const Safir::Dob::Typesystem::HandlerId &handlerId);
void OnNewEntity(const Safir::Dob::EntityProxy entityProxy);
void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy);

void OnDeletedEntity( const Safir::Dob::EntityProxy /*entityProxy*/, const bool /*deletedByOwner*/);

/*
    void OnCreateRequest(Safir::Dob::EntityPtr entity,Safir::Dob::ResponseSenderPtr responseSender);
    void OnUpdateRequest(Safir::Dob::EntityPtr entity, Safir::Dob::ResponseSenderPtr responseSender);
    void OnDeleteRequest(const Safir::Dob::Typesystem::ObjectId& objectId, Safir::Dob::ResponseSenderPtr responseSender);
    void OnPersistentData(Safir::Dob::EntityPtr entity);
    void OnRegistrationStatus(const Safir::Dob::Typesystem::ObjectId & ObjId, const Safir::Dob::RegistrationStatus::Enumeration registrationStatus);

    void OnNewEntity(Safir::Dob::EntityPtr entity, bool);
    void OnUpdatedEntity(Safir::Dob::EntityPtr entity);
    void OnRemovedEntity(const Safir::Dob::Typesystem::ObjectId& objectId, bool deleted);
*/
    void Test();
};

#endif
