/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
#ifndef __DOPE_APP_H__
#define __DOPE_APP_H__

#include <Safir/Application/Backdoor.h>
#include <Safir/Application/BackdoorKeeper.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ResponseSender.h>
#include "PersistenceHandler.h"
#include <Safir/Application/Tracer.h>
#include <ace/Reactor.h>
#include <Safir/Utilities/AceDispatcher.h>

class DopeApp :
    public Safir::Dob::StopHandler,
    public Safir::Dob::EntityHandlerPending,
    public Safir::Dob::EntitySubscriber,
    public Safir::Application::Backdoor
{
public:
    /**
     * Constructor
     */
    DopeApp();

    /**
     * Destructor
     */
    virtual ~DopeApp();

    void Run();

private:
    /** Implements Safir::Dob::StopHandler. */
    virtual void  OnStopOrder();

    /** Implements Safir::Application::Backdoor. */
    virtual void HandleCommand(const std::vector<std::wstring>& cmdTokens);

    /** Implements Safir::Application::Backdoor. */
    virtual std::wstring GetHelpText();

    /** Implements Safir::Dob::EntityHandler. */
    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
        const Safir::Dob::Typesystem::HandlerId& handlerId);

    virtual void OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
        const Safir::Dob::Typesystem::HandlerId& handlerId);

    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender);

    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender);

    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender);


    /** Implements Safir::Dob::EntitySubscriber. */
    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy);
    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy){};
    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool ){};


    void StartUp(bool restore);

    Safir::Dob::Connection  m_dobConnection;
    Safir::Utilities::AceDispatcher m_dispatcher;
    Safir::Dob::Typesystem::HandlerId m_handlerId;
    Safir::Dob::Typesystem::InstanceId m_instanceId;

    bool m_persistenceStarted; // Any dope has started successfully and loaded persistent data into the system.

    Safir::Application::BackdoorKeeper m_keeper;

    boost::shared_ptr<PersistenceHandler> m_persistenceHandler;

    Safir::Application::Tracer m_debug;
};

#endif
