/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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
#pragma once

#include <Safir/Application/Backdoor.h>
#include <Safir/Application/BackdoorKeeper.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ResponseSender.h>
#include "PersistenceHandler.h"
#include <Safir/Application/Tracer.h>
#include <boost/bind.hpp>
#include <boost/thread.hpp>
#include <Safir/Utilities/AsioDispatcher.h>

class DopeApp :
    public Safir::Dob::StopHandler,
    public Safir::Dob::EntityHandlerPending,
    public Safir::Dob::EntityHandler,
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
    virtual void  OnStopOrder() override;

    /** Implements Safir::Application::Backdoor. */
    virtual void HandleCommand(const std::vector<std::wstring>& cmdTokens) override;

    /** Implements Safir::Application::Backdoor. */
    virtual std::wstring GetHelpText() override;

    /** Implements Safir::Dob::EntityHandler. */
    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
        const Safir::Dob::Typesystem::HandlerId& handlerId) override;

    virtual void OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
        const Safir::Dob::Typesystem::HandlerId& handlerId) override;

    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender) override;

    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender) override;

    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender) override;


    /** Implements Safir::Dob::EntitySubscriber. */
    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy) override;
    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy) override{};
    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool ) override{};


    void StartUp(bool restore);
    void Start(bool restore);

    boost::asio::io_service m_ioService;

    Safir::Utilities::AsioDispatcher m_dispatcher;
    Safir::Dob::Connection m_dobConnection;
    Safir::Dob::Typesystem::HandlerId m_handlerId;
    Safir::Dob::Typesystem::InstanceId m_instanceId;

    /*
    Dope connects on context -1, before all other apps.
    But to ensure that registration time is set correct we must wait with registration and entity(dope owned) creation
    until it is ok to connect as a normal app on context 0.
    The connection thread is waiting to it is ok to connect on context 0.
    Then we can register entityhander and create our entity.
    */
    void ConnectionThread();
    //Handler when ok to connect for applications.
    void SignalOkToConnect(bool ok);

    boost::thread m_thread;

    bool m_persistenceStarted; // Any dope has started successfully and loaded persistent data into the system.
    bool m_persistenceInitialized; // Dope has initialized persistence.

    Safir::Application::BackdoorKeeper m_keeper;

    boost::shared_ptr<PersistenceHandler> m_persistenceHandler;

    Safir::Application::Tracer m_debug;
};
