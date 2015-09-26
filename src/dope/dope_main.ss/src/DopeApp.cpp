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
#include "DopeApp.h"

#include <Safir/Dob/PersistenceParameters.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include "FilePersistor.h"
#include "NonePersistor.h"
#include <Safir/Logging/Log.h>
#include <Safir/Application/CrashReporter.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/PersistentDataStatus.h>
#include <clocale>

#ifdef _MSC_VER
#  pragma warning(push)
#  pragma warning (disable: 4702)
#  pragma warning (disable: 4267)
#  pragma warning (disable: 4100)
#endif

#include <boost/lexical_cast.hpp>
#include <boost/thread.hpp>

#ifdef _MSC_VER
#  pragma warning(pop)
#endif

#ifndef NO_DATABASE_SUPPORT
#  include "OdbcPersistor.h"
#endif


//Dope uses context 0 to connect to the dob. The strange looking negative number
//is a way to indicate that this is a connection with special privileges.
const Safir::Dob::Typesystem::Int32 PERSISTENCE_CONTEXT = -1000000;

//-------------------------------------------------------
DopeApp::DopeApp():
    m_dispatcher(m_dobConnection, m_ioService),
    m_persistenceStarted(false),
    m_persistenceInitialized(false),
    m_keeper(m_dobConnection),
    m_debug(L"DopeApp")
{
    try
    {
        m_dobConnection.Open(L"DOPE", L"0", PERSISTENCE_CONTEXT, this, &m_dispatcher);
        m_debug << "Opened DOB connection"<<std::endl;
    }
    catch (Safir::Dob::NotOpenException e)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Critical,
                                      L"Failed to connect to Dob. Maybe Dope is already started on this node.");
        throw StartupError();
    }

    m_instanceId = Safir::Dob::Typesystem::InstanceId
        (Safir::Dob::ConnectionAspectMisc(m_dobConnection).GetNodeId());

    Safir::Application::TracerBackdoor::Start(m_dobConnection);
    m_keeper.Start(*this);
    m_debug << "Started keeper"<<std::endl;

    // Start pending registration to try be the active dope.
    m_dobConnection.RegisterEntityHandlerPending(Safir::Dob::PersistentDataStatus::ClassTypeId,
        m_handlerId,
        Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
        this);

    // Subscribe to see which dope got the active state.
    m_dobConnection.SubscribeEntity(Safir::Dob::PersistentDataStatus::ClassTypeId, false, false, false, this);


}

//-------------------------------------------------------
DopeApp::~DopeApp()
{
    Safir::Application::TracerBackdoor::Stop();
}


//-------------------------------------------------------
void DopeApp::OnStopOrder()
{
    m_debug << "Got Stop order, will terminate"<< std::endl;
    m_ioService.stop();
}

//-------------------------------------------------------
void DopeApp::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId /*typeId*/,
                                    const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
{
    throw Safir::Dob::Typesystem::SoftwareViolationException
        (L"Dope got an OnRevokedRegistration! Something is wrong with your configuration!",
         __WFILE__,__LINE__);
}

void DopeApp::OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                      const Safir::Dob::Typesystem::HandlerId& )
{
    if (typeId == Safir::Dob::PersistentDataStatus::ClassTypeId)
    {
        if (!m_persistenceStarted)
        {
            std::wcout << L"Dope is starting as new active persistence."  << std::endl;
            StartUp(true);
            m_debug << L"Start completed."  << std::endl;
            m_persistenceStarted = true;
        }
        else
        {
            std::wcout << L"Dope is taking over the persistence."  << std::endl;
            StartUp(false);
            m_debug << L"Take over completed."  << std::endl;
        }
    }
}

void DopeApp::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                              Safir::Dob::ResponseSenderPtr        responseSender)
{
    responseSender->Send(Safir::Dob::ErrorResponse::Create());
};

void DopeApp::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                              Safir::Dob::ResponseSenderPtr        responseSender)
{
    responseSender->Send(Safir::Dob::ErrorResponse::Create());
};

void DopeApp::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                              Safir::Dob::ResponseSenderPtr        responseSender)
{
    responseSender->Send(Safir::Dob::ErrorResponse::Create());
};

//-------------------------------------------------------

void DopeApp::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    if (entityProxy.GetTypeId() == Safir::Dob::PersistentDataStatus::ClassTypeId)
    {
        if (!m_persistenceStarted)
        {
            std::wcout << L"Dope is started as standby persistence. Active persistence is running on node ";
            std::wcout << entityProxy.GetInstanceId().GetRawValue() << "." << std::endl;
            m_persistenceStarted = true;
        }
    }
}

//-------------------------------------------------------
void DopeApp::Start(bool restore)
{
    m_debug << "Start(" << restore << ")."<< std::endl;
    if (!m_persistenceInitialized)
    {
        switch (Safir::Dob::PersistenceParameters::Backend())
        {
        case Safir::Dob::PersistenceBackend::None:
            {
                m_debug << "Using 'None' persistence" << std::endl;
                m_persistenceHandler.reset(new NonePersistor(m_ioService));
            }
            break;
        case Safir::Dob::PersistenceBackend::File:
            {
                m_debug << "Using file persistence" << std::endl;
                m_persistenceHandler.reset(new FilePersistor(m_ioService));
            }
            break;

        case Safir::Dob::PersistenceBackend::Odbc:
            {
                m_debug << "Using database persistence" << std::endl;

                //Mimer requires locale to be set like this for character conversions
                //to work properly. Hopefully this does not adversely affect other
                //databases.
                std::setlocale(LC_CTYPE, "");

#ifndef NO_DATABASE_SUPPORT
                m_persistenceHandler.reset(new OdbcPersistor(m_ioService));
#endif
            }
            break;

        default:
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Unknown backend!",__WFILE__,__LINE__);
        }
        m_persistenceInitialized = true;
    }

    m_persistenceHandler->Start(restore);
}


//-------------------------------------------------------
void DopeApp::StartUp(bool restore)
{
    Start(restore);

    if (m_thread.get_id() != boost::thread::id())
    {
        // wait for ok to connect on context 0
        m_thread = boost::thread(boost::bind(&DopeApp::ConnectionThread, this));
    }
}

//-------------------------------------------------------
void
DopeApp::HandleCommand(const std::vector<std::wstring>& cmdTokens)
{
    if (cmdTokens[0] == L"ShowBackend")
    {
        m_debug << "Using backend: " << Safir::Dob::PersistenceBackend::ToString(Safir::Dob::PersistenceParameters::Backend()) <<std::endl;
    }
}

//-------------------------------------------------------
std::wstring
DopeApp::GetHelpText()
{
    return L"ShowBackend - Show which backend is used";
}


void
DopeApp::Run()
{
    boost::asio::io_service::work keepRunning(m_ioService);
    m_ioService.run();

    if (m_persistenceHandler != nullptr)
    {
        m_persistenceHandler->Stop();
    }
}
//-------------------------------------------------------
/*
 dummy dispather
*/
class DummyDispatcher:
    public Safir::Dob::Dispatcher
{
    virtual void OnDoDispatch() override {}
};

/*
 Just wait for ok to connect on context 0
*/
void DopeApp::ConnectionThread()
{
    DummyDispatcher dispatcher;
    Safir::Dob::Connection tmpConnection;

    try
    {
        tmpConnection.Open(L"DOPE_TMP",L"0",0,nullptr,&dispatcher);
        m_ioService.post([this]
                         {
                             SignalOkToConnect(true);
                         });
    }
    catch (Safir::Dob::NotOpenException e)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Critical,
                                      L"NotOpenException in ConnectionThread!!!");
    }
    catch (const std::exception & e)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Critical,
                                      L"Unhandled exception in ConnectionThread: " +
                                      Safir::Dob::Typesystem::Utilities::ToWstring(e.what()));
    }
    catch (...)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Critical,
                                      L"Unhandled '...' exception in ConnectionThread");
    }
    m_ioService.post(boost::bind(&DopeApp::SignalOkToConnect,this, false));
}

void DopeApp::SignalOkToConnect(bool ok)
{
    if (ok)
    {
        // use a different handler to register PersistentDataStatus to get the correct registration time.
        Safir::Dob::Typesystem::HandlerId entityHandler(L"DOPE_ENTITY_HANDLER");

        m_dobConnection.RegisterEntityHandler(Safir::Dob::PersistentDataStatus::ClassTypeId,
                                              entityHandler,
                                              Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                              this);

        Safir::Dob::PersistentDataStatusPtr status = Safir::Dob::PersistentDataStatus::Create();
        status->State().SetVal( Safir::Dob::PersistentDataState::Started);
        m_dobConnection.SetAll(status, m_instanceId, entityHandler);

        m_thread.join();
        m_thread = boost::thread();
    }
    else
    {
        m_thread.join();
        m_thread = boost::thread();

        //not ok
        throw StartupError();
    }
}
