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
#include "DopeApp.h"

#include <Safir/Dob/PersistenceParameters.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include "FilePersistor.h"
#include <Safir/SwReports/SwReport.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/PersistentDataStatus.h>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4702)
#pragma warning(disable: 4267)
#endif

#include <boost/lexical_cast.hpp>
#include <ace/Thread.h>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#ifndef NO_DATABASE_SUPPORT
#include "OdbcPersistor.h"
#endif


//Dope uses context 0 to connect to the dob. The strange looking negative number
//is a way to indicate that this is a connection with special privileges.
const Safir::Dob::Typesystem::Int32 PERSISTANCE_CONTEXT = -1000000;

//-------------------------------------------------------
DopeApp::DopeApp():
    m_dispatcher(m_dobConnection),
    m_instanceId(Safir::Dob::ThisNodeParameters::NodeNumber()),
    m_persistenceStarted(false),
    m_debug(L"DopeApp")
{
    //perform sanity check!
    if (!Safir::Dob::PersistenceParameters::SystemHasPersistence())
    {
        //std::wcout << "DOPE was started even though Safir.Dob.PersistenceParameters.SystemHasPersistence is set to false. Please check your configuration"<<std::endl;
        Safir::SwReports::SendFatalErrorReport
            (L"Bad Configuration",L"DopeApp::DopeApp",
             L"DOPE was started even though Safir.Dob.PersistenceParameters.SystemHasPersistence is set to false. Please check your configuration");
        Safir::SwReports::Stop();
        exit(-1);
    }

    try
    {
        m_dobConnection.Open(L"DOPE", L"0", PERSISTANCE_CONTEXT, this, &m_dispatcher);
        m_debug << "Opened DOB connection"<<std::endl;
    }
    catch (Safir::Dob::NotOpenException e)
    {
        Safir::SwReports::SendFatalErrorReport
            (L"Failed to connect to DOB.",L"DopeApp::DopeApp",
            L"Maybe DOPE is already started on this node.");
        exit(-1);
    }

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

}


//-------------------------------------------------------
void DopeApp::OnStopOrder()
{
    m_debug << "Got Stop order, will terminate"<< std::endl;
    ACE_Reactor::instance()->end_reactor_event_loop();
}

//-------------------------------------------------------
void DopeApp::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                    const Safir::Dob::Typesystem::HandlerId& )
{
    Safir::SwReports::SendErrorReport
        (L"OnRevokedRegistration",L"DopeApp::DopeApp",
        std::wstring(L"Revoked registration : ") + Safir::Dob::Typesystem::Operations::GetName(typeId) );
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
            std::wcout << L"Start completed."  << std::endl;
            m_persistenceStarted = true;
        } 
        else
        {
            std::wcout << L"Dope is taking over the persistence."  << std::endl;
            StartUp(false);
            std::wcout << L"Take over completed."  << std::endl;
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

void DopeApp::StartUp(bool restore)
{
    //Safir::Dob::PersistentDataStatusPtr status = Safir::Dob::PersistentDataStatus::Create();
    //status->State().SetVal( Safir::Dob::PersistentDataState::Init);
    //m_dobConnection.SetAll(status, m_instanceId, m_handlerId);

    switch (Safir::Dob::PersistenceParameters::Backend())
    {
    case Safir::Dob::PersistenceBackend::File:
        {
            m_debug << "Using file persistence" << std::endl;
            m_persistenceHandler.reset(new FilePersistor());
        }
        break;

    case Safir::Dob::PersistenceBackend::Odbc:
        {
            m_debug << "Using database persistence" << std::endl;
#ifndef NO_DATABASE_SUPPORT
            m_persistenceHandler.reset(new OdbcPersistor());
#endif
        }
        break;

    default:
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Unknown backend!",__WFILE__,__LINE__);
    }

    m_persistenceHandler->Start(restore);

    // waith for ok to connect on context 0
    ACE_Thread::spawn(&DopeApp::ConnectionThread,this);
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
    ACE_Reactor::instance()->run_reactor_event_loop();
}
//-------------------------------------------------------
/*
 dummy dispather
*/
class DummyDispatcher:
    public Safir::Dob::Dispatcher
{
    virtual void OnDoDispatch() {}
};

/*
 Just wait for ok to connect on context 0
*/
ACE_THR_FUNC_RETURN DopeApp::ConnectionThread(void * _this)
{
    DopeApp * This = static_cast<DopeApp *>(_this);
    DummyDispatcher dispatcher;
    Safir::Dob::Connection tmpConnection;

    tmpConnection.Open(L"DOPE_TMP",L"0",0,NULL,&dispatcher);
    ACE_Reactor::instance()->notify(This,ACE_Event_Handler::READ_MASK);

    return NULL;
}

int DopeApp::handle_input(ACE_HANDLE)
{
    // use a different hander to overregister PersistentDataStatus to get the correct registration time.
    Safir::Dob::Typesystem::HandlerId entityHandler(L"DOPE_ENTITY_HANDLER"); 

    m_dobConnection.RegisterEntityHandler(Safir::Dob::PersistentDataStatus::ClassTypeId, 
        entityHandler,
        Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
        this);

    Safir::Dob::PersistentDataStatusPtr status = Safir::Dob::PersistentDataStatus::Create();
    status->State().SetVal( Safir::Dob::PersistentDataState::Started);
    m_dobConnection.SetAll(status, m_instanceId, entityHandler);


    return 0;
}
