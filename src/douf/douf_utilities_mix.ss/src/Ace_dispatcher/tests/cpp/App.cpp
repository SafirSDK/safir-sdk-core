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
#if defined(_WIN32) || defined(__WIN32) || defined(__WIN32__)
#ifdef _WIN32_WINNT
#undef _WIN32_WINNT
#endif
#define _WIN32_WINNT 0x0501
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN    
#endif
#endif

#include <iostream>
#include <vector>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <Safir/Application/ResourceHandler.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Utilities/TestDispatcher.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Application/AceReactorEventLoop.h>

#include "app.h"

App::App(boost::shared_ptr<Safir::Application::AceReactorEventLoop> eventLoop): 
            Safir::Application::BusinessApplication(eventLoop),
            m_dispatcher(m_dobConnection,eventLoop->Reactor())
{
    m_State = Connected;
    m_dobConnection.Open(L"DispatcherTest", L"0", 0 ,this, &m_dispatcher );
}

void App::Startup()
{
    Safir::Utilities::TestDispatcherPtr test = Safir::Utilities::TestDispatcher::Create();
    m_dobConnection.RegisterEntityHandler(Safir::Utilities::TestDispatcher::ClassTypeId, Safir::Dob::Typesystem::HandlerId(), Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId, this);
    m_dobConnection.SubscribeEntity(Safir::Utilities::TestDispatcher::ClassTypeId, true, true, true, this);
    m_dobConnection.SetAll(test, Safir::Dob::Typesystem::InstanceId(0), Safir::Dob::Typesystem::HandlerId());
    ACE_Reactor::instance()->schedule_timer(this, 0, ACE_Time_Value( 0, 500000 /* 500 millisec */));
}

void App::CloseDown()
{
    if (m_State != Dispatched)
        m_State = Error;
}
    
void App::OnAddResources()
{        
};

void App::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                         Safir::Dob::ResponseSenderPtr responseSender)
{
    responseSender->Send(Safir::Dob::ErrorResponse::Create());
}

void App::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                         Safir::Dob::ResponseSenderPtr responseSender)
{
    responseSender->Send(Safir::Dob::ErrorResponse::Create());
}

void App::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityReqeustProxy,
                                         Safir::Dob::ResponseSenderPtr responseSender)
{
    responseSender->Send(Safir::Dob::ErrorResponse::Create());
}

void App::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId,const Safir::Dob::Typesystem::HandlerId &handlerId)
{
}

void App::OnNewEntity(const Safir::Dob::EntityProxy entityProxy) 
{
    m_State = Dispatched;
    Terminate();
}

void App::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{
    m_State = Dispatched;
    Terminate();
}

void App::OnDeletedEntity( const Safir::Dob::EntityProxy /*entityProxy*/, const bool /*deletedByOwner*/)
{
}

int App::handle_timeout(const ACE_Time_Value&, const void *)
{
    m_State = Error;
    Terminate();
    return 0;
}

void App::Test()
{
    Run();

    if (m_State == Dispatched)
    {
        std::wcout << "OK" << std::endl;
    }
    else
    {
        std::wcout << "NOK" << L" State = " << ToString(m_State) << std::endl;
    }
}

