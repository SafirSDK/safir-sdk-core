/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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

#include "app.h"
#include <DoseTest/SynchronousVolatileEntity.h>
#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/ThisNodeParameters.h>

#include <boost/bind.hpp>

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4702)
#endif

#include <boost/lexical_cast.hpp>
#include <ace/Thread.h>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif

#include <iostream>

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4355)
#endif
App::App() :
    m_dispatcher(boost::bind(&App::DispatchConnection, this)),
    m_connectionInst(0),
    m_typeId(DoseTest::SynchronousVolatileEntity::ClassTypeId),
    m_instanceIdPolicy(Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId),
    m_activeDurationTimerId(0),
    m_setDeleteTimerId(1),
    m_regUnregTimerId(2),
    m_counter(0),
    m_handlerInterval(0, 9),
    m_randomHandler(m_rng, m_handlerInterval),
    m_instanceInterval(0, 9),
    m_randomInstance(m_rng, m_instanceInterval),
    m_actionInterval(1, 2),
    m_randomAction(m_rng, m_actionInterval),
    m_setAllowed(10, false)
{
    m_connection.Open(L"SplitJoinStresser", boost::lexical_cast<std::wstring>(m_connectionInst), 0, this, &m_dispatcher);
}
    #ifdef _MSC_VER
  #pragma warning(pop)
#endif

void App::Run(const int activeDuration)
{
    ACE_Reactor::instance()->owner(ACE_Thread::self());

    if (activeDuration > 0)
    {
        ACE_Time_Value time(activeDuration, 0);

        m_activeDurationTimerId = ACE_Reactor::instance()->schedule_timer(this,
                                                                          0,
                                                                          time);
    }

    ACE_Time_Value setDeleteInterval(0,10000); //10 ms
    m_setDeleteTimerId = ACE_Reactor::instance()->schedule_timer(this,
                                                                 (void*)1,  // Set/Delete timer
                                                                 setDeleteInterval,
                                                                 setDeleteInterval);

    ACE_Time_Value regUnregInterval(5,0); // 5 sec
    m_regUnregTimerId = ACE_Reactor::instance()->schedule_timer(this,
                                                                (void*)2,  // Register/Unregister timer
                                                                regUnregInterval,
                                                                regUnregInterval);

    ACE_Reactor::instance()->run_reactor_event_loop();
}

void App::OnStopOrder()
{
    std::wcout << "Got stop order!" << std::endl;
    exit(0);
}

void App::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                const Safir::Dob::Typesystem::HandlerId& handlerId)
{
    std::wcout << "OnRevokedRegistration for TypeId=" << Safir::Dob::Typesystem::Operations::GetName(typeId)
               << " HandlerId=" << handlerId << std::endl;

    m_setAllowed.at((unsigned int)handlerId.GetRawValue()) = false;

    ACE_Reactor::instance()->cancel_timer(m_setDeleteTimerId);

    m_connection.RegisterEntityHandlerPending(typeId,
                                              handlerId,
                                              m_instanceIdPolicy,
                                              this);
}
    
void App::OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                  const Safir::Dob::Typesystem::HandlerId& handlerId)
{
    std::wcout << "OnCompletedRegistration for TypeId=" << Safir::Dob::Typesystem::Operations::GetName(typeId)
               << " HandlerId=" << handlerId << std::endl;
}

void App::OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId       typeId,
                                  const Safir::Dob::Typesystem::HandlerId&   handlerId)
{
    std::wcout << "OnInitialInjectionsDone for TypeId=" << Safir::Dob::Typesystem::Operations::GetName(typeId)
               << " HandlerId=" << handlerId << std::endl;

    m_setAllowed.at((unsigned int)handlerId.GetRawValue()) = true;
}

int App::handle_timeout (const ACE_Time_Value&, const void* arg)
{
    if (arg == (void*)0)
    {
        std::wcout << "Stopping reg/unreg/set/delete actions!" << std::endl;

        ACE_Reactor::instance()->cancel_timer(m_setDeleteTimerId);
        ACE_Reactor::instance()->cancel_timer(m_regUnregTimerId);
    }
    else if (arg == (void*)1)
    {
        // Set/delete timer

        int action = m_randomAction();
        int handler = m_randomHandler();
        Safir::Dob::Typesystem::InstanceId instanceId = Safir::Dob::Typesystem::InstanceId(m_randomInstance() + handler * 1000);
        Safir::Dob::Typesystem::HandlerId handlerId = Safir::Dob::Typesystem::HandlerId(handler);

        try
        {
            switch (action)
            {
            case 1:
                {
                    DoseTest::SynchronousVolatileEntityPtr entity = DoseTest::SynchronousVolatileEntity::Create();
                    entity->Info().SetVal(boost::lexical_cast<std::wstring>(m_counter));
                    entity->MoreInfo().SetVal(boost::lexical_cast<std::wstring>(Safir::Dob::ThisNodeParameters::NodeNumber()));
                    
                    if (m_setAllowed.at((unsigned int)handlerId.GetRawValue()))
                    {
                        m_connection.SetAll(entity, instanceId, handlerId);

                        //std::wcout << "SetAll, InstanceId=" << instanceId << ", TypeId=" << Safir::Dob::Typesystem::Operations::GetName(m_typeId)
                        //           << " HandlerId=" << handlerId << std::endl;
                    }
                }
                break;

            case 2:
                {
                    if (m_setAllowed.at((unsigned int)handlerId.GetRawValue()))
                    {
                        m_connection.Delete(Safir::Dob::Typesystem::EntityId(m_typeId, instanceId), handlerId);

                        //std::wcout << "Delete, InstanceId=" << instanceId << ", TypeId=" << Safir::Dob::Typesystem::Operations::GetName(m_typeId)
                        //           << " HandlerId=" << handlerId << std::endl;
                    }
                }
                break;

            default:
                {
                    std::wcout << "Unknown action!" << std::endl;
                }
            }
        }
        catch (Safir::Dob::AccessDeniedException&)
        {
            std::wcout << "Tried to access an instance owned by another handler!" << std::endl;
        }    
    }
    else if (arg == (void*)2)
    {
        //Register/unregister timer

        int action = m_randomAction();
        Safir::Dob::Typesystem::HandlerId handlerId = Safir::Dob::Typesystem::HandlerId(m_randomHandler());

        try
        {
            switch (action)
            {
            case 1:
                {
                    m_connection.RegisterEntityHandlerPending(m_typeId, handlerId, m_instanceIdPolicy, this);

                    std::wcout << "RegisterEntityHandlerPending, TypeId=" << Safir::Dob::Typesystem::Operations::GetName(m_typeId)
                               << " HandlerId=" << handlerId << std::endl;
                }
                break;

            case 2:
                {

                    m_connection.UnregisterHandler(m_typeId, handlerId);
                    m_setAllowed.at((unsigned int)handlerId.GetRawValue()) = false;

                    std::wcout << "UnregisterHandler, TypeId=" << Safir::Dob::Typesystem::Operations::GetName(m_typeId)
                               << " HandlerId=" << handlerId << std::endl;

                }
                break;

            default:
                {
                    std::wcout << "Unknown action!" << std::endl;
                }
            }
        }
        catch (Safir::Dob::AccessDeniedException&)
        {
            std::wcout << "Tried to access an instance owned by another handler!" << std::endl;
        }    
    }

    ++m_counter;

    return 0;
}
    
void App::OnCreateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
                          Safir::Dob::ResponseSenderPtr        /*responseSender*/)
{
    std::wcout << "Ignoring OnCreateRequest!" << std::endl; 
}
    
void App::OnUpdateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
                          Safir::Dob::ResponseSenderPtr        /*responseSender*/)
{
    std::wcout << "Ignoring OnUpdateRequest!" << std::endl; 
}
    
void App::OnDeleteRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
                          Safir::Dob::ResponseSenderPtr        /*responseSender*/)
{
    std::wcout << "Ignoring OnDeleteRequest!" << std::endl; 
}
