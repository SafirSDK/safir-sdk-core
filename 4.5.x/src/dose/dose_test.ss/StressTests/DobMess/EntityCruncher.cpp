/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
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
#include <boost/interprocess/detail/os_thread_functions.hpp>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/OverflowException.h>
#include <string>
#include <fstream>
#include <iostream>
#include <boost\asio.hpp>
#include "EntityCruncher.h"

#define TRACE_OUT(X) {if (m_verbose) std::cout<<X<<std::endl;}

EntityCruncher::EntityCruncher( int maxNumEntities, 
                                bool createEnabled, 
                                bool deleteEnabled, 
                                bool verbose) : 
                                                m_pid(0), 
                                                m_numberOfEntities(0),
                                                m_maxNumberOfEntities(maxNumEntities),
                                                m_entityCreateEnabled(createEnabled),
                                                m_entityDeleteEnabled(deleteEnabled),
                                                m_verbose(verbose)
{
    m_pid = boost::interprocess::detail::get_current_process_id();    
    std::wstringstream ss;
    ss<<L"EntityCruncher_"<<Safir::Dob::Typesystem::InstanceId::GenerateRandom().GetRawValue()<<"_pid_"<<m_pid;
    std::wcout<<std::boolalpha;
    std::wcout<<L"EntityCruncher"<<std::endl;
    std::wcout<<L"          handler:               "<<ss.str()<<std::endl;
    std::wcout<<L"          max entities:          "<<m_maxNumberOfEntities<<std::endl;
    std::wcout<<L"          entity create enabled: "<<m_entityCreateEnabled<<std::endl;
    std::wcout<<L"          entity delete enabled: "<<m_entityDeleteEnabled<<std::endl;
    std::wcout<<L"          verbose:               "<<m_verbose<<std::endl;
    std::wcout<<L"running..."<<std::endl;

    m_supervisor.Start();
    m_handler=Safir::Dob::Typesystem::HandlerId(ss.str());
    m_con.Open(ss.str(), L"", 0, this, this);
    m_con.RegisterEntityHandler(DoseStressTest::Ping::ClassTypeId, m_handler, Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId, this);
    m_con.SubscribeEntity(DoseStressTest::Ping::ClassTypeId, this);

    if (m_entityCreateEnabled)
    {
        SetTimer(CreateEntityTimerId, EntityShowerInterval, true);
    }
}


EntityCruncher::~EntityCruncher(void)
{
    TRACE_OUT("~EntityCruncher");
}

void EntityCruncher::DoDispatch()
{    
    m_supervisor.UpdateDispatchTimestamp(); 
    m_con.Dispatch();
}

void EntityCruncher::OnTimeout(int timerId)
{        
    if (timerId==CreateEntityTimerId)
    {
        if (m_entityDeleteEnabled && m_numberOfEntities>=m_maxNumberOfEntities) //delete all if max is reached and delete enabled
        {      
            TRACE_OUT("Deletnig all instances");            
            m_con.DeleteAllInstances(DoseStressTest::Ping::ClassTypeId, m_handler);            
            m_numberOfEntities=0;
        }

        for (int i=0; i<EntityShowerSize && m_numberOfEntities<m_maxNumberOfEntities; ++i) //create new instances as long as not max is reached
        {           
            DoseStressTest::PingPtr ping=DoseStressTest::Ping::Create();
            ping->Number()=0;
            m_con.SetAll(ping, Safir::Dob::Typesystem::InstanceId::GenerateRandom(), m_handler);
            ++m_numberOfEntities;
        }    
                
        TRACE_OUT("NumberOfEntities = "<<m_numberOfEntities);               
    }
    else
    {
        std::cout<<"Unknown timer, id="<<timerId<<std::endl;
    }
}

//StopHandler
void EntityCruncher::OnStopOrder()
{
    std::cout<<"Received Stop Order"<<std::endl;
    m_con.Close();
}

//EntityHandler
void EntityCruncher::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId& handlerId)
{
    std::cout<<"OnRevokedRegistration"<<std::endl;    
}
void EntityCruncher::OnCreateRequest(const Safir::Dob::EntityRequestProxy requestProxy,Safir::Dob::ResponseSenderPtr responseSender)
{
    std::cout<<"OnCreateRequest"<<std::endl;
    responseSender->Send(Safir::Dob::ErrorResponse::Create());
}
void EntityCruncher::OnUpdateRequest(const Safir::Dob::EntityRequestProxy requestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    //TRACE_OUT("OnUpdateRequest");
    m_con.SetChanges(requestProxy.GetRequest(), requestProxy.GetInstanceId(), m_handler);
    responseSender->Send(Safir::Dob::SuccessResponse::Create());  
}
void EntityCruncher::OnDeleteRequest(const Safir::Dob::EntityRequestProxy requestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    TRACE_OUT("OnDeleteRequest");
    if (m_entityDeleteEnabled) //only delete if delete is enabled
    {
        m_con.Delete(requestProxy.GetEntityId(), m_handler); 
        --m_numberOfEntities;
    }
    responseSender->Send(Safir::Dob::SuccessResponse::Create()); //always sends success response even if entity were not deleted   
}

//EntitySubscriber
void EntityCruncher::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    //TRACE_OUT("OnNewEntity");
    SendRequest(entityProxy);
}
void EntityCruncher::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{    
    //TRACE_OUT("OnUpdatedEntity");
    SendRequest(entityProxy);
}

void EntityCruncher::OnDeletedEntity(const Safir::Dob::EntityProxy /*entityProxy*/, const bool /*deletedByOwner*/)
{
    //TRACE_OUT("OnDeletedEntity");
    //Find out something really nasty to do here.
}

//Requestor
void EntityCruncher::OnResponse(const Safir::Dob::ResponseProxy /*responseProxy*/)
{
    //TRACE_OUT("OnResponse");
    //we dont care about the response
}
void EntityCruncher::OnNotRequestOverflow()
{   
    //TRACE_OUT("OnNotRequestOverflow");            
}

void EntityCruncher::SendRequest(const Safir::Dob::EntityProxy& entityProxy)
{  
    if (entityProxy.GetOwner() == m_handler)
    {
        return; //we dont send requests to usselves
    }

    DoseStressTest::PingPtr ping = boost::dynamic_pointer_cast<DoseStressTest::Ping>(entityProxy.GetEntity());
    ping->Number()++;
    if (ping->Number()<std::numeric_limits<int>::max()-1)
    {
        ping->Number()++;        
    }
    else
    {
        ping->Number()=0;
    }

    try
    {
        m_con.UpdateRequest(ping, entityProxy.GetInstanceId(), this);

        /*if (ping->Number()>2)
        {
            m_con.DeleteRequest(entityProxy.GetEntityId(), this);            
        }
        else
        {
            m_con.UpdateRequest(ping, entityProxy.GetInstanceId(), this);
        }*/
            
        m_supervisor.UpdateRequestTimestamp();        
    }
    catch (const Safir::Dob::OverflowException& /*ex*/)
    {
        //TRACE_OUT("Overflow");        
    }
}
