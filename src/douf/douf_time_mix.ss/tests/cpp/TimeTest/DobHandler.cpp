/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
#include <ace/OS_NS_time.h>
#include "DobHandler.h"
#include <Safir/Test/TimeConversion.h>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <Safir/Time/TimeProvider.h>
#include <iostream>

//--------------------------------------------------------------------------------------------------

DobHandler::DobHandler(const std::wstring appName): m_dispatcher(m_Dob, &m_reactor)
{
    std::cout  << "Open connection..." << std::endl;        
    m_Dob.Open(appName, L"1", 0, &m_consumer, &m_dispatcher);  
    std::cout  << "Connection opened" << std::endl;        
}

DobHandler::~DobHandler(void)
{
}


//--------------------------------------------------------------------------------------------------

SubscriptionHandler::SubscriptionHandler(void): DobHandler(L"Sub")
{
}

SubscriptionHandler::~SubscriptionHandler(void)
{
}

void SubscriptionHandler::Start(void)
{

    m_Dob.SubscribeEntity(Safir::Test::TimeConversion::ClassTypeId, true, true, true, &m_consumer);

    m_reactor.run_reactor_event_loop();

}

//--------------------------------------------------------------------------------------------------

RequestHandler::RequestHandler(void): DobHandler(L"Req")
{
}

RequestHandler::~RequestHandler(void)
{
}

void RequestHandler::Start(void)
{
    Safir::Test::TimeConversionPtr timeConv = Safir::Test::TimeConversion::Create();

    m_Dob.RegisterEntityHandler(Safir::Test::TimeConversion::ClassTypeId, Safir::Dob::Typesystem::HandlerId(), Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId, &m_consumer);

    do
    {
        // Get current UTC time 
        Safir::Dob::Typesystem::Float64 f = Safir::Time::TimeProvider::GetUtcTime();
        boost::posix_time::ptime tim = Safir::Time::TimeProvider::ToPtime(f);
        std::cout << tim << std::endl;

        // Send it to Dob
        timeConv->TimeStamp().SetVal(f);
    
        m_Dob.SetAll(timeConv, Safir::Dob::Typesystem::InstanceId(0), Safir::Dob::Typesystem::HandlerId());

        timespec t;
        t.tv_sec = 5;
        t.tv_nsec = 17000;
        ACE_OS::nanosleep(&t);

        m_Dob.Dispatch();

    }while(true);
}
