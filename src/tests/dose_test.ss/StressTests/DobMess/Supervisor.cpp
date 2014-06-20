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
#ifdef WIN32
#include <Windows.h>
#endif

#include "Supervisor.h"
#include <boost/thread.hpp>
#include <boost/interprocess/detail/os_thread_functions.hpp>

int Supervisor::TimeThreshold = 15; //default value is 15 seconds

void threadStart(Supervisor* ptr)
{   
    ptr->Thread();
}

Supervisor::Supervisor(void)
{
}


Supervisor::~Supervisor(void)
{
}

void Supervisor::Start()
{
    boost::thread worker(threadStart, this);    
}

void Supervisor::Thread()
{
    bool dispatchErrorReported = false;
    bool dispatchOk = false;
    bool requestErrorReported = false;
    bool requestOk = false;
    UpdateDispatchTimestamp();
    UpdateRequestTimestamp();
    while (true)
    {
        CheckTimestamps(dispatchOk, requestOk);

        if (dispatchOk && requestOk)
        {
            if (dispatchErrorReported && dispatchOk)
            {
                std::cout<<"Received dispatch again, time: "<<boost::posix_time::to_simple_string(m_dispatchTimestamp)<<std::endl;
                dispatchErrorReported=false;
            }

            if (requestErrorReported && requestOk)
            {
                std::cout<<"Request sent again, time: "<<boost::posix_time::to_simple_string(m_requestTimestamp)<<std::endl;
                requestErrorReported=false;
            }
            
            Sleep(TimeThreshold*500); //wait half the TimeThreshold before next check.
        }
        else
        {      
#ifdef WIN32
            Beep(700, 300);    //Beep in computer speaker. Todo: implement for Linux
#else
            printf("\a"); //will make a short beep
#endif
            if (!dispatchErrorReported && !dispatchOk)
            {              
                std::cout<<"Process: "<<boost::interprocess::detail::get_current_process_id()<<", time now: "<<boost::posix_time::to_simple_string(boost::posix_time::second_clock::local_time())<<std::endl;
                std::cout<<"Last dispatch time: "<<boost::posix_time::to_simple_string(m_dispatchTimestamp)<<std::endl;
                dispatchErrorReported=true;                
            }
            if (!requestErrorReported && !requestOk)
            {                    
                std::cout<<"Process: "<<boost::interprocess::detail::get_current_process_id()<<", time now: "<<boost::posix_time::to_simple_string(boost::posix_time::second_clock::local_time())<<std::endl;
                std::cout<<"Last request sent: "<<boost::posix_time::to_simple_string(m_requestTimestamp)<<std::endl;
                requestErrorReported=true;                
            }
            
            Sleep(2000);
        }        
    }
}

void Supervisor::UpdateDispatchTimestamp()
{
    boost::mutex::scoped_lock lock(m_mutex);
    m_dispatchTimestamp = boost::posix_time::second_clock::local_time();    
}

void Supervisor::UpdateRequestTimestamp()
{
    boost::mutex::scoped_lock lock(m_mutex);
    m_requestTimestamp = boost::posix_time::second_clock::local_time();    
}

void Supervisor::CheckTimestamps(bool& dispatchOk, bool& requestOk)
{
    boost::mutex::scoped_lock lock(m_mutex);
    boost::posix_time::ptime now = boost::posix_time::second_clock::local_time();
    boost::posix_time::time_duration dispatchDur = now-m_dispatchTimestamp;   
    boost::posix_time::time_duration reqDur = now-m_requestTimestamp;   
    dispatchOk=dispatchDur.total_seconds()<TimeThreshold;
    requestOk=reqDur.total_seconds()<TimeThreshold;    
}
