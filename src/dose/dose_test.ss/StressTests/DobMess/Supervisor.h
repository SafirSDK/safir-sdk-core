/******************************************************************************
*
* Copyright Saab AB, 2007-2011 (http://www.safirsdk.com)
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
#ifndef _supervisor_h_
#define _supervisor_h_

#include <boost/thread/mutex.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

class Supervisor
{
public:
    Supervisor();
    ~Supervisor(void);

    void UpdateDispatchTimestamp();  
    void UpdateRequestTimestamp();  
    void Start(); //start supervisor in own thread

   //thread entry
    void Thread();

private:    
    boost::mutex m_mutex;
    boost::posix_time::ptime m_dispatchTimestamp;
    boost::posix_time::ptime m_requestTimestamp;

    void CheckTimestamps(bool& dispatchOk, bool& requestOk);    
    
    static const int TIME_THRESHOLD = 15; //seconds before signaling error.
};

#endif
