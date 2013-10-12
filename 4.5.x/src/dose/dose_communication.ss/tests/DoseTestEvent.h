/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Engdahl / stlsen
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

/********************************************
*
* Example of usage:
* CDoseComEvent *pMyEvent;
*
* Thread 1:
* pMyEvent = new CDoseTestvent;
* pMyEvent->Create();
* pMyEvent->WaitFor(1000);
*
* Thread 2:
* pMyEvent->Set();
*********************************************/

#ifdef _LINUX
#include <pthread.h>     // pthread functions and data structures
#endif


class CDoseTestEvent
{
public:
    int Set();
    int Create();

    // WaitFor returns:
    //  0 - timeout / 1 - wakedup by an event / -1 - error
    int WaitFor(unsigned long TimeoutMs);

private:
#ifdef _WIN32
    HANDLE m_EventId;
#endif
#ifdef _LINUX
    pthread_mutex_t m_Mutex;
    pthread_cond_t  m_Condition;
    int             m_SignalCounter;
#endif
};
