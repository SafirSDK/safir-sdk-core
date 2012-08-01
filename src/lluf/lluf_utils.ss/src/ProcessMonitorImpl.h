/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Jonas Thor / stjth
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
#ifndef __PROCESS_MONITOR_IMPL_H__
#define __PROCESS_MONITOR_IMPL_H__

//disable warnings in ace
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4267 4244)
#endif

#include <ace/OS_NS_unistd.h>

//and enable the warnings again
#if defined _MSC_VER
  #pragma warning (pop)
#endif

namespace Safir
{
namespace Utilities
{
    class ProcessMonitorImpl
    {
    public:
        ProcessMonitorImpl() {};
        virtual ~ProcessMonitorImpl() {};

        virtual void StartThread() = 0;
        virtual void StopThread() = 0;
        
        
        virtual void StartMonitorPid(const pid_t pid) = 0;
        virtual void StopMonitorPid(const pid_t pid) = 0;
    };
}
}

#endif
