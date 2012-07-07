/******************************************************************************
*
* Copyright Saab AB, 2012 (http://www.safirsdk.com)
*
* Created by: Anders Widén / aiwi
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

#ifndef _dose_main_lock_monitor_h
#define _dose_main_lock_monitor_h

#include <boost/thread.hpp>
#include <Safir/Dob/Internal/InternalDefs.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class LockMonitor
    {
    public:

        // Monitors apparently abandoned locks in shared memory

        LockMonitor();
        ~LockMonitor();

        //void StartWatchdog(const boost::thread::id& threadId,
        //                   const std::string& threadName);

        //void StopWatchdog(const boost::thread::id& threadId);

        //void KickWatchdog(const boost::thread::id& threadId);

    private:

        void Check();

        Dob::Typesystem::TypeIdVector m_serviceTypeIds;
        Dob::Typesystem::TypeIdVector m_entityTypeIds;
        
        bool    m_loggingIsEnabled;

        boost::thread m_checkerThread;
    };
}
}
}

#endif
