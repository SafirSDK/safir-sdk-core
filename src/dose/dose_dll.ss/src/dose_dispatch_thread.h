/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / stjoot
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

#ifndef _dose_dispatch_thread_h
#define _dose_dispatch_thread_h

#include <Safir/Dob/Internal/Interface.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/ConsumerId.h>

#include <boost/noncopyable.hpp>
#include <boost/enable_shared_from_this.hpp>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4244)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    /**
     * This class is best used with a shared_ptr, since the only way to stop the thread is to
     * destroy the object.
     */
    class DispatchThread:
        public boost::enable_shared_from_this<DispatchThread>,
        private boost::noncopyable
    {
    public:
        DispatchThread(const ConnectionId & id,
                       const std::string& connectionName,
                       const ConsumerId & consumer,
                       OnDispatchCb * const onDispatchCb);

        /**
         * Thread must be in a stopped state to be possible to destroy!
         */
        ~DispatchThread();

        /**
         * Stop the thread. Blocks until the thread is stopped!
         */
        void Stop();

        /**
         * Start the dispatch thread.
         */
        void Start();

    private:
        void Run(); //dispatch loop

        OnDispatchCb * const m_onDispatchCallback;

        const ConsumerId m_consumer;

        const ConnectionId m_id;
        const std::string m_connectionName;

        boost::thread m_thread;

        AtomicUint32 m_stop;
    };
}
}
}

#endif
