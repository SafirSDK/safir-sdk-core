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
#ifndef __DOB_UTILITIES_DISPATCHER_H__
#define __DOB_UTILITIES_DISPATCHER_H__

#include <ace/Reactor.h>
#include <boost/noncopyable.hpp>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Internal/Atomic.h>

namespace Safir
{
namespace Utilities
{

    /**
     * The class makes a thread switch and perform a dispatch on Dob connection.
     */
    class AceDispatcher:
        public Safir::Dob::Dispatcher,
        public ACE_Event_Handler,
        private boost::noncopyable
    {
    public:
       /**
        * Constructor.
        * The process wide ACE reactor is used and is excepted to be the owner of the running event loop.
        *
        * @param [in] connection - The dob connection.
        */
        AceDispatcher(const Safir::Dob::Connection & connection):
            ACE_Event_Handler(ACE_Reactor::instance()),
            m_connection(connection),
            m_isNotified(0){}

       /**
        * Constructor.
        * A pointer to a local ACE reactor is supplied by the user of this class.
        *
        * @param [in] connection - The dob connection.
        * @param [in] reactor    - A pointer to the reactor running the event loop
        */
        AceDispatcher(const Safir::Dob::Connection & connection,
                      ACE_Reactor * reactor):
            ACE_Event_Handler(reactor),
            m_connection(connection),
            m_isNotified(0){}

    private:

       /**
        * handle_exception performs dispatch on the connection.
        * Overrides ACE_Event_Handler.
        */
        virtual int handle_exception(ACE_HANDLE)
        {
            m_isNotified = 0;
            m_connection.Dispatch();
            return 0;
        }

         /**
         * OnDoDispatch notifies the dob connection thread that it's time to perform a dispatch.
         * Overrides Safir::Dob::Dispatcher.
         */
        virtual void OnDoDispatch()
        {
            if (m_isNotified == 0)
            {
                m_isNotified = 1;
                reactor()->notify(this);
            }
        }


        const Safir::Dob::Connection&   m_connection;
        Safir::Dob::Internal::AtomicUint32                    m_isNotified;
};

} // namespace Utilities
} // namespace Safir

#endif // __DOB_UTILITIES_DISPATCHER_H__
