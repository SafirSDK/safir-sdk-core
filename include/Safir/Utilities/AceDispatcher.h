/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
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
#ifndef __DOB_UTILITIES_ACE_DISPATCHER_H__
#define __DOB_UTILITIES_ACE_DISPATCHER_H__

#ifndef SAFIR_NO_DEPRECATED

#include <boost/noncopyable.hpp>
#include <Safir/Dob/Connection.h>
#include <atomic>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4127 4244 4251)
#endif

#include <ace/Reactor.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace Safir
{
namespace Utilities
{

    /**
     * The class makes a thread switch and perform a dispatch on Dob connection.
     *
     * @deprecated This class will be removed from Safir SDK Core eventually, make your 
     *             own copy of it if you need it.
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
            m_isNotified(){}

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
            m_isNotified(){}

    private:

       /**
        * handle_exception performs dispatch on the connection.
        * Overrides ACE_Event_Handler.
        */
        int handle_exception(ACE_HANDLE) override
        {
            m_isNotified.clear();
            m_connection.Dispatch();
            return 0;
        }

         /**
         * OnDoDispatch notifies the dob connection thread that it's time to perform a dispatch.
         * Overrides Safir::Dob::Dispatcher.
         */
        void OnDoDispatch() override
        {
            if (!m_isNotified.test_and_set())
            {
                reactor()->notify(this);
            }
        }


        const Safir::Dob::Connection&      m_connection;
        std::atomic_flag                   m_isNotified;
};

} // namespace Utilities
} // namespace Safir

#endif
#endif // __DOB_UTILITIES_ACE_DISPATCHER_H__
