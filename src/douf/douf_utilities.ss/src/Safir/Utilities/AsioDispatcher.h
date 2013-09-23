/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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
#ifndef __DOB_UTILITIES_ASIO_DISPATCHER_H__
#define __DOB_UTILITIES_ASIO_DISPATCHER_H__

#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Internal/Atomic.h>
#include <boost/asio.hpp>
#include <boost/bind.hpp>
#include <boost/noncopyable.hpp>

namespace Safir
{
namespace Utilities
{

    /**
     * The class makes a thread switch and perform a dispatch on Dob connection.
     */
    class AsioDispatcher
        : public Safir::Dob::Dispatcher
        , private boost::noncopyable
    {
    public:
       /**
        * Constructor.
        *
        * @param [in] connection - The dob connection.
        * @param [in] ioService  - The ioService that runs the main loop
        */
        AsioDispatcher(const Safir::Dob::Connection & connection,
                       boost::asio::io_service& ioService)
            : m_connection(connection)
            , m_ioService(ioService)
            , m_isNotified(0) {}

    private:

       /**
        * Perform dispatch on the connection.
        */
        void Dispatch()
        {
            m_isNotified = 0;
            m_connection.Dispatch();
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
                m_ioService.post(boost::bind(&AsioDispatcher::Dispatch,this));
            }
        }


        const Safir::Dob::Connection&      m_connection;
        boost::asio::io_service&           m_ioService;
        Safir::Dob::Internal::AtomicUint32 m_isNotified;
    };


} // namespace Utilities
} // namespace Safir

#endif // __DOB_UTILITIES_ASIO_DISPATCHER_H__
