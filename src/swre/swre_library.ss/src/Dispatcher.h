/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrha
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
#ifndef __SWRE_DISPATCHER_H__
#define __SWRE_DISPATCHER_H__

#include <Safir/Dob/Connection.h>
#include <Safir/Utilities/Internal/Atomic.h>
#include <boost/bind.hpp>
#include <boost/noncopyable.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

/**
 * This is a copy of the AsioDispatcher from douf, which we can't use since
 * douf depends on us, rather than the other way around.
 */
class Dispatcher
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
    Dispatcher(const Safir::Dob::Connection & connection,
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
            m_ioService.post(boost::bind(&Dispatcher::Dispatch,this));
        }
    }


    const Safir::Dob::Connection&      m_connection;
    boost::asio::io_service&           m_ioService;
    Safir::Utilities::Internal::AtomicUint32 m_isNotified;
};

#endif
