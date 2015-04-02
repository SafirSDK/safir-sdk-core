/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#pragma once

#include <Safir/Dob/Connection.h>
#include <Safir/Utilities/Internal/Atomic.h>
#include <boost/bind.hpp>
#include <boost/bind/protect.hpp>
#include <boost/noncopyable.hpp>
#include <boost/function.hpp>
#include <boost/atomic.hpp>
#include <boost/make_shared.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

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
        * Strandless constructor.
        *
        * Creates an AsioDispatcher that performs Dispatches using an internal strand.
        * I.e. all Dob callbacks will be performed from within the same - hidden - strand.
        *
        * If you need access to the strand, use the other constructor.
        *
        * @param [in] connection - The dob connection.
        * @param [in] ioService  - The ioService that runs the main loop
        */
        AsioDispatcher(const Safir::Dob::Connection & connection,
                       boost::asio::io_service& ioService)
            : m_connection(connection)
            , m_strand(boost::make_shared<boost::asio::strand>(ioService))
            , m_isNotified()
        {

        }

        /**
        * Stranded constructor.
        *
        * Creates an AsioDispatcher that performs Dispatches using a specified strand.
        * I.e. all Dob callbacks will be performed from the given strand.
        *
        * @param [in] connection - The dob connection.
        * @param [in] strand  - The strand to dispatch from.
        */
        AsioDispatcher(const Safir::Dob::Connection & connection,
                       boost::asio::strand& strand)
            : m_connection(connection)
            , m_strand(&strand,null_deleter())
            , m_isNotified()
        {

        }

    private:
        struct null_deleter
        {
            void operator()(void const *) const
            {
            }
        };

       /**
        * Perform dispatch on the connection.
        */
        void CallDobDispatch()
        {
            m_isNotified.clear();
            m_connection.Dispatch();
        }

         /**
         * OnDoDispatch notifies the dob connection thread that it's time to perform a dispatch.
         * Overrides Safir::Dob::Dispatcher.
         */
        void OnDoDispatch() override
        {
            if (!m_isNotified.test_and_set())
            {
                m_strand->dispatch(boost::bind(&AsioDispatcher::CallDobDispatch,this));
            }
        }

        const Safir::Dob::Connection&      m_connection;
        boost::function<void()>            m_notifyFunction;
        boost::shared_ptr<boost::asio::strand> m_strand;
        boost::atomic_flag                 m_isNotified;
    };


}
}
