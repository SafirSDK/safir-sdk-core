/******************************************************************************
*
* Copyright Saab AB, 2008-2013,2022 (http://safirsdkcore.com)
*
* Created by: Petter Lönnstedt / stpeln
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

#ifdef VEHICLEAPP_USE_BOOST
#include <Safir/Utilities/AsioDispatcher.h>
#else
#include <mutex>
#include <condition_variable>
#endif

#include "EntityHandler.h"
#include "ServiceHandler.h"
#include <Safir/Dob/Connection.h>


namespace VehicleAppCpp
{
    /**
    * This application is an example of how to use DOB producer mechanisms.
    * The code is written to show the possibilities of the interfaces
    * rather than showing a real world example. The application can be run together
    * with VehicleMmi (.Net) or VehicleMmiQt application that implements DOB
    * consumer mechanisms.
    *
    * The application will show how to :
    * 1) Register as handler of entity 'Vehicle' and create, update and delete vehicles
    *    in DOB (shared memory) on create, update and delete requests from users.
    * 2) Handle service request 'CalculateSpeedDifference' and calculate speed difference
    *    and send service response 'CalculateSpeedDifferenceResponse' to service requestors.
    * 3) Use property 'SpeedObjectProperty' to get a property value from an object.
    * 4) Send message 'VehicleMsg'.
    */


    /**
     * Main class. Controls startup, closedown and receives events.
     */
    class App :
        // Allows this class to receive a stop order.
        public Safir::Dob::StopHandler
    {
    public:

        App();

        /**
        * Startup application: open dob connection, register entity ownership
        * and service provider, attach message sender and start event loop.
        */
        int Run();

        /**
         * Called by the stop handler.
         */
        void OnStopOrder() override;

    private:
        // Primary connection for Dob calls
        Safir::Dob::Connection m_connection;

#ifdef VEHICLEAPP_USE_BOOST
        // This application is event driven. It's triggered by events
        // received in the main event loop.
        // The main event loop for this application is implemented by
        // the asio io_service.
        //
        // The Dispatcher class makes a thread switch from the calling
        // Dob thread to this applications main thread. It performs a dispatch
        // on the Dob connection that will result in callbacks to all overidden
        // Dob interface methods, for example OnCreateRequest call in EntityOwner.
        boost::asio::io_service m_ioService;
        Safir::Utilities::AsioDispatcher   m_dispatch;
#else
        // The StdDispatcherLoop class is a very basic replacement for Boost.Asio io_service
        // All this does is to switch to the main thread and do dispatching there when an
        // OnDoDispatch callback is received. Read more about dispatching in the User's Guide.
        // This class is probably not really useful for you, it is only here to allow compilation
        // of this example even when Boost is not installed. Prefer solving this problem using
        // whatever framework you are using for your event loops.
        class StdDispatcherLoop
            : public Safir::Dob::Dispatcher
        {
        public:
            explicit StdDispatcherLoop(const Safir::Dob::Connection& connection) : m_connection(connection) {}

            void Run()
            {
                for(;;)
                {
                    std::unique_lock lk(m_mutex);
                    m_cv.wait(lk);

                    if (m_dispatchPending)
                    {
                        m_dispatchPending = false;
                        m_connection.Dispatch();
                    }

                    if (m_stopPending)
                    {
                        return;
                    }

                }
            }

            void Stop()
            {
                //OnStopOrder happens in the main thread, so no need for mutex
                m_stopPending = true;
            }

            void OnDoDispatch() override
            {
                {
                    std::lock_guard lk(m_mutex);
                    m_dispatchPending = true;
                }
                m_cv.notify_one();
            }

        private:
            const Safir::Dob::Connection& m_connection;
            bool m_dispatchPending = false;
            bool m_stopPending = false;
            std::mutex m_mutex;
            std::condition_variable m_cv;
        };

        StdDispatcherLoop m_dispatch;
#endif
        // DOB object handlers.
        EntityHandler entityHandler;
        ServiceHandler serviceHandler;

    };
}

