/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

#ifndef __APP_H
#define __APP_H

#include <Safir/Utilities/AsioDispatcher.h>

#include <Safir/Dob/Connection.h>

namespace VehicleDatabaseCpp
{
    /**
    * This application is an example of how to use the database interface.
    * The application will show how to :
    * 1) Read an entry in a database table.
    * 2) Create or update an entry in a database table.
    * 3) Delete an entry in a database table.
    * The operations are provided as DOB services.
    *
    * This simple example manages vehicle category information stored in
    * a database. Each category is associated with some information.
    * A real world example would probably contain more complex operations.
    *
    * Exception handling, resource handling, recovery handling and SW reporting
    * is omitted or minimized to not obscure the database interfacing.
    * Yet, the necessary exception handling is indicated.
    *
    * The application can be run together with:
    *  - VehicleApp (producer of Vehicle objects).
    *  - VehicleMmi (consumer of vehicle objects and user of the database
    *                services provided by this example).
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
        void OnStopOrder();

    private:
        // Primary connection for Dob calls
        Safir::Dob::Connection m_connection;

        // This application is event driven. It's triggered by events
        // received in the main event loop.
        // The main event loop for this application is implemented by
        // the asio io_service
        //
        // The Dispatcher class makes a thread switch from the calling
        // Dob thread to this applications main thread. It performs a dispatch
        // on the Dob connection that will result in callbacks to all overidden
        // Dob interface methods, for example OnCreateRequest call in EntityOwner.
        boost::asio::io_service m_ioService;
        Safir::Utilities::AsioDispatcher   m_dispatch;
    };

}
#endif
