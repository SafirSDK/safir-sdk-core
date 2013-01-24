/******************************************************************************
*
* Copyright Saab AB, 2008-2009 (http://www.safirsdk.com)
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

// The cmake configuration allows us to compile this application with or without ACE
// If ace is not found we use Boost.Asio instead.
#ifdef NO_ACE
#  include <Safir/Utilities/AsioDispatcher.h>
#else
#  include <Safir/Utilities/AceDispatcher.h>
#  ifndef ACE_HAS_WINSOCK2
#    define ACE_HAS_WINSOCK2 0
#  endif
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
        void OnStopOrder();

    private:
        // Primary connection for Dob calls
        Safir::Dob::Connection m_connection;
   
        // This application is event driven. It's triggered by events 
        // received in the main event loop. 
        // The main event loop for this application is implemented by 
        // the process wide reactor in the singleton class ACE_Reactor.
        // The event loop is started in the Run method.
        //
        // The Dispatcher class makes a thread switch from the calling 
        // Dob thread to this applications main thread. It performs a dispatch
        // on the Dob connection that will result in callbacks to all overidden 
        // Dob interface methods, for example OnCreateRequest call in EntityOwner.
#ifdef NO_ACE
        boost::asio::io_service m_ioService;
        Safir::Utilities::AsioDispatcher   m_dispatch;
#else
        Safir::Utilities::AceDispatcher   m_dispatch;
#endif
        
        // DOB object handlers.
        EntityHandler entityHandler;
        ServiceHandler serviceHandler;

    };
}
#endif
