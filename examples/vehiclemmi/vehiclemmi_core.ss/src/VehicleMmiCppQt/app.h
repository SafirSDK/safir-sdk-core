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

#include "entitymw.h"
#include "qtdispatcher.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4257)
#pragma warning (disable: 4251)
#endif

#include <QtGui/QApplication>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include <Safir/Dob/Connection.h>

namespace VehicleMmiCppQt
{
    /**
    * This presentation application is an example of how to use Dob consumer
    * mechanisms. The code is written to show the possibilities of the interfaces
    * rather than showing a real world example. The application can be run together
    * with VehicleApp application that implements Dob producer mechanisms.
    *
    * The application will show how to :
    * 1) Subscribe for entity 'Vehicle' and display data in a listview and a dialog.
    *    It is possible for the operator to create, update and delete vehicles.
    * 2) Send service request 'CalculateSpeedDifference' to calculate speed difference
    *    and receive servive response 'CalculateSpeedDifferenceResponse' and
    *    present the result in a dialog.
    * 3) Use property 'SpeedObjectProperty' to get a property value from an object.
    * 4) Susbcribe for message 'VehicleMsg' and present recived message text in a dialog.
    */


    /**
    * Main class, controls startup, closedown, Dob connection and main window.
    */
    class App :
        // The main event loop for this application is implemented by the QApplication.
        public QApplication,
        // Allows this class to receive a stop order.
        public Safir::Dob::StopHandler
    {
    public:

        /**
        * Open Dob connection and show main window.
        */
        App(int& argc,char** argv);

        virtual ~App(void);

        /**
        * Called by the stop handler to terminate application.
        */
        void OnStopOrder();

    private:
        // Primary connection for Dob calls
        Safir::Dob::Connection      m_connection;

        // Main window and Dob object handler.
        EntityMw*                   m_pEntityMw;
    };

}


#endif   // __APP_H

