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

#ifndef _QT_QTDISPATCHER_H
#define _QT_QTDISPATCHER_H

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4251)
#endif

#include <QtCore/QObject>
#include <QtCore/QEvent>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include <Safir/Dob/Connection.h>

namespace VehicleMmiCppQt
{
    /**
    * This application is event driven. It's triggered by events
    * received in the main event loop.
    * The main event loop for this application is implemented by
    * the QApplication. The event loop is started in the main method.
    *
    * This class makes a thread switch from the calling Dob thread to this
    * applications main thread. It performs a dispatch on the Dob connection
    * that will result in callbacks to all overidden Dob interface methods,
    * for example OnNewEntity call in EntityTableHandler.
    */
    class QtDispatcher :
        public QObject,
        // Allows this class to receive a dispatch order.
        public Safir::Dob::Dispatcher
    {
    public:
        /**
        * Install eventfilter, the event filter receives all events that are sent to this
        * application. The event filter receives events via its eventFilter() function,
        * see below.
        */
        QtDispatcher(QObject* pParent, QEvent::Type eEventType, Safir::Dob::Connection& connection);

        virtual ~QtDispatcher(void);

    protected:

        /**
        * Filter events. Checks if the event is a QT4_Dob_EVENT_TYPE,
        * if so perform a dispatch on the Dob connection that will result
        * in callbacks to all overidden Dob interface methods, for example
        * OnNewEntity call in EntityTableHandler.
        * The function return true if the event is QT4_Dob_EVENT_TYPE
        * (defined in App file) and should be filtered, (i.e. stopped);
        * otherwise it return false
        */
        virtual bool eventFilter(QObject* pObject,QEvent* pEvent);

        /*
        * Overrides Safir::Dob::Dispatcher, called by Dob to indicate
        * that there is incoming data for the connection so the
        * application shall call Dispatch().
        * Post event QT4_Dob_EVENT_TYPE to do the thread switch between
        * Dob and this application.
        */
        void OnDoDispatch();

        // Member variables
        Safir::Dob::Connection& m_connection;
        QEvent::Type            m_eEventType;
    };

} // namespace VehicleMmiCppQt

#endif // _QT_QTDISPATCHER_H
