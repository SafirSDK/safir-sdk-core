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

#include "qtdispatcher.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4251)
#endif

#include <QtCore/QCoreApplication>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

namespace VehicleMmiCppQt
{

    QtDispatcher::QtDispatcher(QObject* pParent,QEvent::Type eEventType, Safir::Dob::Connection& connection)
        : QObject(pParent),
          m_connection(connection),
          m_eEventType(eEventType)
    {
        installEventFilter(this);
    }


    QtDispatcher::~QtDispatcher(void)
    {
    }


    bool QtDispatcher::eventFilter(QObject* pObject,QEvent* pEvent)
    {
        // Checks if the event is a Dob event
        if(pEvent->type() == m_eEventType)
        {
            // Dispatches the event
            m_connection.Dispatch();

            // Return true to prevent additional event handling since the event
            // has been taken care of already
            return true;
        }

        // There is no interest in the message for this application
        return QObject::eventFilter(pObject,pEvent);
    }


    void QtDispatcher::OnDoDispatch()
    {
        // Post event to switch thread between Dob and this application
        // thread
        QCoreApplication::postEvent(this, new QEvent(m_eEventType));
    }

} // namespace VehicleMmiCppQt
