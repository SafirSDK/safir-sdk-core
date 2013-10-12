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
#include "app.h"

namespace VehicleMmiCppQt
{

    // Defines own event to perform thread switch by posting this event,
    // see class QtDispatcher.
    const QEvent::Type QT4_Dob_EVENT_TYPE = (QEvent::Type)(QEvent::User + 100);

    App::App(int& argc,char** argv) : QApplication(argc,argv)
    {
        std::wstring wstrInstance = L"0";

        // Open the Dob connection
        m_connection.Open(L"VehicleMmiCppQt", wstrInstance, 0, this,
            new VehicleMmiCppQt::QtDispatcher(this, QT4_Dob_EVENT_TYPE, m_connection));

        // Show main window
        m_pEntityMw = new EntityMw();
        if(m_pEntityMw)
        {
           m_pEntityMw->show();
        }

        // To enable termination of application by operator choice.
        connect(this, SIGNAL(lastWindowClosed()), this, SLOT(quit()));
    }


    App::~App(void)
    {
        // Delete pointers
        if(m_pEntityMw)
        {
           delete m_pEntityMw;
           m_pEntityMw = NULL;
        }
    }


    void App::OnStopOrder()
    {
        // Call the terminate method in framework
        QApplication::quit();
    }
}
