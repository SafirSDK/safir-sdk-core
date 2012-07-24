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

#include "App.h"
#include "MessageSender.h"

#ifdef NO_ACE

#else
#  include <ace/Event_Handler.h>
#endif

namespace VehicleAppCpp
{
    App::App()
#ifdef NO_ACE
        : m_dispatch(m_connection,m_ioService)
#else
        : m_dispatch(m_connection)
#endif
    {
    }

    int App::Run()
    {
        m_connection.Open(L"VehicleAppCpp", L"0", 0, this, &m_dispatch);

        entityHandler.Init();
        //StartRemoveInExercise
        serviceHandler.Init();
        //StopRemoveInExercise
        MessageSender::Instance().Init();

#ifdef NO_ACE
        boost::asio::io_service::work keepRunning(m_ioService);
        m_ioService.run();
#else
        // Start Ace event loop in order to receive DOB callbacks
        // for example OnCreateRequest in EntityOwner.
        ACE_Reactor::instance()->run_reactor_event_loop();
#endif
        return 0;
    }

    void App::OnStopOrder()
    {
#ifdef NO_ACE
        m_ioService.stop();
#else
        ACE_Reactor::instance()->end_event_loop();
#endif
    }
}
