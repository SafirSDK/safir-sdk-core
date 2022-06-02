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

#include "App.h"
#include "MessageSender.h"

namespace VehicleAppCpp
{
    App::App()
#ifdef VEHICLEAPP_USE_BOOST
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

#ifdef VEHICLEAPP_USE_BOOST
        // Start the asio io-service loop in order to receive DOB callbacks
        // for example OnCreateRequest in EntityOwner.
        // We also need to define some dummy work in order for the io_service
        // to keep running until we tell it to stop.
        boost::asio::io_service::work keepRunning(m_ioService);
        m_ioService.run();
#else
        m_dispatch.Run();
#endif
        m_connection.Close();
        return 0;
    }

    void App::OnStopOrder()
    {
#ifdef VEHICLEAPP_USE_BOOST
        m_ioService.stop();
#else
        m_dispatch.Stop();
#endif
    }
}
