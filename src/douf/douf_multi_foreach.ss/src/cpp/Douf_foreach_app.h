/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Stefan Lindström / stsyli
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
#ifndef __DOUF_FOREACH_APP_H
#define __DOUF_FOREACH_APP_H


#include <Safir/Dob/Connection.h>
#include <Safir/Application/Tracer.h>
#include <Safir/Utilities/AceDispatcher.h>

#include "Douf_foreach_services.h"

namespace Safir
{
    namespace Utilities
    {
        namespace ForEach
        {
            /** 
            * Main class. Controls startup, closedown and receives events.
            */
            class ForEachApp :
                // Allows this class to receive a stop order.
                public Safir::Dob::StopHandler
            {
            public:

                /** Constructor
                */
                ForEachApp();

                int Run();

                /** 
                * Called by the stop handler.
                */
                void OnStopOrder();

            private:

                // DOB connection.
                Safir::Dob::Connection m_connection;

                // The DOB dispatch event.
                Safir::Utilities::AceDispatcher     m_dispatcher;

                // DOB object handlers.
                Safir::Utilities::ForEach::Services m_service;

                // Tracer.
                Safir::Application::Tracer m_debug;
            };
        }
    }
}
#endif
