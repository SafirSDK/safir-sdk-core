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

#include "Douf_foreach_app.h"

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4702)
#endif

#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include <Safir/Dob/NotOpenException.h>
#include <Safir/SwReports/SwReport.h>

namespace Safir
{
    namespace Utilities
    {
        namespace ForEach
        {
            ForEachApp::ForEachApp() :
            m_dispatcher(m_connection),
            m_debug(L"ForEachApp")
        {
            // Send something to the tracer to open the connection.
            // This line will hopefully not be needed in the future.
            m_debug << " "<<std::endl;
        }

        int ForEachApp::Run()
        {
            static int inst = 0;

            for (;;)
            {
                try
                {
                    // Open the DOB connection.
                    m_connection.Open(L"ForEach", boost::lexical_cast<std::wstring>(++inst), -1, this, &m_dispatcher);
                    break;
                }
                catch (const Safir::Dob::NotOpenException&)
                {
                }
            }

            // Call the init method on startup.
            // Register as a service provider.
            m_service.Init();      

            ACE_Reactor::instance()->run_reactor_event_loop();

             // Stop swre before exiting
            Safir::SwReports::Stop();

            return 0;
        }


        void ForEachApp::OnStopOrder()
        {
            ACE_Reactor::end_event_loop();
        } 
        }
    }
}
