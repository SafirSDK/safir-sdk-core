/******************************************************************************
*
* Copyright Saab AB, 2007-2013, 2023 (http://safirsdkcore.com)
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

#include "foreach_app.h"
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Logging/Log.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/LowMemoryException.h>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4702)
#endif

#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

namespace Safir
{
namespace Utilities
{
namespace ForEach
{
    ForEachApp::ForEachApp()
    {
        const Safir::Dob::Typesystem::Int32 numContexts = Safir::Dob::NodeParameters::NumberOfContexts();
        for (int context = 0; context < numContexts; ++context)
        {
            m_context.push_back(boost::shared_ptr<ContextData>(new ContextData(m_ioContext)));
        }
    }

    int ForEachApp::Run()
    {
        // Open a connection in each context
        for (unsigned int context = 0; context < m_context.size(); ++context)
        {
            int inst = 0;
            std::wstring connectionName(L"ForEach-context");
            connectionName += boost::lexical_cast<std::wstring>(context);
            for (;;)
            {
                try
                {
                    // Open the DOB connection.
                    m_context[context]->m_connection.Open
                        (connectionName,
                         boost::lexical_cast<std::wstring>(inst),
                         context,
                         context == 0 ? this : NULL, //only context 0 connection has stop handler.
                         &m_context[context]->m_dispatcher);
                    break;
                }
                catch (const Safir::Dob::NotOpenException&)
                {
                    ++inst;
                }
                catch (const Safir::Dob::LowMemoryException&)
                {
                    Safir::Logging::SendSystemLog(Safir::Logging::Critical,
                                                  L"Failed to open Dob connection due to low shared memory. Exiting.");
                    return 1;
                }
            }

            if (context == 0)
            {
                Safir::Application::TracerBackdoor::Start(m_context[0]->m_connection);
            }

            // Send something to the tracer to open the connection.
            m_context[context]->m_debug << "Starting ForEach in context " << context << std::endl;

            try
            {
                // Call the init method on startup.
                // Register as a service provider.
                m_context[context]->m_service.Init(connectionName,
                                                   boost::lexical_cast<std::wstring>(inst));
            }
            catch (const Safir::Dob::LowMemoryException&)
            {
                Safir::Logging::SendSystemLog(Safir::Logging::Critical,
                                              L"Failed to register as a service provider due to low shared memory. Exiting.");
                return 1;
            }

        }

        auto keepRunning = boost::asio::make_work_guard(m_ioContext);

        m_ioContext.run();

        for (ContextVector::reverse_iterator it = m_context.rbegin();
             it != m_context.rend(); ++it)
        {
            (*it)->m_connection.Close();
        }

        return 0;
    }


    void ForEachApp::OnStopOrder()
    {
        Safir::Application::TracerBackdoor::Stop();
        m_ioContext.stop();
    }
}
}
}
