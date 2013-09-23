/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#ifndef __FOREACH_APP_H
#define __FOREACH_APP_H


#include <Safir/Dob/Connection.h>
#include <Safir/Application/Tracer.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <vector>

#include "foreach_services.h"

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

        struct ContextData
        {
            explicit ContextData(boost::asio::io_service& ioService) 
                : m_dispatcher(m_connection, ioService)
                , m_service(ioService)
                , m_debug(L"ForEachApp") {}

            Safir::Dob::Connection m_connection;

            Safir::Utilities::AsioDispatcher m_dispatcher;

            Safir::Utilities::ForEach::Services m_service;

            Safir::Application::Tracer m_debug;

        private:

            //Disable copying and assignment
            ContextData(const ContextData&);
            ContextData& operator=(const ContextData&);

        };

        boost::asio::io_service m_ioService;

        typedef std::vector<boost::shared_ptr<ContextData> > ContextVector;

        ContextVector m_context;
    };
}
}
}
#endif
