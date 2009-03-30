/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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
#ifndef __SWRE_LOGGER_APP_H
#define __SWRE_LOGGER_APP_H

#include <Safir/Dob/Connection.h>
#include <Safir/Utilities/AceDispatcher.h>
#include <iostream>

#include "swre_report_handler.h"

namespace Safir
{
namespace Swre
{

    class ReportHandler;

    class LoggerApp :
        public Safir::Dob::StopHandler
    {
    public:
        LoggerApp();
        ~LoggerApp();

        int Run(const std::vector<std::string> & args);
    private:

        /*        // Implementation of application interface methods
        virtual void Startup();
        virtual void CloseDown();        */

        virtual void OnStopOrder();

        bool ParseCommandLine(const std::vector<std::string> & args);
        void Usage();

        ReportHandler m_reportHandler;
        Safir::Dob::Connection m_Connection;
        Safir::Utilities::AceDispatcher   m_dispatchEvent;

    };

}
}

#endif
