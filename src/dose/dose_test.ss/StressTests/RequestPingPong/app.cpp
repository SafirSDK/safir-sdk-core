/******************************************************************************
*
* Copyright Saab AB, 2006-2011 (http://www.safirsdk.com)
*
* Created by: Mikael Wennerberg/ stmiwn
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
#include <iostream>

App::App(const std::vector<std::string> & commandLine): m_dispatch(m_Connection,m_ioService)
{
    if (commandLine.size() < 3)
    {
        PrintHelp();
        exit(1);
    }
 
    wchar_t t[8];
    memset(t, 0, 8*sizeof(wchar_t));
    mbtowc(t, commandLine[1].c_str(), commandLine[1].length());

    m_Connection.Open(L"dose_test_reg_pingpong", t, 0, this, &m_dispatch);

    if (commandLine[2].compare("req") == 0)
    {
        m_requestor.Start();
    }
    else if (commandLine[2].compare("handler") == 0)
    {
        m_handler.Start();
    }
    else
    {
        PrintHelp();
        exit(1);
    }

}

void App::PrintHelp()
{
    std::wcout << "Usage: dose_test_reg_pingpong <instance> [req]|[handler]" <<std::endl;
}

void App::OnStopOrder()
{
    m_ioService.stop();
}



void App::Run()
{
    boost::asio::io_service::work keepRunning(m_ioService);
    m_ioService.run();
}
