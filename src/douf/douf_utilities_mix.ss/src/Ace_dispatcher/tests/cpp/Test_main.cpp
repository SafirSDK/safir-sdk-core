/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Erik Adolfsson / sterad
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
#if defined(_WIN32) || defined(__WIN32) || defined(__WIN32__)
#ifdef _WIN32_WINNT
#undef _WIN32_WINNT
#endif
#define _WIN32_WINNT 0x0501
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN    
#endif
#endif

#include <iostream>
#include <Safir/Application/AceReactorEventLoop.h>
#include "app.h"

int main(int argc, char* argv[])
{
    std::wcout << "ACE Dispatcher : ";
    // Create an own event loop
    ACE_Reactor m_reactor;
    boost::shared_ptr<Safir::Application::AceReactorEventLoop> loop = 
        boost::shared_ptr<Safir::Application::AceReactorEventLoop>(new Safir::Application::AceReactorEventLoop(&m_reactor));

    // Run the application
    App app(loop);
    app.Test();

    return 0;

}
