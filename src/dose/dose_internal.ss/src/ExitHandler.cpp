/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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

#include "ExitHandler.h"
#include <ace/Signal.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    // the singleton has to be instantiated with a pointer, since if it is just a static
    // inside Instance() it will get destroyed before the exit handler is called.
    ExitHandler * ExitHandler::m_instance = NULL;

    ExitHandler & ExitHandler::Instance()
    {
        if (m_instance == NULL)
        {
            m_instance = new ExitHandler();
        }
        return *m_instance;
    }

    ExitHandler::ExitHandler()
    {
        ACE_Sig_Set sigset;
        sigset.sig_add(SIGABRT);
        sigset.sig_add(SIGFPE);
        sigset.sig_add(SIGILL);
        sigset.sig_add(SIGINT);
        sigset.sig_add(SIGSEGV);
        sigset.sig_add(SIGTERM);
        ACE_Sig_Action sig(sigset,&SignalFunc);
        atexit(ExitFunc);
    }

    ExitHandler::~ExitHandler()
    {
        lllout << "ExitHandler destructor called!!!"<<std::endl;
    }

    void ExitHandler::SignalFunc(const int signal)
    {
        lllout << "Running SignalFunc! signal = " << signal << std::endl;
        Instance().DoCleanup();
        _exit(-1);
    }

    void ExitHandler::ExitFunc()
    {
        lllout << "Running ExitFunc!" << std::endl;
        Instance().DoCleanup();
    }

    void ExitHandler::DoCleanup()
    {
        for (Connections::iterator it = m_connections.begin();
             it != m_connections.end(); ++it)
        {
            lllout << "ExitHandler::DoCleanup for connection " << it->first->NameWithCounter() << " id = " << it->first->Id() << std::endl;
            ENSURE(!it->second.empty(), << "Found an empty cleanupFunc!");
            it->second(it->first.get().get());
        }
        m_connections.clear();
    }

    void ExitHandler::AddFunction(const ConnectionPtr & connection, CleanupFunction cleanupFunction)
    {
        ENSURE(connection != NULL, << "Cannot add NULL connection to ExitHandler!");
        ENSURE(!cleanupFunction.empty(), << "Cannot add empty cleanupFunction for connection " << connection->NameWithCounter() << " to ExitHandler!");
        m_connections.insert(std::make_pair(connection,cleanupFunction));
    }

    void ExitHandler::RemoveConnection(const ConnectionPtr & connection)
    {
        m_connections.erase(connection);
    }

}
}
}

