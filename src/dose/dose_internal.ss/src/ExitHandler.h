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

#ifndef __DOSE_EXIT_HANDLER_H__
#define __DOSE_EXIT_HANDLER_H__

#include <boost/noncopyable.hpp>
#include <boost/function.hpp>
#include <map>
#include <Safir/Dob/Internal/InternalFwd.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    /**
     * Class that helps ensuring that dose_main gets told when applications
     * that have open connections exit.
     *
     * This class is located in process memory.
     * Note that this class is not thread-safe, but it is only
     * meant to be called when the connect mutex is locked in Connections.
     */
    class ExitHandler:
        private boost::noncopyable
    {
    public:
        static ExitHandler & Instance();

        typedef boost::function<void (Connection*)> CleanupFunction;

        /**
         * Add a function to be called when the process exits.
         */
        void AddFunction(const ConnectionPtr & connection, CleanupFunction cleanupFunction);

        /**
         * Remove all functions for a connection.
         */
        void RemoveConnection(const ConnectionPtr & connection);
    private:
        ExitHandler();
        ~ExitHandler();

        void DoCleanup();

        static void SignalFunc(int signal);
        static void ExitFunc();
        typedef std::multimap<ConnectionPtr, CleanupFunction> Connections;
        Connections m_connections;

        static ExitHandler * m_instance;
    };
}
}
}

#endif

