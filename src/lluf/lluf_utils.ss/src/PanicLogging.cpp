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
#include <Safir/Utilities/Internal/PanicLogging.h>

//disable warnings in ace
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4267)
#endif

#include <ace/Thread.h>
#include <ace/Process.h>
#include <ace/SOCK_Dgram.h>
#include <ace/OS_NS_sys_socket.h>
#include <ace/OS_NS_unistd.h>

//and enable the warnings again
#if defined _MSC_VER
  #pragma warning (pop)
#endif

#include <Safir/Utilities/ProcessInfo.h>
#include <sstream>
namespace Safir
{
namespace Utilities
{
namespace Internal
{
    void PanicLogging::Log(const std::string& text)
    {
        try
        {
            ACE_OS::socket_init();
            ACE_INET_Addr addr;
            addr.set(31221,ACE_LOCALHOST);
            ACE_SOCK_Dgram sock;
            sock.open(ACE_Addr::sap_any,addr.get_type());
            std::ostringstream ostr;
            const pid_t pid = ACE_OS::getpid();
            ProcessInfo proc(pid);
            ostr << "== PANIC LOG! =================================================================" << std::endl
                 << "Pid = " << pid << std::endl
                 << "Process Name = " << proc.GetProcessName() << std::endl
                 << "Process Description = " << proc.GetProcessDescription() << std::endl
                 << "Error Message:" << std::endl
                 << text
                 << std::endl
                 << "===============================================================================" << std::endl;
            sock.send(ostr.str().c_str(),ostr.str().size(),addr);
            sock.close();
        }
        catch (...)
        {

        }
    }
}
}
}
