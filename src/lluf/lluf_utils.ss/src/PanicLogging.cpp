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

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  //TODO: which do we need to disable for asio?
#endif

#include <boost/asio.hpp>

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
            //prepare message
            std::ostringstream ostr;
            const pid_t pid = ProcessInfo::GetPid();
            ProcessInfo proc(pid);
            ostr << "== PANIC LOG! =================================================================" << std::endl
                 << "Pid = " << pid << std::endl
                 << "Process Name = " << proc.GetProcessName() << std::endl
                 << "Process Description = " << Safir::Utilities::ProcessInfo::GetProcessDescription() << std::endl
                 << "Error Message:" << std::endl
                 << text
                 << std::endl
                 << "===============================================================================" << std::endl;

            //Set up address
            const boost::asio::ip::address addr = 
              boost::asio::ip::address::from_string("127.0.0.1");
            const unsigned short port = 31221;
            const boost::asio::ip::udp::endpoint endpoint(addr, port);

            //Create socket and send            
            boost::asio::io_service service;
            boost::asio::ip::udp::socket sock(service, 
                                              boost::asio::ip::udp::endpoint(boost::asio::ip::udp::v4(), 0));
            sock.send_to(boost::asio::buffer(ostr.str().c_str(),
                                             ostr.str().size()),
                         endpoint);
        }
        catch (...)
        {

        }
    }
}
}
}
