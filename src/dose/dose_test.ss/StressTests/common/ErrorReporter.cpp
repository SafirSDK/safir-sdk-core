/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
#include "ErrorReporter.h"
#include "../common/CommonIncludes.h"
#include <Safir/Utilities/ProcessInfo.h>
#include <sstream>
void ErrorReporter::Log(const std::string& text)
{
    try
    {
        std::ostringstream ostr;
        const pid_t pid = Safir::Utilities::ProcessInfo::GetPid();
        Safir::Utilities::ProcessInfo proc(pid);
        ostr << "== DOSE STRESSTEST LOG ========================================================" << std::endl
             << "This is an error message from one of the dose stress test programs. These logs " << std::endl
             << "are sent using UDP broadcast. If you didnt expect this message it probably" << std::endl
             << "means that someone is running dose stresstests on your LAN." << std::endl
             << "Pid = " << pid << std::endl
             << "Process Name = " << proc.GetProcessName() << std::endl
             << "Process Description = " << Safir::Utilities::ProcessInfo::GetProcessDescription() << std::endl
             << "Error Message:" << std::endl
             << text
             << std::endl
             << "===============================================================================" << std::endl;
        
        //Set up address
        const boost::asio::ip::address addr = 
            boost::asio::ip::address::from_string("255.255.255.255");
        const unsigned short port = 31221;
        const boost::asio::ip::udp::endpoint endpoint(addr, port);
        
        //Create socket and send            
        boost::asio::io_service service;
        boost::asio::ip::udp::socket sock(service, 
                                          boost::asio::ip::udp::endpoint(boost::asio::ip::address_v4::broadcast(), 0));
        sock.set_option(boost::asio::socket_base::broadcast(true));
        sock.send_to(boost::asio::buffer(ostr.str().c_str(),
                                         ostr.str().size()),
                     endpoint);
        

    }
    catch (...)
    {

    }
}
