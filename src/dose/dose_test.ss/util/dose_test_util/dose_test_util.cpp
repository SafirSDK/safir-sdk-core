/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
*
* Created by: Mikael Wennerberg / stmiwn
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
#include <iostream>
#include "dose_test_util.h"
#include "dose_com_utils.h"
#include <Safir/Dob/Typesystem/Utilities.h>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable: 4244)
#endif

#include <ace/SOCK_Dgram_Mcast.h>
#include <ace/OS_NS_sys_socket.h>

#if defined _MSC_VER
  #pragma warning (pop)
#endif

ACE_SOCK_Dgram_Mcast g_sock;

void InhibitOutgoingTraffic(const bool inhibit, bool& success)
{
    DOSE_SHARED_DATA_S * pShm = (DOSE_SHARED_DATA_S *) Get_NodeSharedData_Pointer();  

    if (pShm->MyIpAddr_nw == 0)
    {
        std::wcout << "No dose_main running!"  << std::endl;
        success = false;
        return;
    }

    pShm->InhibitOutgoingTraffic = inhibit;     
    success = true;
}


void InhibitOutgoingTrafficStatus(bool& isInhibited)
{
    DOSE_SHARED_DATA_S * pShm = (DOSE_SHARED_DATA_S *) Get_NodeSharedData_Pointer();  
    if (pShm->InhibitOutgoingTraffic == 0)
    {
        isInhibited = false;
    }
    else
    {
        isInhibited = true;
    }
}

void JoinMulticastGroup(const char* const multicastAddress,
                        const int         multicastPort,
                        const char* const multicastNic)
{
    ACE_INET_Addr addr((unsigned short)multicastPort, multicastAddress);

    ACE_OS::socket_init();

    std::wcout << "Joined socket to group for multicast reception. Multicast address " << multicastAddress << ", port " << multicastPort << "." << std::endl;
    if (*multicastNic == 0)
    {
        std::wcout << "NIC is not set. Windows: Listen on all interfaces. Linux: Listen on default interface." << std::endl;
    }
    else
    {
        std::wcout << "Used NIC: " << Safir::Dob::Typesystem::Utilities::ToWstring(multicastNic) << std::endl;
    }

    int res;
    if (*multicastNic == 0)
    {
        res = g_sock.join(addr);
    }
    else
    {
        res = g_sock.join(addr,
                          1,  // 1 => reuse address
                          multicastNic);
    }
    if (res == -1)
    {
        std::wcout << "Error joining multicast group!" << std::endl;
    }

    //g_sock.enable(ACE_NONBLOCK);        
}


void ReceiveMulticastPacket(char* buf,
                            int bufLen)
{
    ACE_INET_Addr from;
    size_t res = g_sock.recv(buf, bufLen, from);

    if (res == -1)
    {
        buf[0] = 0;
        std::wcout << "recv returned error code " << ACE_OS::last_error() << std::endl;      
    }
}


