/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
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

#include "ActionSender.h"
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/NodeParameters.h>
#include <DoseTest/Parameters.h>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable: 4244)
#endif

#include <ace/OS_NS_sys_socket.h>

#if defined _MSC_VER
  #pragma warning (pop)
#endif

#include <iostream>

ActionSender::ActionSender(const std::string& multicastNic)
    : m_seqNbr(0)
{
    m_connection.Attach();

    // Port and address must correspond to what is used by the partners
    const unsigned int cPort = 31789;
    const std::wstring cMulticastAddr = DoseTest::Parameters::TestMulticastAddress();

    ACE_OS::socket_init();

    ACE_INET_Addr addr;
    addr.set(cPort, 
             cMulticastAddr.c_str());

    int ttl = Safir::Dob::NodeParameters::RoutingHops();
    static_cast<ACE_SOCK&>(m_sock).set_option(IPPROTO_IP, IP_MULTICAST_TTL, (void*) &ttl, sizeof(ttl));

    std::wcout << "Opening socket for multicast send. Multicast address " << cMulticastAddr << ", port " << cPort << "." << std::endl;
    if (multicastNic.empty())
    {
        std::wcout << "NIC is not set which means that the system default interface will be used" << std::endl;
    }
    else
    {
        std::wcout << "Used NIC: " << Safir::Dob::Typesystem::Utilities::ToWstring(multicastNic) << std::endl;
    }

    int res;
    if (multicastNic.empty())
    {
        res = m_sock.open(addr);
    }
    else
    {
        res = m_sock.open(addr, multicastNic.c_str());
    }

    if (res == -1)
    {
        std::wcout << "Error opening socket for multicast send!" << std::endl;
    }
    
}

ActionSender::~ActionSender()
{
}

void ActionSender::Send(const DoseTest::ActionPtr& msg)
{
    ++m_seqNbr;
    msg->SeqNbr().SetVal(m_seqNbr);

    Safir::Dob::Typesystem::BinarySerialization binary;

    Safir::Dob::Typesystem::Serialization::ToBinary(msg, binary);

    if (binary.size() > 63000)
    {
        // Can't send messages that don't fit in a UDP datagram via the socket. In this case
        // we send the action via the DOB.
        m_connection.Send(msg, msg->Partner(), this);
    }
    else
    {

        if (m_sock.send(&binary[0], binary.size()) == -1)
        {
            std::wcout << "Error " <<  ACE_OS::last_error() << " when sending on multicast address" << std::endl;
        }
    }
}

void ActionSender::OnNotMessageOverflow()
{
    std::wcout << "On_Not_Message_Overflow"
               << std::endl;
}
