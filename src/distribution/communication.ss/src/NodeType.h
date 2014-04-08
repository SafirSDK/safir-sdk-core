/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#ifndef __SAFIR_DOB_COMMUNICATION_NODETYPE_H__
#define __SAFIR_DOB_COMMUNICATION_NODETYPE_H__

#include <map>
#include <boost/shared_ptr.hpp>
#include <boost/asio.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    class NodeType
    {
    public:
        NodeType(const std::string& name,
                 const std::string& multicastAddr,
                 int heartBeatInterval,
                 int retryTimeout);

    private:
        boost::int64_t m_id;
        std::string m_name;               //unique readable name
        std::string m_multicastAddress;   //multicast address including port number, 'address:port' empty string if not multicast enabled
        boost::asio::ip::udp::endpoint m_multicastEndpoint;
        int m_heartBeatInterval;          //time between heartBeats
        int m_retryTimeout;               //time to wait before retransmitting unacked data
    };
}
}
}
}

#endif
