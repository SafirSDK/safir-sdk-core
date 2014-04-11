/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
#ifndef __SAFIR_DOB_COMMUNICATION_UTILIITIES_H__
#define __SAFIR_DOB_COMMUNICATION_UTILIITIES_H__

#include <string>
#include <boost/asio/ip/udp.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
namespace Utilities
{
    bool SplitAddress(const std::string& address, std::string& ip, unsigned short& port);

    boost::asio::ip::udp::endpoint CreateEndpoint(const std::string& address, int& ipVersion);

    boost::asio::ip::udp::endpoint CreateEndpoint(const std::string& address);

    boost::asio::ip::udp::endpoint::protocol_type Protocol(int p);
}
}
}
}
}

#endif
