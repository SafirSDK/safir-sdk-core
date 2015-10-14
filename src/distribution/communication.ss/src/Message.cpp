/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include "Message.h"
#include <boost/static_assert.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{

    BOOST_STATIC_ASSERT(CommonHeaderSize == 3*8);
    BOOST_STATIC_ASSERT(sizeof(Heartbeat) == CommonHeaderSize);
    BOOST_STATIC_ASSERT(sizeof(Ack) == CommonHeaderSize + 8 + 1 + Parameters::SlidingWindowSize);
    BOOST_STATIC_ASSERT(sizeof(MessageHeader) == CommonHeaderSize
                        + 8 + 6 * 4);
}
}
}
}
