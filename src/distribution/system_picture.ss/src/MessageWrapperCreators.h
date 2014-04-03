/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
#ifndef __MESSAGE_WRAPPER_CREATORS_H__
#define __MESSAGE_WRAPPER_CREATORS_H__

#include <Safir/Dob/Internal/RawStatistics.h>
#include <Safir/Dob/Internal/SystemState.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{
    //forward declaration
    class NodeStatisticsMessage;
    class SystemStateMessage;

    class RawStatisticsCreator
    {
    public:
        static RawStatistics Create(const boost::shared_ptr<NodeStatisticsMessage>& message);

    };

    class SystemStateCreator
    {
    public:
        static SystemState Create(const boost::shared_ptr<SystemStateMessage>& message);

    };

}
}
}
}

#endif

