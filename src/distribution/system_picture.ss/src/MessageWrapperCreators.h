/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
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
#pragma once

#include <Safir/Dob/Internal/RawStatistics.h>
#include <Safir/Dob/Internal/SystemState.h>
#include <memory>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{
    //forward declaration
    class RawStatisticsMessage;
    class SystemStateMessage;

    class RawStatisticsCreator
    {
    public:
        typedef RawStatisticsMessage WrappedType;

        static RawStatistics Create(std::unique_ptr<WrappedType> message);

    };

    class SystemStateCreator
    {
    public:
        typedef SystemStateMessage WrappedType;

        static SystemState Create(std::unique_ptr<WrappedType> message);

    };

}
}
}
}

