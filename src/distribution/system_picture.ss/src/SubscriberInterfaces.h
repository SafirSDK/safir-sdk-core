/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{
    /**
     * Interface to an object that allows or subscriptions to raw statistics objects.
     *
     * This interface is not intended for users to implement. Instead use the SystemPicture 
     * class to obtain an instance of a class that implements this interface.
     */
    class RawStatisticsSubscriber
    {
    public:
        typedef RawStatistics DataWrapper;

        virtual void Start(const boost::function<void (const RawStatistics& data)>& dataCallback) = 0;

        virtual void Stop() = 0;
    };

    /**
     * Interface to an object that allows or subscriptions to system state objects.
     *
     * This interface is not intended for users to implement. Instead use the SystemPicture 
     * class to obtain an instance of a class that implements this interface.
     */
    class SystemStateSubscriber
    {
    public:
        typedef SystemState DataWrapper;

        /** Start a subscription to system states. */
        virtual void Start(const boost::function<void (const SystemState& data)>& dataCallback) = 0;

        /** Stop the subscription. */
        virtual void Stop() = 0;
    };


}
}
}
}


