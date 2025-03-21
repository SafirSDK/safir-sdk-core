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

#include <Safir/Utilities/Internal/VisibilityHelpers.h>
#include <utility>
#include <cstdint>
#include <string>
#include <vector>
#include <boost/chrono.hpp>

#ifdef system_picture_EXPORTS
#  define DISTRIBUTION_SYSTEM_PICTURE_API SAFIR_HELPER_DLL_EXPORT
#else
#  define DISTRIBUTION_SYSTEM_PICTURE_API SAFIR_HELPER_DLL_IMPORT
#endif
#define DISTRIBUTION_SYSTEM_PICTURE_LOCAL SAFIR_HELPER_DLL_LOCAL


namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{
    struct NodeType
    {
        NodeType(const int64_t id_,
                 std::string name_,
                 const bool isLightNode_,
                 const std::chrono::steady_clock::duration& heartbeatInterval_,
                 const int maxLostHeartbeats_,
                 const std::vector<std::chrono::steady_clock::duration>& retryTimeout_)
            : id(id_)
            , name(std::move(name_))
            , isLightNode(isLightNode_)
            , heartbeatInterval(heartbeatInterval_)
            , maxLostHeartbeats(maxLostHeartbeats_)
            , retryTimeout(retryTimeout_)
            , deadTimeout(maxLostHeartbeats_ * (heartbeatInterval_ + std::chrono::milliseconds(50)))
            , twoMinutesOfRetries(TwoMinutes(retryTimeout_)) {}

        const int64_t id;                                                //node type id
        const std::string name;                                          //readable name
        const bool isLightNode;                                          //is the node a light node
        const std::chrono::steady_clock::duration heartbeatInterval;   //time between heartbeats
        const int maxLostHeartbeats;                                     //number of heartbeats that can be lost before node is considered dead
        const std::vector<std::chrono::steady_clock::duration> retryTimeout;        //time to wait for replies.
        const std::chrono::steady_clock::duration deadTimeout;         //heartbeatInterval * maxLostHeartbeats + a little extra
        const size_t twoMinutesOfRetries;                                //Number of retries to make up two minutes

    private:
        static size_t TwoMinutes(const std::vector<std::chrono::steady_clock::duration>& retryTimeouts)
        {
            size_t retries = 0;
            std::chrono::steady_clock::duration twoMinutes = std::chrono::minutes(2);
            for (auto rt = retryTimeouts.cbegin(); rt != retryTimeouts.cend(); ++rt)
            {
                twoMinutes -= *rt;
                ++retries;
                if (twoMinutes < std::chrono::seconds(0))
                {
                    return retries;
                }
            }

            if (retryTimeouts.back() == std::chrono::seconds(0))
            {
                throw std::logic_error("Invalid timeout");
            }

            while(twoMinutes >= std::chrono::seconds(0))
            {
                twoMinutes -= retryTimeouts.back();
                ++retries;
            }

            return retries;
        }
    };
}
}
}
}

