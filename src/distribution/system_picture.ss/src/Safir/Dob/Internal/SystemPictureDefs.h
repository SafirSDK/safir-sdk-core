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
#ifndef __SYSTEM_PICTURE_EXPORT_DEFS_H__
#define __SYSTEM_PICTURE_EXPORT_DEFS_H__

#include <Safir/Utilities/Internal/VisibilityHelpers.h>
#include <utility>

#ifdef system_picture_EXPORTS
#  define DISTRIBUTION_SYSTEM_PICTURE_API SAFIR_HELPER_DLL_EXPORT
#else
#  define DISTRIBUTION_SYSTEM_PICTURE_API SAFIR_HELPER_DLL_IMPORT
#  define SAFIR_LIBRARY_NAME "system_picture"
#  include <Safir/Utilities/Internal/AutoLink.h>
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
                 const bool isLight_,
                 const int heartbeatInterval_, //in milliseconds
                 const int maxLostHeartbeats_)
            : id(id_)
            , name(std::move(name_))
            , isLight(isLight_)
            , heartbeatInterval(heartbeatInterval_)
            , maxLostHeartbeats(maxLostHeartbeats_) {}

        const int64_t id;           //node type id
        const std::string name;            //readable name
        const bool isLight;                //is the node a light node
        const int heartbeatInterval;       //time between heartbeats
        const int maxLostHeartbeats;       //number of heartbeats that can be lost before node is considered dead
    };
}
}
}
}

#endif
