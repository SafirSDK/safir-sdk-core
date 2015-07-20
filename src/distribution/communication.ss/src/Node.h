/******************************************************************************
*
* Copyright Saab AB, 2013-2015 (http://safir.sourceforge.net)
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
#pragma once

#include <iostream>
#include <map>
#include <set>
#include <vector>
#include <boost/shared_ptr.hpp>
#include "Utilities.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    struct Node
    {
        std::string name;
        int64_t nodeId;
        int64_t nodeTypeId;
        const std::string controlAddress;
        const std::string dataAddress;
        const std::string unicastAddress; //will be controlAddress or dataAddress depending on the mode
        bool systemNode;
        bool isSeed; //is the node a seed to this Communication instance.

        Node(const std::string& name_,
             int64_t nodeId_,
             int64_t nodeTypeId_,
             const std::string& controlAddress_,
             const std::string& dataAddress_,
             bool isControlInstance)
            :name(name_)
            ,nodeId(nodeId_)
            ,nodeTypeId(nodeTypeId_)
            ,controlAddress(controlAddress_)
            ,dataAddress(dataAddress_)
            ,unicastAddress(isControlInstance ? controlAddress : dataAddress)
            ,systemNode(false)
            ,isSeed(false)
        {
        }

    };

    typedef std::map<int64_t, Node> NodeMap;
}
}
}
}
