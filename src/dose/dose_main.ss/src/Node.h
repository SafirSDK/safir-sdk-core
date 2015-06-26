/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / anders.widen@consoden.se
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

#include <string>
#include <unordered_set>
#include <cstdint>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    struct NodeInfo
    {
        NodeInfo(const std::string& _nodeName,
                 int64_t            _nodeId,
                 int64_t            _nodeTypeId,
                 const std::string& _dataAddress)
            : nodeName(_nodeName),
              nodeId(_nodeId),
              nodeTypeId(_nodeTypeId),
              dataAddress(_dataAddress)
        {}

        std::string nodeName;
        int64_t     nodeId;
        int64_t     nodeTypeId;
        std::string dataAddress;
    };

    typedef std::unordered_set<int64_t> NodeTypeIds;

}
}
}
