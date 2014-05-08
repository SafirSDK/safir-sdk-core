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
#ifndef __SAFIR_DOB_COMMUNICATION_NODE_H__
#define __SAFIR_DOB_COMMUNICATION_NODE_H__

#include <iostream>
#include <map>
#include <set>
#include <vector>
#include <boost/shared_ptr.hpp>
#include <boost/asio.hpp>
#include "Utilities.h"

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
        boost::int64_t nodeId;
        boost::int64_t nodeTypeId;
        const std::string controlAddress;
        const std::string dataAddress;
        const std::string unicastAddress; //will be controlAddress or dataAddress depending on the mode
        bool systemNode;

        Node(const std::string& name_,
             boost::int64_t nodeId_,
             boost::int64_t nodeTypeId_,
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
        {
        }

    };

    typedef std::map<boost::int64_t, Node> NodeMap;
}
}
}
}

#endif
