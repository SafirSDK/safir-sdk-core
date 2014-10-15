/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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

#include <boost/function.hpp>
#include <Safir/Dob/Internal/SystemState.h>

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
namespace Control
{

    struct Node
    {
        Node(const std::string& name_,
             int64_t nodeId_,
             int64_t nodeTypeId_,
             const std::string& controlAddress_,
             const std::string& dataAddress_)
            : name(name_),
              nodeId(nodeId_),
              nodeTypeId(nodeTypeId_),
              controlAddress(controlAddress_),
              dataAddress(dataAddress_)
        {
        }

        std::string         name;
        int64_t             nodeId;
        int64_t             nodeTypeId;
        const std::string   controlAddress;
        const std::string   dataAddress;
    };

    /**
     * @brief The SystemStateHandler class holds the current system state. When a new
     * state is set this class figures out what has happened and the appropriate
     * callbacks are called.
     */
    class SystemStateHandler
    {
    public:
        //TODO: beskriv exakta betydelsen controlkanal uppe
        typedef boost::function<void(const Node& node)> NodeIncludedCb;
        //TODO: beskriv exakta betydelsen nod nere, controlkanal och datakanal är nere eller kommer tas ner snart.
        typedef boost::function<void(const int64_t nodeId)> NodeDownCb;

        explicit SystemStateHandler(boost::asio::io_service::strand& strand,
                                    const NodeIncludedCb&            nodeIncludedCb,
                                    const NodeDownCb&                nodeDownCb);

        void SetNewState(const Safir::Dob::Internal::SP::SystemState& newState);

    private:

        boost::asio::io_service::strand& m_strand;

        std::map<int64_t, Node> m_systemState;

        NodeIncludedCb          m_nodeIncludedCb;
        NodeDownCb              m_nodeDownCb;

    };

}
}
}
}



