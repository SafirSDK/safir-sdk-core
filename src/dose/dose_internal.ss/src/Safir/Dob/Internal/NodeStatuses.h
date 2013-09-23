/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / stawi
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
#ifndef __DOSE_NODE_STATUS_H__
#define __DOSE_NODE_STATUS_H__

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/NodeStatus.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API NodeStatuses:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    private:
        //This is to make sure that only Instance can call the constructor even though the constructor
        //itself has to be public (limitation of boost::interprocess)
        struct private_constructor_t {};
    public:
        static void Initialize();

        static NodeStatuses& Instance();

        typedef std::vector<Dob::NodeStatus::Enumeration> Status;

        // Set status for a single node
        void SetNodeStatus(const Dob::Typesystem::Int32 nodeNumber, NodeStatus::Enumeration status);

        // Get status for a single node
        NodeStatus::Enumeration GetNodeStatus(const Dob::Typesystem::Int32 nodeNumber) const;

        //Get the current status of all nodes. Own node is indicated as down!!!
        Status GetNodeStatuses() const;

        // Returns true if one or more nodes has the given status
        bool AnyNodeHasStatus(NodeStatus::Enumeration status) const;

        //The constructor and destructor have to be public for the boost::interprocess internals to be able to call
        //them, but we can make the constructor "fake-private" by making it require a private type as argument.
        explicit NodeStatuses(private_constructor_t);
    private:
        typedef Containers<NodeStatus::Enumeration>::vector NodeStatusVec;

        int             m_nbrOfNodes;
        NodeStatusVec   m_nodeStatuses;

        static NodeStatuses* m_instance;
    };
}
}
}

#endif

