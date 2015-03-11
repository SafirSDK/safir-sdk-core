/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
*
* Created by: Lars Hagström / stlrha
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

#include "Node.h"
#include "dose_main_blocking_handler.h"
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/Communication.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class MessageHandler:
        private boost::noncopyable
    {
    public:
        MessageHandler(Com::Communication& communication,
                       const NodeTypeIds& nodeTypeIds);

        void DistributeMessages(const ConnectionPtr & connection);

        void HandleMessageFromDoseCom(const DistributionData & msg);

    private:
        void DispatchMessage(size_t & numberDispatched,
                             const ConnectionPtr & connection,
                             const DistributionData & msg,
                             bool & exitDispatch,
                             bool & dontRemove);

        void TraverseMessageQueue(const ConnectionPtr & connection);

        void Send(const DistributionData& msg);
        void Receive(int64_t fromNodeId, int64_t fromNodeType, const char* data, size_t size);

        Com::Communication& m_communication;
        const NodeTypeIds   m_nodeTypeIds;
        const int64_t       m_dataTypeIdentifier;
    };
}
}
}
