/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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
#pragma once

#include <Safir/Dob/Internal/InternalFwd.h>
#include "Distribution.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
    /**
     * This class handles distribution of responses within local and remote nodes.
     *
     * All methods are thread safe.
     */
    class ResponseHandler
        : private boost::noncopyable
    {
    public:
        ResponseHandler(boost::asio::io_service& ioService,
                        Distribution& distribution,
                        const std::function<void(const ConnectionId& connectionId,
                                                 const InternalRequestId requestId)>& responsePostedCallback);

        /**
         * Distribute any responses the given connection has in its request in queues.
         */
        void DistributeResponses(const ConnectionPtr& sender);

        /**
         * Send a response.
         *
         * This can be used by the request handler to send automatic responses, e.g. when
         * a service is not registered or a timeout occurs.
         */
        bool SendResponse(const DistributionData& response);

    private:
#if 0 //stewart
        void HandleResponseFromDoseCom(const DistributionData& response) {HandleResponse(response);}
#endif

        void DispatchResponse(const DistributionData& response,
                              bool & dontRemove,
                              bool & communicationOverflow,
                              const ConnectionPtr & sender);
        void DispatchResponsesFromRequestInQueue(RequestInQueue & queue, const ConnectionPtr & sender);

        void PostResponse(const ConnectionPtr& receiver,
                          const DistributionData& response);

        boost::asio::strand m_strand;
        Distribution& m_distribution;
        const std::function<void(const ConnectionId& connectionId,
                                 const InternalRequestId requestId)> m_responsePostedCallback;
    };
}
}
}

