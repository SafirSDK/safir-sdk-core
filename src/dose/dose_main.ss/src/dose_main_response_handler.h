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

namespace Safir
{
namespace Dob
{
namespace Internal
{
    //forward declarations
    class BlockingHandlers;
    class TimerHandler;
    namespace Com
    {
        class Communication;
    }

    class ResponseHandler
        : private boost::noncopyable
    {
    public:
        ResponseHandler(TimerHandler& timerHandler,
                        BlockingHandlers& blockingHandler,
                        Com::Communication& communication);

        virtual ~ResponseHandler();

        void DistributeResponses(const ConnectionPtr& sender);

        void HandleResponseFromDoseCom(const DistributionData& response) {HandleResponse(response);}

        //if the responses destination is a remote node false will be returned if there
        //is a dosecom overflow.
        bool HandleResponse(const DistributionData& response);

    private:
        void DispatchResponse(const DistributionData& response, bool & dontRemove, bool & doseComOverflowed, const ConnectionPtr & sender);
        void DispatchResponsesFromRequestInQueue(RequestInQueue & queue, const ConnectionPtr & sender);

        void PostResponse(const ConnectionPtr& receiver,
                          const DistributionData& response);

        TimerHandler& m_timerHandler;

        BlockingHandlers& m_blockingHandler;

        Com::Communication& m_communication;

    };
}
}
}

