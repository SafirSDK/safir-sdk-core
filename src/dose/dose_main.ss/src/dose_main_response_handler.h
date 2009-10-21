/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#ifndef _dose_main_response_handler_h
#define _dose_main_response_handler_h

#include <Safir/Dob/Internal/InternalFwd.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    //forward declarations
    class BlockingHandlers;
    class ExternNodeCommunication;

    class ResponseHandler
    {
    public:
        ResponseHandler();
        virtual ~ResponseHandler();

        void Init(BlockingHandlers & blockingHandler, ExternNodeCommunication& ecom);

        void DistributeResponses(const ConnectionPtr& sender);

        void HandleResponseFromDoseCom(const DistributionData& response) {HandleResponse(response);}

        //if the responses destination is a remote node false will be returned if there
        //is a dosecom overflow.
        bool HandleResponse(const DistributionData& response);

    private:
        void operator=(const ResponseHandler&) const; //Disable assignment operator

        void DispatchResponse(const DistributionData& response, bool & dontRemove, bool & doseComOverflowed, const ConnectionPtr & sender);
        void DispatchResponsesFromRequestInQueue(RequestInQueue & queue, const ConnectionPtr & sender);

        void PostResponse(const ConnectionPtr& receiver,
                          const DistributionData& response);

        ExternNodeCommunication* m_ecom;
        BlockingHandlers* m_blockingHandler;

        const Dob::Typesystem::Int32 m_thisNode;
    };
}
}
}
#endif

