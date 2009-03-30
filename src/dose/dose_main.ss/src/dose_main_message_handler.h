/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#ifndef _dose_main_message_handler_h
#define _dose_main_message_handler_h

#include "dose_main_communication.h"
#include "dose_main_blocking_handler.h"

#ifdef DispatchMessage
#undef DispatchMessage
#endif

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
        MessageHandler();

        void Init(BlockingHandlers & blockingHandler,
                  ExternNodeCommunication & ecom);

        void DistributeMessages(const ConnectionPtr & connection);

        void HandleMessageFromDoseCom(const DistributionData & msg);

    private:
        void DispatchMessage(size_t & numberDispatched,
                             const ConnectionPtr & connection,
                             const DistributionData & msg,
                             bool & exitDispatch,
                             bool & dontRemove);

        void TraverseMessageQueue(const ConnectionPtr & connection);
        ExternNodeCommunication* m_ecom;
        BlockingHandlers * m_blockingHandler;
    };
}
}
}
#endif
