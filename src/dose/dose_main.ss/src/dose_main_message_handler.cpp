/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#include "dose_main_message_handler.h"

#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/MessageTypes.h>



namespace Safir
{
namespace Dob
{
namespace Internal
{
    MessageHandler::MessageHandler():
#if 0 //stewart
        m_ecom(NULL),
#endif
        m_blockingHandler(NULL)
    {
    }

    void MessageHandler::Init(BlockingHandlers & blockingHandler)
#if 0 //stewart
                              ExternNodeCommunication & ecom)
#endif
    {
        m_blockingHandler = &blockingHandler;
#if 0 //stewart
        m_ecom = &ecom;
#endif
    }



    void MessageHandler::DistributeMessages(const ConnectionPtr & connection)
    {
        lllout << "Distributing messages for connection " << connection->Id() << "." << std::endl;
        TraverseMessageQueue(connection);
    }


    void MessageHandler::DispatchMessage(size_t & numberDispatched, const ConnectionPtr & connection, const DistributionData & msg, bool & exitDispatch, bool & dontRemove)
    {
        exitDispatch = false;
        dontRemove = false;

#if 0 //stewart
        if (!m_ecom->Send(msg))
        {
            m_blockingHandler->Message().AddWaitingConnection(ExternNodeCommunication::DoseComVirtualConnectionId,connection->Id().m_id);
            dontRemove = true;
            exitDispatch = true;
            return;
        }
#endif

        lllout << "DOSE_MAIN has found a message in msg out queue for connection " << connection->Id() << std::endl;

        MessageTypes::Instance().DistributeMsg(msg);
        ++numberDispatched;

        //If we have dispatched more than the connection queue length of messages
        //it is probably time to serve other connections too.
        //We set the write event of the connection and break out of this loop.
        //Since the event has been set we will return to this connection as soon
        //as we've served the other connections.
        if (numberDispatched > connection->GetMessageOutQueue().capacity())
        {
            connection->SignalOut();
            lllout << "Have dispatched " << numberDispatched << " messages from " << connection->NameWithCounter()
                << ", time to serve other apps too. Setting the event to return to this connection later" <<std::endl;
            exitDispatch = true;
        }
    }

    void PostFullAction(const ConnectionPtr & connection)
    {
        connection->SignalIn();
    }

    void MessageHandler::TraverseMessageQueue(const ConnectionPtr & connection)
    {
        size_t noPopped = 0;

        connection->GetMessageOutQueue().Dispatch
            (boost::bind(&MessageHandler::DispatchMessage,this,boost::ref(noPopped),boost::cref(connection),_1,_2,_3),
            boost::bind(PostFullAction,boost::cref(connection)));
    }

    void MessageHandler::HandleMessageFromDoseCom(const DistributionData & msg)
    {
        MessageTypes::Instance().DistributeMsg(msg);
    }

}
}
}
