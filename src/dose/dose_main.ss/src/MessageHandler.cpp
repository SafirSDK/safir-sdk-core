/******************************************************************************
*
* Copyright Saab AB, 2007-2013,2015 (http://safir.sourceforge.net)
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

#include "MessageHandler.h"

#include <boost/shared_ptr.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/MessageTypes.h>
#include <Safir/Dob/NodeParameters.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    MessageHandler::MessageHandler(Distribution& distribution)
        : m_distribution(distribution),
          m_dataTypeIdentifier(LlufId_Generate64("MessageHandler"))
    {
        m_distribution.GetCommunication().SetDataReceiver([this]
                                                          (const int64_t fromNodeId,
                                                           int64_t /*fromNodeType*/,
                                                           const char* data,
                                                           size_t /*size*/)
                                                          {
                                                              const DistributionData msg =
                                                                  DistributionData::ConstConstructor(new_data_tag, data);

                                                              DistributionData::DropReference(data);

                                                              ENSURE(!m_distribution.IsLocal(msg.GetTypeId()),
                                                                     << "Received Local Message of type "
                                                                     << msg.GetTypeId()
                                                                     << " from node " << fromNodeId
                                                                     << ", system configuration is bad!");

                                                              MessageTypes::Instance().DistributeMsg(msg);
                                                          },
                                                          m_dataTypeIdentifier,
                                                          DistributionData::NewData);
    }

    void MessageHandler::DistributeMessages(const ConnectionPtr & connection)
    {
        lllout << "Distributing messages for connection " << connection->Id() << "." << std::endl;
        TraverseMessageQueue(connection);
    }


    void MessageHandler::DispatchMessage(size_t& numberDispatched,
                                         const ConnectionPtr& connection,
                                         const DistributionData& msg,
                                         bool& exitDispatch,
                                         bool& dontRemove)
    {
        ENSURE (msg.GetType() == DistributionData::Message, <<
                "MessageHandler found a DistributionData that is not of type Message!");

        exitDispatch = false;
        dontRemove = false;

        Send(msg);

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

    void MessageHandler::TraverseMessageQueue(const ConnectionPtr & connection)
    {
        size_t noPopped = 0;

        connection->GetMessageOutQueue().Dispatch
            ([this,&noPopped,&connection] (const DistributionData& msg,
                                           bool& exitDispatch,
                                           bool& dontRemove)
             {
                 this->DispatchMessage(noPopped,
                                 connection,
                                 msg,
                                 exitDispatch,
                                 dontRemove);
             },
             [&connection]{connection->SignalIn();});
    }

    void MessageHandler::Send(const DistributionData& msg)
    {
        if (m_distribution.IsLocal(msg.GetTypeId()) ||
            Safir::Dob::NodeParameters::LocalContexts(msg.GetSenderId().m_contextId) == true)
        {
            return;
        }

        boost::shared_ptr<const char[]> msgP(msg.GetReference(),
                                             [](const char* data)
                                             {
                                                 DistributionData::DropReference(data);
                                             });

        // Send message to all node types
        for (auto nodeType = m_distribution.GetNodeTypeIds().cbegin(); nodeType != m_distribution.GetNodeTypeIds().cend(); ++nodeType)
        {
            m_distribution.GetCommunication().Send(0,  // All nodes of the type
                                                   *nodeType,
                                                   msgP,
                                                   msg.Size(),
                                                   m_dataTypeIdentifier,
                                                   false); // no delivery guarantee for messages
        }
    }
}
}
}
