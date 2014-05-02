/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Utilities/Internal/Id.h>
#include <boost/asio.hpp>
#include <boost/lexical_cast.hpp>
#include "CommunicationImpl.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    Communication::Communication(const boost::shared_ptr<boost::asio::io_service>& ioService,
                                 const std::string& nodeName,
                                 boost::int64_t nodeId, //0 is not a valid id.
                                 boost::int64_t nodeTypeId,
                                 const std::string& controlAddress,
                                 const std::string& dataAddress,
                                 bool isControlInstance,
                                 const std::vector<Communication::NodeType>& nodeTypes)
    {
        int ipVersion=4;
        if (isControlInstance)
        {
            ipVersion=Utilities::Protocol(controlAddress);
        }
        else
        {
            ipVersion=Utilities::Protocol(dataAddress);
        }

        //find own node type and check if we are multicast enabled
        auto nodeTypeIt=std::find_if(nodeTypes.cbegin(), nodeTypes.cend(), [=](const Communication::NodeType& n){return n.id==nodeTypeId;});
        if (nodeTypeIt==nodeTypes.end())
        {
            throw std::logic_error("Own nodeType does not exist "+std::string(__FILE__));
        }
        bool thisNodeIsMulticastEnabled=!nodeTypeIt->multicastAddress.empty();


        //create node type map
        NodeTypeMap nodeTypeMap;
        for (const auto& nt : nodeTypes)
        {
            bool useMulticast=(thisNodeIsMulticastEnabled && !nt.multicastAddress.empty());
            auto ptr=boost::make_shared<Safir::Dob::Internal::Com::NodeType>(ioService, nodeId, useMulticast, nt.id, nt.name, nt.multicastAddress, ipVersion, nt.heartbeatInterval, nt.retryTimeout);
            nodeTypeMap.insert(NodeTypeMap::value_type(nt.id, ptr));
        }

        //create impl object
        m_impl=boost::make_shared<CommunicationImpl>(ioService, nodeName, nodeId, nodeTypeId, controlAddress, dataAddress, isControlInstance, nodeTypeMap);
    }

    Communication::~Communication()
    {
    }

    //set callbacks
    void Communication::SetNewNodeCallback(const NewNode& callback)
    {
        m_impl->SetNewNodeCallback(callback);
    }

    void Communication::SetGotReceiveFromCallback(const GotReceiveFrom& callback)
    {
        m_impl->SetGotReceiveFromCallback(callback);
    }

    void Communication::SetRetransmitToCallback(const RetransmitTo& callback)
    {
        m_impl->SetRetransmitToCallback(callback);
    }

    void Communication::SetQueueNotFullCallback(const QueueNotFull& callback, int freePartThreshold)
    {
       m_impl->SetQueueNotFullCallback(callback, freePartThreshold);
    }

    void Communication::SetDataReceiver(const ReceiveData& callback, boost::int64_t dataTypeIdentifier)
    {
        m_impl->SetDataReceiver(callback, dataTypeIdentifier);
    }

    void Communication::Start()
    {
        m_impl->Start();
    }

    void Communication::Stop()
    {
        m_impl->Stop();
    }

    void Communication::InjectSeeds(const std::vector<std::string>& seeds)
    {
        m_impl->InjectSeeds(seeds);
    }

    void Communication::IncludeNode(boost::int64_t nodeId)
    {
        m_impl->IncludeNode(nodeId);
    }

    void Communication::ExcludeNode(boost::int64_t nodeId)
    {
        m_impl->ExcludeNode(nodeId);
    }

    bool Communication::SendToNode(boost::int64_t nodeTypeId, boost::int64_t nodeId, const boost::shared_ptr<char[]>& data, size_t size, boost::int64_t dataTypeIdentifier)
    {
        return m_impl->SendToNode(nodeTypeId, nodeId, data, size, dataTypeIdentifier);
    }

    bool Communication::SendToNodeType(boost::int64_t nodeTypeId, const boost::shared_ptr<char[]>& data, size_t size, boost::int64_t dataTypeIdentifier)
    {
        return m_impl->SendToNodeType(nodeTypeId, data, size, dataTypeIdentifier);
    }

    size_t Communication::NumberOfQueuedMessages(boost::int64_t nodeTypeId) const
    {
        return m_impl->NumberOfQueuedMessages(nodeTypeId);
    }

    const std::string& Communication::Name() const
    {
        return m_impl->Name();
    }

    boost::int64_t Communication::Id() const
    {
        return m_impl->Id();
    }
}
}
}
}
