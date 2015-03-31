/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include <boost/lexical_cast.hpp>
#include "CommunicationImpl.h"

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
namespace Com
{
namespace
{
    std::unique_ptr<CommunicationImpl> Init(bool isControlInstance,
                                              boost::asio::io_service& ioService,
                                              const std::string& nodeName,
                                              int64_t nodeId, //0 is not a valid id.
                                              int64_t nodeTypeId,
                                              const std::string& controlAddress,
                                              const std::string& dataAddress,
                                              const std::vector<NodeTypeDefinition>& nodeTypes)
    {
        //find address of local interface to use
        Resolver resolver(ioService);

        const auto controlAddressResolved=(isControlInstance ? resolver.ResolveLocalEndpoint(controlAddress) : "");
        const auto dataAddressResolved=resolver.ResolveLocalEndpoint(dataAddress);
        const auto localIf=(isControlInstance ? controlAddressResolved : dataAddressResolved);

        //find out if we are using ip4 or ip6
        const int ipVersion=Resolver::Protocol(localIf);

        //find own node type and check if we are multicast enabled
        auto nodeTypeIt=std::find_if(nodeTypes.cbegin(), nodeTypes.cend(), [=](const NodeTypeDefinition& n){return n.id==nodeTypeId;});
        if (nodeTypeIt==nodeTypes.end())
        {
            throw std::logic_error("Own nodeType does not exist "+std::string(__FILE__));
        }
        bool thisNodeIsMulticastEnabled=!(isControlInstance ? nodeTypeIt->controlMulticastAddress.empty() : nodeTypeIt->dataMulticastAddress.empty());

        //create node type map
        NodeTypeMap nodeTypeMap;
        for (const auto& nt : nodeTypes)
        {
            const std::string& mc=isControlInstance ? nt.controlMulticastAddress : nt.dataMulticastAddress;
            bool useMulticast=(thisNodeIsMulticastEnabled && !mc.empty());
            auto ptr=boost::make_shared<Safir::Dob::Internal::Com::NodeType>(ioService,
                                                                             nodeId,
                                                                             localIf,
                                                                             useMulticast,
                                                                             nt.id,
                                                                             nt.name,
                                                                             mc,
                                                                             ipVersion,
                                                                             nt.heartbeatInterval,
                                                                             nt.maxLostHeartbeats,
                                                                             nt.retryTimeout);
            nodeTypeMap.insert(NodeTypeMap::value_type(nt.id, ptr));
        }

        //create impl object
        return std::unique_ptr<CommunicationImpl>(new CommunicationImpl(ioService, nodeName, nodeId, nodeTypeId, controlAddressResolved, dataAddressResolved, isControlInstance, nodeTypeMap));
    }
}

    Communication::Communication(ControlModeTag,
                  boost::asio::io_service& ioService,
                  const std::string& nodeName,
                  int64_t nodeId, //0 is not a valid id.
                  int64_t nodeTypeId,
                  const std::string& controlAddress,
                  const std::string& dataAddress,
                  const std::vector<NodeTypeDefinition>& nodeTypes)
        :m_impl(Init(true, ioService, nodeName, nodeId, nodeTypeId, controlAddress, dataAddress, nodeTypes))
    {
    }


    Communication::Communication(DataModeTag,
                                 boost::asio::io_service& ioService,
                                 const std::string& nodeName,
                                 int64_t nodeId, //0 is not a valid id.
                                 int64_t nodeTypeId,
                                 const std::string& dataAddress,
                                 const std::vector<NodeTypeDefinition>& nodeTypes)
        :m_impl(Init(false, ioService, nodeName, nodeId, nodeTypeId, "", dataAddress, nodeTypes))
    {
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

    void Communication::SetQueueNotFullCallback(const QueueNotFull& callback, int64_t nodeTypeId)
    {
       m_impl->SetQueueNotFullCallback(callback, nodeTypeId);
    }

    void Communication::SetDataReceiver(const ReceiveData& callback, int64_t dataTypeIdentifier, const Allocator& allocator)
    {
        m_impl->SetDataReceiver(callback, dataTypeIdentifier, allocator);
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

    void Communication::IncludeNode(int64_t nodeId)
    {
        m_impl->IncludeNode(nodeId);
    }

    void Communication::ExcludeNode(int64_t nodeId)
    {
        m_impl->ExcludeNode(nodeId);
    }

    void Communication::InjectNode(const std::string& name, int64_t id, int64_t nodeTypeId, const std::string& dataAddress)
    {
        m_impl->InjectNode(name, id, nodeTypeId, dataAddress);
    }

    bool Communication::Send(int64_t nodeId,
                             int64_t nodeTypeId,
                             const boost::shared_ptr<const char[]>& data,
                             size_t size,
                             int64_t dataTypeIdentifier,
                             bool deliveryGuarantee)
    {
        return m_impl->Send(nodeId, nodeTypeId, data, size, dataTypeIdentifier, deliveryGuarantee);
    }

    size_t Communication::SendQueueCapacity(int64_t nodeTypeId) const
    {
        return m_impl->SendQueueCapacity(nodeTypeId);
    }

    size_t Communication::NumberOfQueuedMessages(int64_t nodeTypeId) const
    {
        return m_impl->NumberOfQueuedMessages(nodeTypeId);
    }

    const std::string& Communication::Name() const
    {
        return m_impl->Name();
    }

    int64_t Communication::Id() const
    {
        return m_impl->Id();
    }

    std::string Communication::ControlAddress() const
    {
        return m_impl->ControlAddress();
    }

    std::string Communication::DataAddress() const
    {
        return m_impl->DataAddress();
    }
}
}
}
}
