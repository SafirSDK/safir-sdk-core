/******************************************************************************
*
* Copyright Saab AB, 2013-2022 (http://safirsdkcore.com)
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
// Initiate the extern global parameters in Parameters.h
namespace Parameters
{
    std::atomic<bool> NetworkEnabled;
    std::string LogPrefix;
}

namespace
{
    std::unique_ptr<CommunicationImpl> Init(bool isControlInstance,
                                            boost::asio::io_context& ioContext,
                                            const std::string& nodeName,
                                            int64_t nodeId, //0 is not a valid id.
                                            int64_t nodeTypeId,
                                            const std::string& controlAddress,
                                            const std::string& dataAddress,
                                            const std::vector<NodeTypeDefinition>& nodeTypes,
                                            int fragmentSize)
    {
        Parameters::NetworkEnabled = true;
        Parameters::LogPrefix = isControlInstance ? "COMc["+std::to_string(nodeId)+"]: " : "COMd["+std::to_string(nodeId)+"]: ";

        //find address of local interface to use
        Resolver resolver(ioContext);

        const auto localIf=(isControlInstance ? controlAddress : dataAddress);

        //find out if we are using ip4 or ip6
        const int ipVersion=Resolver::Protocol(localIf);

        //find own node type and check if we are multicast enabled
        auto nodeTypeIt=std::find_if(nodeTypes.cbegin(), nodeTypes.cend(),
                                     [nodeTypeId](const NodeTypeDefinition& n){return n.id==nodeTypeId;});
        if (nodeTypeIt==nodeTypes.end())
        {
            throw std::logic_error("Own nodeType does not exist "+std::string(__FILE__));
        }
        bool thisNodeIsMulticastEnabled=!(isControlInstance ? nodeTypeIt->controlMulticastAddress.empty() : nodeTypeIt->dataMulticastAddress.empty());

        //create node type map
        NodeTypeMap nodeTypeMap;
        for (auto nt = nodeTypes.cbegin(); nt != nodeTypes.cend(); ++nt)
        {
            if (nt->id==0)
            {
                throw std::invalid_argument("Safir.Communication: NodeType '"+nt->name+"' has id=0. NodeTypeId 0 is reserved and can't be assigned to an specific nodeType.");
            }

            const std::string& mc=isControlInstance ? nt->controlMulticastAddress : nt->dataMulticastAddress;
            bool useMulticast=(thisNodeIsMulticastEnabled && !mc.empty());
            auto ptr= std::shared_ptr<Safir::Dob::Internal::Com::NodeType>
                (new Safir::Dob::Internal::Com::NodeType(ioContext,
                                                         nodeId,
                                                         localIf,
                                                         useMulticast,
                                                         nt->id,
                                                         nt->name,
                                                         mc,
                                                         ipVersion,
                                                         nt->heartbeatInterval,
                                                         nt->maxLostHeartbeats,
                                                         nt->slidingWindowSize,
                                                         nt->ackRequestThreshold,
                                                         fragmentSize,
                                                         nt->isLightNode,
                                                         nt->retryTimeout));
            nodeTypeMap.insert(NodeTypeMap::value_type(nt->id, ptr));
        }

        //create impl object
        return std::unique_ptr<CommunicationImpl>(new CommunicationImpl(ioContext, nodeName, nodeId, nodeTypeId, controlAddress, dataAddress, isControlInstance, nodeTypeMap, fragmentSize));
    }
}

    ResolvedAddress::ResolvedAddress(const std::string& pattern)
        : m_address(Resolver::ResolveLocalEndpoint(pattern))
    {
    }

    const std::string& ResolvedAddress::Address() const
    {
        if(!Ok())
        {
            throw std::logic_error("Address resolution has failed! Not ok to call Address!");
        }
        return m_address;
    }

    Communication::Communication(ControlModeTag,
                                 boost::asio::io_context& ioContext,
                                 const std::string& nodeName,
                                 int64_t nodeId, //0 is not a valid id.
                                 int64_t nodeTypeId,
                                 const ResolvedAddress& controlAddress,
                                 const ResolvedAddress& dataAddress,
                                 const std::vector<NodeTypeDefinition>& nodeTypes,
                                 int fragmentSize)
        :m_impl(Init(true, ioContext, nodeName, nodeId, nodeTypeId, controlAddress.Address(), dataAddress.Address(), nodeTypes, fragmentSize))
    {
    }


    Communication::Communication(DataModeTag,
                                 boost::asio::io_context& ioContext,
                                 const std::string& nodeName,
                                 int64_t nodeId, //0 is not a valid id.
                                 int64_t nodeTypeId,
                                 const ResolvedAddress& dataAddress,
                                 const std::vector<NodeTypeDefinition>& nodeTypes,
                                 int fragmentSize)
        :m_impl(Init(false, ioContext, nodeName, nodeId, nodeTypeId, "", dataAddress.Address(), nodeTypes, fragmentSize))
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

    void Communication::SetDataReceiver(const ReceiveData& callback, int64_t dataTypeIdentifier, const Allocator& allocator, const DeAllocator& deallocator)
    {
        m_impl->SetDataReceiver(callback, dataTypeIdentifier, allocator, deallocator);
    }

    void Communication::Start()
    {
        m_impl->Start();
    }

    void Communication::Stop()
    {
        m_impl->Stop();
    }

    void Communication::Reset()
    {
        m_impl->Reset();
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

    void Communication::SetExcludeNodeTimeLimit(unsigned int seconds)
    {
        m_impl->SetExcludeNodeTimeLimit(seconds);
    }

    void Communication::InjectNode(const std::string& name, int64_t id, int64_t nodeTypeId, const std::string& dataAddress)
    {
        m_impl->InjectNode(name, id, nodeTypeId, dataAddress);
    }

    bool Communication::Send(int64_t nodeId,
                             int64_t nodeTypeId,
                             const Safir::Utilities::Internal::SharedConstCharArray& data,
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
