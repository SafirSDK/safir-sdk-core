/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
#ifndef __SAFIR_DOB_COMMUNICATION_H__
#define __SAFIR_DOB_COMMUNICATION_H__

#include <cstdint>
#include <vector>
#include <memory>
#include <functional>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <Safir/Dob/Internal/CommunicationExportDefs.h>

namespace boost{namespace asio{class io_service;}} //forward declaration

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    /**
     * Definition of a NodeType.
     */
    struct NodeTypeDefinition
    {
        int64_t id;                             //node type id
        std::string name;                       //unique readable name
        std::string controlMulticastAddress;    //multicast address including port number, 'address:port' empty string if not multicast enabled
        std::string dataMulticastAddress;       //multicast address including port number, 'address:port' empty string if not multicast enabled
        int heartbeatInterval;                  //time between heartbeats (milliseconds)
        int retryTimeout;                       //time to wait before retransmitting data (milliseconds)
    };

    //Callbacks functions used in Communications public interface.
    typedef std::function<void(const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& controlAddress, const std::string& dataAddress)> NewNode;
    typedef std::function<void(int64_t fromNodeId)> GotReceiveFrom;
    typedef std::function<void(int64_t toNodeId)> RetransmitTo;
    typedef std::function<void(int64_t fromNodeId, int64_t fromNodeType, const boost::shared_ptr<char[]>& data, size_t size)> ReceiveData;
    typedef std::function<void(int64_t nodeTypeId)> QueueNotFull;

    struct ControlModeTag {};
    const ControlModeTag controlModeTag = ControlModeTag();

    struct DataModeTag {};
    const DataModeTag dataModeTag = DataModeTag();

    class CommunicationImpl; //forward declaration

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4275)
#pragma warning (disable: 4251)
#endif
    /**
     * @brief The Communication class handles low level socket communication between nodes including discovering nodes,
     * message fragmentation and retranmits of lost packets and ordering of incoming data.
     *
     * After construction all callbacks must be set before calling the Start method. Seeds can be injected at any time,
     * even before Start method is called.
     *
     * Nowhere in this class is 0 (zero) allowed as node Id.
     */
    class DISTRIBUTION_COMMUNICATION_API Communication : private boost::noncopyable
    {
    public:

        /**
         * @brief Communication - Creates an instance of Communication in control mode. It will run the discover mechanism after calling start.
         * @param controlModeTag [in] - Tag that specifies that this instance will be used in control mode.
         * @param ioService [in] - Pointer to an io_service that will be used as engine.
         * @param nodeName [in] - Name of this node.
         * @param nodeId [in] - Unique id of this node. Note that 0 (zero) is not a valid id.
         * @param nodeTypeId [in] - The node type of this node.
         * @param controlAddress [in] - Control channel unicast address on format address:port, mandatory.
         * @param dataAddress [in] -  Data channel unicast address on format address:port, mandatory.
         * @param nodeTypes [in] - List of all node types that we shall be able to communicate with.
         */
        Communication(ControlModeTag,
                      boost::asio::io_service& ioService,
                      const std::string& nodeName,
                      int64_t nodeId, //0 is not a valid id.
                      int64_t nodeTypeId,
                      const std::string& controlAddress,
                      const std::string& dataAddress,
                      const std::vector<NodeTypeDefinition>& nodeTypes);

        /**
         * @brief Communication - Creates an instance of Communication in data mode. Will not run discover and nodes must be added manually.
         * @param dataModeTag [in] - Tag that specifies that this instance will be used in data mode.
         * @param ioService [in] - Pointer to an io_service that will be used as engine.
         * @param nodeName [in] - Name of this node.
         * @param nodeId [in] - Unique id of this node. Note that 0 (zero) is not a valid id.
         * @param nodeTypeId [in] - The node type of this node.
         * @param dataAddress [in] -  Data channel unicast address on format address:port, mandatory.
         * @param nodeTypes [in] - List of all node types that we shall be able to communicate with.
         */
        Communication(DataModeTag,
                      boost::asio::io_service& ioService,
                      const std::string& nodeName,
                      int64_t nodeId, //0 is not a valid id.
                      int64_t nodeTypeId,
                      const std::string& dataAddress,
                      const std::vector<NodeTypeDefinition>& nodeTypes);

        /**
         * ~Communication - destructor.
         */
        virtual ~Communication();

        /**
         * Set callback for notification of new discovered nodes. Must be called before calling Start.
         * If created with DataModeTag this callback will be triggered immediately from within InjectNode.
         * I.e when calling InjectNode, this callback will be called before InjectNode returns.
         *
         * @param callback [in] - Callback function.
         */
        void SetNewNodeCallback(const NewNode& callback);

        /**
         * Set callback for receive notifications. Must be called before calling Start.
         *
         * @param callback [in] - Callback function.
         */
        void SetGotReceiveFromCallback(const GotReceiveFrom& callback);

        /**
         * Set callback for retransmit notifications. Must be called before calling Start.
         * Note: It is not allowed to call a send method from inside the retransmit callback function.
         *
         * @param callback [in] - Callback function.
         */
        void SetRetransmitToCallback(const RetransmitTo& callback);

        /**
         * Set callback for notification that the send queue is nolonger full. After a SendToNode or SendToNodeType has returned false due to
         * full send queue, this callback will notfy that the send queue is no longer full. The notification will be executed when
         * at least the specified percent of the send queue is free.
         * Note that callback will only be received if user called a Send method that returned false. If the send queue has been full but
         * no attempt was made to send anything while it was full, no notification will be made.
         *
         * @param callback [in] - Callback function.
         * @param percentFreeLimit [in] - Threshold value given in percent of the total capacity of the send queue.
         */
        void SetQueueNotFullCallback(const QueueNotFull& callback, int freePartThreshold);

        /**
         * Set callback for receive data for a specific data type. Can be called multiple times with different dataTypeIdentifier.
         * Must be called before calling Start.
         *
         * @param callback [in] - Callback function.
         * @param dataTypeIdentifier [in] - Only data sent with the same dataTypeIdentifier will be received by the callback.
         */
        void SetDataReceiver(const ReceiveData& callback, int64_t dataTypeIdentifier);

        /**
         * Start communication, no callbacks can be setup after started.
         */
        void Start();

        /**
         * Stop communication. Communication will finish ongoing jobs and close all connections.
         */
        void Stop();

        //Only unicast addresses.
        /**
         * Add seed addresses used for discovering other nodes.
         * Only useful if created with ControlModeTag
         *
         * Can be called before or after Start is called.
         * Can also be called more than one time with new seed addresses.
         *
         * @param seeds [in] - Vector of seed addresses on the form address:port. Only unicast addresses are valid.
         */
        void InjectSeeds(const std::vector<std::string>& seeds);

        /**
         * Make a node that have been reported on the NewNode callback a system node.
         * Only useful if created with ControlModeTag
         *
         * @param nodeId [in] - Id of the node to include.
         */
        void IncludeNode(int64_t nodeId);

        /**
         * Exclude a system node. After a node has been excluded it can never be included  again.
         * Only useful if created with ControlModeTag
         *
         * @param nodeId [in] - Id of the node to exclude.
         */
        void ExcludeNode(int64_t nodeId);

        /**
         * Tell communication that a node exist and is part of the system. Calls to this method will be ignored if the instance was created with
         * ControlModeTag and hence is running the discover mechanism.
         * Only useful if created with DataModeTag
         * @param name [in] - Name of the node.
         * @param id [in] - Id of the node.
         * @param nodeTypeId [in] - Node type of the node.
         * @param dataAddress [in] - Ip address for the unicast data channel to the node. On the form ip:port
         */
        void InjectNode(const std::string& name, int64_t id, int64_t nodeTypeId, const std::string& dataAddress);

        /**
         * Send data to system nodes. Can either send to a specific node or to all nodes within a node type.
         *
         * @param nodeId [in] - Id of specific receiver node. Shall be 0 when sending to all nodes in a node type.
         * @param nodeTypeId [in] - Receiver node type, or if nodeId is 0 the node type of the specified node.
         * @param data [in] - pointer to the data that shall be sent.
         * @param size [in] - Size of data.
         * @param dataTypeIdentifier [in] - Custom identifier specifying which type of data. Only data receivers added with the same identifier will get the data.
         * @param deliveryGuarantee [in] - If true communication will assure delivery by requesting all receivers to acknowledge the reception of the data. Will also perform retransmits if necessary.
         * @return When sending with delivery guarantee, true is returned if data could be added to send queue. False if send queue is full, in that case try again later. If sending without delivery
         *          guarantee true is always returned.
         */
        bool Send(int64_t nodeId,
                  int64_t nodeTypeId,
                  const boost::shared_ptr<char[]>& data,
                  size_t size,
                  int64_t dataTypeIdentifier,
                  bool deliveryGuarantee);

        /**
         * Get the number of messages in a node types acked send queue.
         * @param nodeTypeId [in] - Node type.
         * @return Number of messages in the acked send queue.
         */
        size_t NumberOfQueuedMessages(int64_t nodeTypeId) const;

        /**
         * Get the name that was passed as argument to the constructor.
         *
         * @return Name of node.
         */
        const std::string& Name() const;

        /**
         * Get the id that was passed as argument to the constructor.
         *
         * @return Id of the node.
         */
        int64_t Id() const;

    private:
        std::unique_ptr<CommunicationImpl> m_impl;

    };
#ifdef _MSC_VER
#pragma warning (pop)
#endif
}
}
}
}

#endif
