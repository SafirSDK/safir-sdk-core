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

#include <vector>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/function.hpp>
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
    class DOB_COMMUNICATION_API Communication : public boost::noncopyable
    {
    public:

        //Callbacks functions used in Communications public interface.
        typedef boost::function<void(const std::string& name, boost::int64_t id, const std::string& addr, bool isMulticast)> NewNode;
        typedef boost::function<void(boost::int64_t fromId)> GotReceiveFrom;
        typedef boost::function<void(boost::int64_t toId)> RetransmitTo;
        typedef boost::function<void(boost::int64_t from, const boost::shared_ptr<char[]>& data, size_t size)> ReceiveData;
        typedef boost::function<void()> QueueNotFull;

        /**
         * Communication - constructor.
         *
         * @param ioService [in] - Pointer to an io_service that will be used as engine.
         * @param name [in] - Name of this node.
         * @param id [in] - Unique id of this node. Note that 0 (zero) is not a valid id.
         * @param unicastAddress [in] - Unicast address on format address:port, mandatory.
         * @param multicastAddress [in] - Multicast address on format address:port. Empty string if not applicable.
         */
        Communication(const boost::shared_ptr<boost::asio::io_service>& ioService,
                      const std::string& name,
                      const boost::int64_t id, //0 is not a valid id.
                      const std::string& unicastAddress,   //mandatory
                      const std::string& multicastAddress); //optional, can be empty string

        /**
         * ~Communication - destructor.
         */
        virtual ~Communication();

        /**
         * Set callback for notification of new discovered nodes. Must be called before calling Start.
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
         * Set callback for notification that the send queue is nolonger full. After a SendAll or SendTo has returned false due to
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
        void SetDataReceiver(const ReceiveData& callback, boost::int64_t dataTypeIdentifier);

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
         * Add seed addresses used for discovering other nodes. Can be called before or after Start is called.
         * Can also be called more than one time with new seed addresses.
         *
         * @param seeds [in] - Vector of seed addresses on the form address:port. Only unicast addresses are valid.
         */
        void InjectSeeds(const std::vector<std::string>& seeds);

        /**
         * Make a node that have been reported on the NewNode callback a system node.
         *
         * @param id [in] - id of the node.
         */
        void IncludeNode(boost::int64_t id);

        /**
         * Exclude a system node. After a node has been excluded it can never be included  again.
         *
         * @param id [in] - id of the node.
         */
        void ExcludeNode(boost::int64_t id);

        /**
         * Send data to all system nodes.
         *
         * @param data [in] - Pointer to the data that shall be sent.
         * @param size [in] - Size of data.
         * @param dataTypeIdentifier [in] - Custom identifier specifying which type of data. Only data receivers added with the same identifier will get the data.
         * @return True if data could be added to send queue. False if send queue is full, in that case try again later.
         */
        bool SendAll(const boost::shared_ptr<char[]>& data, size_t size, boost::int64_t dataTypeIdentifier);

        /**
         * Send data to a specific node. If the specified node is not a system node, the message is silently ignored and the return value will be 'true'.
         *
         * @param toId [in] - Id of the node to send to.
         * @param data [in] - Pointer to the data that shall be sent.
         * @param size [in] - Size of data.
         * @param dataTypeIdentifier [in] - Custom identifier specifying which type of data. Only data receivers added with the same identifier will get the data.
         * @return True if data could be added to send queue. False if send queue is full, in that case try again later.
         */
        bool SendTo(boost::int64_t toId, const boost::shared_ptr<char[]>& data, size_t size, boost::int64_t dataTypeIdentifier);

        /**
         * Get the number of messages in the send queue.
         *
         * @return Number of messages.
         */
        size_t NumberOfQueuedMessages() const;

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
        boost::int64_t Id() const;

    private:
        boost::shared_ptr<CommunicationImpl> m_impl;
    };
#ifdef _MSC_VER
#pragma warning (pop)
#endif
}
}
}
}

#endif
