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
#ifndef __SAFIR_DOB_COMMUNICATION_IMPL_H__
#define __SAFIR_DOB_COMMUNICATION_IMPL_H__

#include <set>
#include <boost/noncopyable.hpp>
#include <boost/asio.hpp>
#include <boost/function.hpp>
#include "Node.h"
#include "Reader.h"
#include "DeliveryHandler.h"
#include "Writer.h"
#include "Discoverer.h"
#include "AckedDataSender.h"
#include "HeartBeatSender.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    typedef boost::function<void(const std::string& name, boost::int64_t id, const std::string& addr, bool isMulticast)> NewNode;

    class CommunicationImpl : public boost::noncopyable
    {
    public:
        CommunicationImpl(const boost::shared_ptr<boost::asio::io_service>& ioService,
                          const std::string& name,
                          const boost::int64_t id, //0 is not a valid id.
                          const std::string& unicastAddress,   //mandatory
                          const std::string& multicastAddress);

        virtual ~CommunicationImpl();

        //set callbacks
        void SetNewNodeCallback(const NewNode& callback);
        void SetGotReceiveFromCallback(const GotReceiveFrom& callback);
        void SetRetransmitToCallback(const RetransmitTo& callback);
        void SetQueueNotFullCallback(const QueueNotFull& callback, int freePartThreshold);
        void SetDataReceiver(const ReceiveData& callback, boost::int64_t dataTypeIdentifier);

        void InjectSeeds(const std::vector<std::string>& seeds);

        void Start();
        void Stop();

        void IncludeNode(boost::int64_t id);
        void ExcludeNode(boost::int64_t id);
        bool SendAll(const boost::shared_ptr<char[]>& msg, size_t size, boost::int64_t dataTypeIdentifier);
        bool SendTo(boost::int64_t toId, const boost::shared_ptr<char[]>& msg, size_t size, boost::int64_t dataTypeIdentifier);

        size_t NumberOfQueuedMessages() const {return m_ackedDataSender.SendQueueSize();}

        const std::string& Name() const {return m_me.Name();}
        boost::int64_t Id() const {return m_me.Id();}

    private:
        ::google::protobuf::LogSilencer m_disableProtobufLogs;
        boost::shared_ptr<boost::asio::io_service> m_ioService;
        Node m_me;

        //Callbacks
        NewNode m_onNewNode;
        GotReceiveFrom m_gotRecv;

        //main components of communication
        Reader m_reader;
        Writer<UserData> m_discoverWriter;
        Discoverer m_discoverer;
        AckedDataSender m_ackedDataSender;
        HeartBeatSender m_heartBeatSender;
        DeliveryHandler m_deliveryHandler;

        void SetSystemNode(boost::int64_t id, bool isSystemNode);
        bool OnRecv(const char* data, size_t size); //returns true if it is ok to call OnRecv again, false if flooded with received messages
        void OnNewNode(const Node& node);

        //Received internal Communication msg that is not directly passed to application, i.e discover, nodeInfo etc.
        void ReceivedControlData(const MessageHeader* header, const char* payload);
    };
}
}
}
}

#endif
