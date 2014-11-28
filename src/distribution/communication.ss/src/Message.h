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
#ifndef __SAFIR_DOB_COMMUNICATION_MESSAGE_H__
#define __SAFIR_DOB_COMMUNICATION_MESSAGE_H__

#include <set>
#include <bitset>
#include <boost/cstdint.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/chrono.hpp>
#include "Parameters.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4244)
#pragma warning (disable: 4245)
#pragma warning (disable: 4127)
#endif

#include <boost/crc.hpp>
#include "CommunicationMessage.pb.h"

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
    //Constants
    static const int64_t HeartbeatType=-1113057794592031140; //Hash for 'Communication.Heartbeat'
    static const int64_t AckType=-6769271806353797703; //Hash for 'Communication.Ack'
    static const int64_t AckRequestType=-3908639933957133038; //Hash for 'Communication.AckRequest'
    static const int64_t ControlDataType=186858702748131856; //Hash for 'Communication.ControlData'


    //Send method
    static const uint8_t SingleReceiverSendMethod=0;
    static const uint8_t MultiReceiverSendMethod=1;

    //Delivery guarantee
    static const uint8_t Unacked=0;
    static const uint8_t Acked=1;

    //------------------------------------------------------------
    //toString functions for convenience
    //------------------------------------------------------------
    inline std::string SendMethodToString(uint8_t sm) {return sm==SingleReceiverSendMethod ? "SingleReceiver" : "MultiReceiver";}
    inline std::string DeliveryGuaranteeToString(uint8_t dg) {return dg==Acked ? "Acked" : "Unacked";}
    inline void hexdump(const char* data, size_t first, size_t last)
    {
        for (size_t i=first; i<last; ++i)
        {
            if (i%50==0)
                std::cout<<std::endl;

            std::cout<<std::hex<<(unsigned int)data[i];
        }
        std::cout<<std::dec<<std::endl;
    }
    //------------------------------------------------------------


    #pragma pack(push)
    #pragma pack(1)
    struct CommonHeader
    {
        int64_t senderId;
        int64_t receiverId; //0 if message is to everyone
        int64_t dataType;
        CommonHeader(int64_t senderId_,
                     int64_t receiverId_,
                     int64_t dataType_)
            :senderId(senderId_)
            ,receiverId(receiverId_)
            ,dataType(dataType_) {}
    };
    static const size_t CommonHeaderSize=sizeof(CommonHeader);

    struct Heartbeat
    {
        CommonHeader commonHeader;
        Heartbeat(int64_t senderId_) : commonHeader(senderId_, 0, HeartbeatType) {}
    };

    struct Ack
    {
        CommonHeader commonHeader;
        uint64_t sequenceNumber;
        uint8_t sendMethod; //tells if message being acked was sent to one or many receivers (different sequence numbers)
        unsigned char missing[Parameters::SlidingWindowSize]; //1 means missing, 0 not missing sequenceNumber is index 0, seqNo-1 is index 1 and so on.
        Ack(int64_t senderId_, int64_t receiverId_, uint64_t sequenceNumber_, uint8_t sendMethod_)
            :commonHeader(senderId_, receiverId_, AckType)
            ,sequenceNumber(sequenceNumber_)
            ,sendMethod(sendMethod_)
        {
        }
    };

    inline std::string AckToString(const Ack& ack)
    {
        std::ostringstream os;
        os<<"Ack from: "<<ack.commonHeader.senderId<<" "<<SendMethodToString(ack.sendMethod)<<
            " dataType: "<<ack.commonHeader.dataType<<" seq: "<<ack.sequenceNumber<<" gaps: ";
        for (auto i : ack.missing) os<<static_cast<int>(i);
        return os.str();
    }

    struct MessageHeader
    {
        CommonHeader commonHeader;
        uint8_t sendMethod;
        uint8_t deliveryGuarantee;
        uint64_t sequenceNumber;
        size_t totalContentSize;
        size_t fragmentContentSize;
        uint16_t numberOfFragments;
        uint16_t fragmentNumber;
        size_t fragmentOffset;
        uint8_t ackNow;

        MessageHeader(int64_t senderId_,
                      int64_t receiverId_,
                      int64_t dataType_ ,
                      uint8_t sendMethod_,
                      uint8_t deliveryGuarantee_,
                      uint64_t sequenceNumber_,
                      size_t totalContentSize_,
                      size_t fragmentContentSize_,
                      uint16_t numberOfFragments_,
                      uint16_t fragmentNumber_,
                      size_t fragmentOffset_)
            :commonHeader(senderId_, receiverId_, dataType_)
            ,sendMethod(sendMethod_)
            ,deliveryGuarantee(deliveryGuarantee_)
            ,sequenceNumber(sequenceNumber_)
            ,totalContentSize(totalContentSize_)
            ,fragmentContentSize(fragmentContentSize_)
            ,numberOfFragments(numberOfFragments_)
            ,fragmentNumber(fragmentNumber_)
            ,fragmentOffset(fragmentOffset_)
            ,ackNow(0)
        {
        }
    };
    #pragma pack(pop)

    static const size_t MessageHeaderSize=sizeof(MessageHeader);

    //This is for keeping track of ack's and not sent messages.
    typedef std::set<int64_t> Receivers;

    struct UserData
    {
        MessageHeader header; //message header
        boost::shared_ptr<char[]> message; //This is to prevent  destruction of data before all fragments are sent
        const char* fragment; //This is what is sent in this UserData. If not fragmented these will be the same as payload and payloadSize
        Receivers receivers; //Set of receivers, can be filled with a receiver list, or if MultiReceiverSendMethod it will be filled when its sent
        boost::chrono::steady_clock::time_point sendTime; //timestamp when this messages was last transmitted so we know when it's time to make retransmit

        UserData(const int64_t senderId,
                 const int64_t receiverId,
                 const int64_t dataType,
                 const boost::shared_ptr<char[]>& message_,
                 size_t messageSize)
            :header(senderId, receiverId, dataType, MultiReceiverSendMethod, Acked, 0, messageSize, messageSize, 1, 0, 0)
            ,message(message_)
            ,fragment(message.get())
        {
        }

        UserData(const int64_t senderId,
                 const int64_t receiverId,
                 const int64_t dataType,
                 const boost::shared_ptr<char[]>& message_,
                 size_t messageSize,
                 const char* fragment_,
                 size_t fragmentSize)
            :header(senderId, receiverId, dataType, MultiReceiverSendMethod, Acked, 0, messageSize, fragmentSize, 1, 0, static_cast<size_t>(fragment_-message_.get()))
            ,message(message_)
            ,fragment(fragment_)
        {
        }
    };

    typedef boost::shared_ptr<UserData> UserDataPtr;
}
}
}
} //Safir::Dob::Internal::Com

#endif
