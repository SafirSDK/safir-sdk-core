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
#ifndef __RAW_SUBSCRIBER_LOCAL_H__
#define __RAW_SUBSCRIBER_LOCAL_H__

#include "MessageWrapperCreators.h"
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/make_shared.hpp>
#include <Safir/Utilities/Internal/IpcSubscriber.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "CrcUtils.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{
    
    template <class Subscriber>
    class RawSubscriberLocalBasic
        : public RawStatisticsSubscriber
    {
    public:
        explicit RawSubscriberLocalBasic(const char* const name)
            : m_name(name)
        {

        }


        //callback will be delivered on one strand.
        //but Start and Stop are not MT safe, please only call one at a time.
        void Start(boost::asio::io_service& ioService,
                   const std::function<void (const RawStatistics& data)>& dataCallback) override
        {
            if (m_subscriber != nullptr)
            {
                throw std::logic_error("RawSubscriberLocal already started");
            }

            m_dataCallback = dataCallback;
            
            m_subscriber = Safir::make_unique<Subscriber>
                (ioService,
                 m_name,
                 [this](const char* const data, size_t size)
                 {
                     DataReceived(data,size);
                 });
            m_subscriber->Connect();
        }

        void Stop() override
        {
            if (m_subscriber != nullptr)
            {
                m_subscriber->Disconnect();
                m_subscriber.reset();
            }
        }
 
    private:
        void DataReceived(const char* const data, size_t size)
        {
#ifdef CHECK_CRC
            size -= sizeof(int); //remove the crc from size
            int expected;
            memcpy(&expected, data + size, sizeof(int));
            const int crc = GetCrc32(data, size);
            if (crc != expected)
            {
                SEND_SYSTEM_LOG(Alert,
                                << "Bad CRC in RawSubscriber, expected " << expected << " got " << crc);
                throw std::logic_error("CRC check failed!");
            }
#endif

            auto msg = Safir::make_unique<NodeStatisticsMessage>();
        
            const bool parseResult = msg->ParseFromArray(data, static_cast<int>(size));

            if (!parseResult)
            {
                throw std::logic_error("RawSubscriberLocal: Failed to parse message");
            }
            m_dataCallback(RawStatisticsCreator::Create(std::move(msg)));
        }

        //Order/sync is guaranteed by IpcSubscribers delivery order guarantee.

        const std::string m_name;

        std::function<void (const RawStatistics& data)> m_dataCallback;
        std::unique_ptr<Subscriber> m_subscriber;
    };

    typedef RawSubscriberLocalBasic<Safir::Utilities::Internal::IpcSubscriber> RawSubscriberLocal;
}
}
}
}

#endif

