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
#ifndef __STATE_SUBSCRIBER_LOCAL_H__
#define __STATE_SUBSCRIBER_LOCAL_H__

#include "MessageWrapperCreators.h"
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <functional>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4127)
#endif

#include "SystemStateMessage.pb.h"

#ifdef _MSC_VER
#pragma warning (pop)
#endif


namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{

    class StateSubscriberLocal
        : public SystemStateSubscriber
    {
    public:
        explicit StateSubscriberLocal(const char* const name)
            : m_name (name)
        {

        }
        //callback will be delivered on one strand.
        //but Start and Stop are not MT safe, please only call one at a time.
        void Start(boost::asio::io_service& ioService,
                   const std::function<void (const SystemState& data)>& dataCallback) override
        {
            if (m_subscriber != nullptr)
            {
                throw std::logic_error("StateSubscriberLocal already started");
            }

            m_dataCallback = dataCallback;
            
            m_subscriber = Safir::Utilities::Internal::IpcSubscriber::Create
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
                                << "Bad CRC in StateSubscriberLocal, expected " << expected << " got " << crc);
                throw std::logic_error("CRC check failed!");
            }
#endif
            auto state = Safir::make_unique<SystemStateMessage>();
        
            const bool parseResult = state->ParseFromArray(data, static_cast<int>(size));

            if (!parseResult)
            {
                throw std::logic_error("StateSubscriberLocal: Failed to parse message");
            }
            m_dataCallback(SystemStateCreator::Create(std::move(state)));
        }

        //Order/sync is guaranteed by IpcSubscribers delivery order guarantee.

        const std::string m_name;

        std::function<void (const SystemState& data)> m_dataCallback;
        boost::shared_ptr<Safir::Utilities::Internal::IpcSubscriber> m_subscriber;
    };
}
}
}
}

#endif

