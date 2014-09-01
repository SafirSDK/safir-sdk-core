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
#pragma once

#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "SubscriberInterfaces.h"
#include <functional>
#include <boost/asio.hpp>
#include "CrcUtils.h"
#include <atomic>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{

    template <class IpcSubscriberT, class SubscriberInterfaceT, class WrapperCreatorT>
    class LocalSubscriber
        : public SubscriberInterfaceT
    {
    public:
        LocalSubscriber(boost::asio::io_service& ioService,
                        const char* const name)
            : m_strand(ioService)
            , m_name (name)
            , m_subscriber(ioService,
                           m_name,
                           m_strand.wrap([this](const char* const data, size_t size)
                                         {
                                             DataReceived(data,size);
                                         }))

        {

        }

        //callback will be delivered on one strand.
        void Start(const std::function<void (const typename SubscriberInterfaceT::DataWrapper& data)>& dataCallback) override
        {
            m_strand.dispatch([this, dataCallback]
                              {
                                  m_dataCallback = dataCallback;

                                  m_subscriber.Connect();
                              });
        }

        void Stop() override
        {
            m_strand.dispatch([this]
                              {
                                  m_subscriber.Disconnect();
                              });
        }

    private:
        //called in strand
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
                                << "Bad CRC in LocalSubscriber, expected " << expected << " got " << crc);
                throw std::logic_error("CRC check failed!");
            }
#endif

            auto msg = Safir::make_unique<typename WrapperCreatorT::WrappedType>();

            const bool parseResult = msg->ParseFromArray(data, static_cast<int>(size));

            if (!parseResult)
            {
                throw std::logic_error("LocalSubscriber: Failed to parse message");
            }
            m_dataCallback(WrapperCreatorT::Create(std::move(msg)));
        }

        //Order/sync is guaranteed by IpcSubscribers delivery order guarantee.

        boost::asio::io_service::strand m_strand;
        const std::string m_name;

        std::function<void (const typename SubscriberInterfaceT::DataWrapper& data)> m_dataCallback;
        IpcSubscriberT m_subscriber;
    };


}
}
}
}
