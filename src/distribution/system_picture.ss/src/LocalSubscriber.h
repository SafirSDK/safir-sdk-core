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
#include <boost/atomic.hpp>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

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
        typedef boost::function<void (const typename SubscriberInterfaceT::DataWrapper& data)> DataCallback;

        LocalSubscriber(boost::asio::io_service& ioService,
                        const char* const name)
            : m_strand(ioService)
            , m_name (name)
        {

            m_subscriber.reset(new IpcSubscriberT(ioService,
                                                  m_name,
                                                  [this](const char* const data, size_t size){DataReceived(data,size);}));
        }


        void Start(const DataCallback& dataCallback) override
        {
            AddSubscriber(dataCallback);
        }

        void AddSubscriber(const DataCallback& dataCallback)
        {
            m_strand.dispatch([this, dataCallback]
            {
                lllog(9) << "SP: AddSubscriber" << std::endl;
                const bool needConnect = m_dataCallbacks.empty();

                m_dataCallbacks.push_back(dataCallback);

                if (needConnect)
                {
                    lllog(9) << "SP: AddSubscriber calling Connect" << std::endl;
                    m_subscriber->Connect();
                }
            });
        }

        void Stop() override
        {
            m_strand.dispatch([this]
                              {
                                  m_subscriber->Disconnect();
                                  m_dataCallbacks.clear();
                              });
        }

    private:
        //called in strand
        void DataReceived(const char* const data, const size_t size)
        {
            lllog(9) << "SP: LocalSubscriber " << m_name.c_str() << " received new data" << std::endl;

            auto msg = Safir::make_unique<typename WrapperCreatorT::WrappedType>();

            const bool parseResult = msg->ParseFromArray(data, static_cast<int>(size));

            if (!parseResult)
            {
                throw std::logic_error("LocalSubscriber: Failed to parse message");
            }

            const auto wrapped = WrapperCreatorT::Create(std::move(msg));
            m_strand.dispatch([this,wrapped]
            {
                for (auto cb = m_dataCallbacks.cbegin(); cb != m_dataCallbacks.cend(); ++cb)
                {
                    (*cb)(wrapped);
                }
            });
        }

        //Order/sync is guaranteed by IpcSubscribers delivery order guarantee.

        boost::asio::io_service::strand m_strand;
        const std::string m_name;

        std::vector<DataCallback> m_dataCallbacks;

        std::unique_ptr<IpcSubscriberT> m_subscriber;
    };


}
}
}
}
