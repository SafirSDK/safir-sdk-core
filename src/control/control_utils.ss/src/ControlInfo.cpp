/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com/)
*
* Created by: Anders Widén / anders.widen@consoden.se
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
#include <Safir/Dob/Internal/ControlInfo.h>
#include <Safir/Utilities/Internal/IpcPublisher.h>
#include <Safir/Utilities/Internal/IpcSubscriber.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include "ControlInfo.pb.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{

    const std::string controlInfoChannel("CONTROL_INFO");

    // Receiver impl
    class ControlInfoReceiver::Impl
        : private boost::noncopyable
    {
    public:

        Impl(boost::asio::io_service&    ioService,
             const InfoCb&               infoCb)
            : m_infoCb(infoCb)
        {
            m_ipcSubscriber.reset( new Safir::Utilities::Internal::IpcSubscriber(ioService,
                                                                                 controlInfoChannel,
                                                                                 [this](const char* data, size_t size)
                                                                                 {
                                                                                     RecvDataCb(data, size);
                                                                                 }));
        }

        void Start()
        {
            m_ipcSubscriber->Connect();
        }

        void Stop()
        {
            m_ipcSubscriber->Disconnect();
        }


    private:

        std::unique_ptr<Safir::Utilities::Internal::IpcSubscriber> m_ipcSubscriber;

        InfoCb   m_infoCb;

        void RecvDataCb(const char* data, size_t size)
        {
            ControlInfo controlInfo;

            controlInfo.ParseFromArray(data, static_cast<int>(size));

            m_infoCb(controlInfo.incarnation_id(), controlInfo.node_id());

        }
    };

    ControlInfoReceiver::ControlInfoReceiver(boost::asio::io_service&     ioService,
                                             const InfoCb&                infoCb)
        : m_impl(Safir::make_unique<Impl>(ioService,
                                          infoCb))
    {
    }

    void ControlInfoReceiver::Start()
    {
        m_impl->Start();
    }

    void ControlInfoReceiver::Stop()
    {
        m_impl->Stop();
    }

    // Sender impl
    class ControlInfoSender::Impl
        : private boost::noncopyable
    {
    public:

        Impl(boost::asio::io_service&       ioService,
             const std::function<void()>    receiverConnectedCb)
            : m_ipcPublisher(ioService, controlInfoChannel, receiverConnectedCb, NULL)
        {
        }

        void Start()
        {
           m_ipcPublisher.Start();
        }

        void Stop()
        {
           m_ipcPublisher.Stop();
        }

        void  SendInfo(int64_t  incarnationId,
                       int64_t  nodeId)
        {
            ControlInfo info;

            info.set_incarnation_id(incarnationId);
            info.set_node_id(nodeId);

            Send(info);
        }

    private:

        Safir::Utilities::Internal::IpcPublisher m_ipcPublisher;

        void Send(const ControlInfo& info)
        {
            const auto size = info.ByteSize();
            auto data = std::unique_ptr<char[]>(new char[size]);
            info.SerializeWithCachedSizesToArray
                (reinterpret_cast<google::protobuf::uint8*>(data.get()));
            m_ipcPublisher.Send(std::move(data), size);
        }

    };

    ControlInfoSender::ControlInfoSender(boost::asio::io_service&       ioService,
                                         const std::function<void()>    receiverConnectedCb)
        : m_impl(Safir::make_unique<Impl>(ioService, receiverConnectedCb))
    {
    }

    void ControlInfoSender::Start()
    {
        m_impl->Start();
    }

    void ControlInfoSender::Stop()
    {
        m_impl->Stop();
    }

    void ControlInfoSender::SendInfo(int64_t  incarnationId,
                                     int64_t  nodeId)
    {
        m_impl->SendInfo(incarnationId,
                         nodeId);
    }

}
}
}
}


