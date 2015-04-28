/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén/ anders.widen@consoden.se
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
#ifndef __LLUF_IPC_SUBSCRIBER_H__
#define __LLUF_IPC_SUBSCRIBER_H__

#include <boost/cstdint.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/function.hpp>
#include <boost/filesystem.hpp>
#include <boost/system/error_code.hpp>
#include <Safir/Utilities/Internal/AsioPeriodicTimer.h>
#include <Safir/Utilities/Internal/IpcName.h>

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
namespace Utilities
{
namespace Internal
{

    typedef boost::function<void(const char*, size_t)> RecvDataCallback;

    /**
     * Implementation class. Users should use class IpcSubscriber.
     */
    template<typename TestPolicy>
    class IpcSubscriberImpl
        : public boost::enable_shared_from_this<IpcSubscriberImpl<TestPolicy>>,
          private boost::noncopyable
    {
    public:

        IpcSubscriberImpl(boost::asio::io_service&  ioService,
                          const std::string&        name,
                          const RecvDataCallback&   onRecvData)
            : m_strand(ioService),
              m_stream(ioService),
              m_connectRetryTimer(ioService),
              m_streamId(GetIpcStreamId(name)),
              m_callback(onRecvData),
              m_msgSize(0),
              m_connected(false),
              m_msgRecvBuffer(1500)  // Start with a small buffer size
        {
        }

        void Connect()
        {
            const bool wasConnected = m_connected.exchange(true);

            if (wasConnected)
            {
                return;
            }

            TestPolicy::ConnectEvent();

            ConnectInternal();
        }

        void Disconnect()
        {
            const bool wasConnected = m_connected.exchange(false);

            if (!wasConnected)
            {
                return;
            }

            auto selfHandle(this->shared_from_this());

            m_strand.post([this, selfHandle]()
                          {
                              if (m_stream.is_open())
                              {
                                  m_stream.close();
                              }
                              m_connectRetryTimer.cancel();

                              TestPolicy::DisconnectEvent();
                          });
        }

    private:

        void ConnectInternal()
        {
            auto selfHandle(this->shared_from_this());

            m_strand.dispatch(
                        [this, selfHandle]()
                        {
                            if (m_stream.is_open() || !m_connected)
                            {
                                return;
                            }

                            boost::system::error_code ec;

    #if defined(linux) || defined(__linux) || defined(__linux__)

                            m_stream.connect(boost::asio::local::stream_protocol::endpoint(m_streamId), ec);

    #elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)

                            // Open client end of the named pipe.
                            HANDLE pipe = ::CreateFileA(m_streamId.c_str(),
                                                        GENERIC_READ,
                                                        0,                      // no sharing
                                                        nullptr,                // default security attributes
                                                        OPEN_EXISTING,          // open existing pipe
                                                        FILE_FLAG_OVERLAPPED,   // asynchronous mode
                                                        nullptr);               // no template file

                            if (pipe != INVALID_HANDLE_VALUE)
                            {
                                // Pipe open succeeded
                                m_stream.assign(pipe, ec);
                            }
                            else
                            {
                                // Failed to open pipe.

                                DWORD errorCode = GetLastError();

                                // I can't find an error_condition that is mapped to ERROR_PIPE_BUSY, so we
                                // do a somewhat dirty trick here and pretend it is an ERROR_BROKEN_PIPE
                                if (errorCode == ERROR_PIPE_BUSY)
                                {
                                    errorCode = ERROR_BROKEN_PIPE;
                                }

                                ec.assign(errorCode, boost::system::system_category());
                            }

    #endif
                            if (!ec)
                            {
                                TestPolicy::ConnectedToPublisherEvent();

                                ReadHeader();
                            }
                            else if (ec == boost::asio::error::connection_refused ||
                                     ec == boost::asio::error::broken_pipe ||
                                     ec == boost::system::errc::no_such_file_or_directory)
                            {
                                // Probably the publisher is not up and running. Keep on trying.

                                m_stream.close();

                                Reconnect();
                            }
                            else
                            {
                                std::ostringstream ostr;
                                ostr << "Ec: " << ec << " " << ec.message().c_str()
                                     << L" IPC Subscriber fatal error when connecting to " << m_streamId << std::endl;
                                throw std::logic_error(ostr.str());
                            }
                        });
        }

        void ReadHeader()
        {
            auto selfHandle(this->shared_from_this());

            boost::asio::async_read(m_stream,
                                    boost::asio::buffer(&m_msgSize, sizeof(m_msgSize)),
                                    m_strand.wrap(
                                        [this, selfHandle](boost::system::error_code ec, size_t /*length*/)
                                        {
                                            if (!m_connected)
                                            {
                                                return;
                                            }

                                            if (!ec)
                                            {
                                                ReadMsg();

                                            }
                                            else if (ec == boost::asio::error::operation_aborted)
                                            {
                                                // Async_read was aborted. Most likely caused by a normal Disconnect
                                            }
                                            else
                                            {
                                                m_stream.close();

                                                TestPolicy::DisconnectedFromPublisherEvent();

                                                Reconnect();
                                            }

            }));
        }

        void ReadMsg()
        {
            auto selfHandle(this->shared_from_this());

            if (m_msgRecvBuffer.size() < m_msgSize)
            {
                // The receive buffer size isn't large enough. Make the new buffer twice as large
                // as the needed size to take care of the case where the received messages are
                // continously increasing the size.
                m_msgRecvBuffer.resize(m_msgSize * 2);
            }

            boost::asio::async_read(m_stream,
                                    boost::asio::buffer(m_msgRecvBuffer.data(), m_msgSize),
                                    m_strand.wrap(
                                        [this, selfHandle](boost::system::error_code ec, size_t /*length*/)
                                        {
                                            if (!m_connected)
                                            {
                                                return;
                                            }

                                            if (!ec)
                                            {
                                                m_callback(m_msgRecvBuffer.data(), m_msgSize);

                                                ReadHeader();

                                            }
                                            else if (ec == boost::asio::error::operation_aborted)
                                            {
                                                // Async_read was aborted. Most likely caused by a normal Disconnect
                                            }
                                            else
                                            {
                                                m_stream.close();

                                                TestPolicy::DisconnectedFromPublisherEvent();

                                                Reconnect();
                                            }

            }));
        }

        void Reconnect()
        {
            auto selfHandle(this->shared_from_this());

            m_connectRetryTimer.expires_from_now(boost::posix_time::seconds(1));

            m_connectRetryTimer.async_wait(m_strand.wrap(
                        [this, selfHandle](const boost::system::error_code&)
                        {
                            ConnectInternal();
                        }));
        }

        boost::asio::io_service::strand                                     m_strand;
    #if defined(linux) || defined(__linux) || defined(__linux__)
         boost::asio::local::stream_protocol::socket                        m_stream;
    #elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        boost::asio::windows::stream_handle                                 m_stream;
    #endif
        boost::asio::deadline_timer                                         m_connectRetryTimer;
        const std::string                                                   m_streamId;
        const RecvDataCallback                                              m_callback;
        boost::uint32_t                                                     m_msgSize;
        std::atomic<bool>                                                   m_connected;
        std::vector<char>                                                   m_msgRecvBuffer;
    };

    struct IpcSubscriberNoTest
    {
        static void ConnectEvent(){}
        static void DisconnectEvent(){}
        static void ConnectedToPublisherEvent(){}
        static void DisconnectedFromPublisherEvent(){}
    };


    /**
     * This class implements an Ipc subscriber which can be used to receive messages
     * sent by an Ipc publisher within the same host.
     *
     * The Ipc "channel" is identified by a name that the subscriber and publisher has
     * to somehow agree on.
     */
    class IpcSubscriber
            : private boost::noncopyable
    {
    public:

        /**
         * Constructor.
         *
         * The message buffer returned in the callback is reused by
         * the IpcSubscriberImpl class which means that the user should
         * not make any assumptions about the buffer content after returning
         * from the callback.
         *
         * @param ioService [in] - io_service that will be used as engine.
         * @param name [in] - Ipc identification.
         * @param onRecvData [in] - Callback that will be called when a message is received.
         */
        IpcSubscriber(boost::asio::io_service&     ioService,
                      const std::string&           name,
                      const RecvDataCallback&      onRecvData)
            : m_pimpl(boost::make_shared<IpcSubscriberImpl<IpcSubscriberNoTest>>(ioService, name, onRecvData))
        {
        }

        /**
         * Destructor
         */
        ~IpcSubscriber()
        {
            m_pimpl->Disconnect();
        }

        /**
         * Connect and start reception of messages.
         *
         * Cyclic retries will be performed until a connection is established. This means
         * that it is ok for a subscriber to call Connect before the publisher is started.
         */
        void Connect()
        {
            m_pimpl->Connect();
        }

        /**
         * Disconnect and stop reception of messages.
         */
        void Disconnect()
        {
            m_pimpl->Disconnect();
        }

    private:
        boost::shared_ptr<IpcSubscriberImpl<IpcSubscriberNoTest>> m_pimpl;
    };
}
}
}

#endif

