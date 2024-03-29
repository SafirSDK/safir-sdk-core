/******************************************************************************
*
* Copyright Saab AB, 2013-2014 (http://safirsdkcore.com)
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
#ifndef __LLUF_IPC_PUBLISHER_H__
#define __LLUF_IPC_PUBLISHER_H__

#include <set>
#include <functional>
#include <memory>
#include <boost/filesystem.hpp>

#include <Safir/Utilities/Internal/IpcSession.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif


#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#  include <Safir/Utilities/Internal/IpcAcceptorWin32.h>
#elif defined(linux) || defined(__linux) || defined(__linux__)
#  include <Safir/Utilities/Internal/IpcAcceptorLinux.h>
#endif

namespace Safir
{
namespace Utilities
{
namespace Internal
{

    /**
     * Implementation class. Users should use class IpcPublisher.
     */
    template<typename TestPolicy, typename Acceptor>
    class IpcPublisherImpl
        : public std::enable_shared_from_this<IpcPublisherImpl<TestPolicy, Acceptor>>,
          private boost::noncopyable
    {
    public:

        IpcPublisherImpl(boost::asio::io_context&       io,
                         const std::string&             name,
                         const std::function<void()>&   subscriberConnectedCb,
                         const std::function<void()>&   subscriberDisconnectedCb)
            : m_running(false),
              m_strand(io),
              m_acceptor(),
              m_sessions(),
              m_subscriberConnectedCb(subscriberConnectedCb),
              m_subscriberDisconnectedCb(subscriberDisconnectedCb)
        {
            m_acceptor = std::make_shared<Acceptor>(
                             m_strand,
                             name,
                             [this, name] (typename Acceptor::StreamPtr streamPtr)
                             {
                                 m_sessions.insert(std::make_shared<SessionType>
                                                   (name, streamPtr, m_strand, m_subscriberDisconnectedCb));

                                 if (m_subscriberConnectedCb != nullptr)
                                 {
                                    m_subscriberConnectedCb();
                                 }
                             });
        }

        void Start()
        {
            const bool wasRunning = m_running.exchange(true);
            if (wasRunning)
            {
                return;
            }

            auto selfHandle(this->shared_from_this());

            boost::asio::dispatch(m_strand,
                        [this, selfHandle]()
                        {
                            if (m_acceptor->IsStarted())
                            {
                                return;
                            }

                            StartListeningEvent();

                            m_acceptor->Start();
                        });
        }

        void Stop()
        {
            const bool wasRunning = m_running.exchange(false);
            if (!wasRunning)
            {
                return;
            }

            auto selfHandle(this->shared_from_this());

            boost::asio::post(m_strand,
                         [this, selfHandle]()
                         {
                             m_sessions.clear();
                             m_acceptor->Stop();

                             StopListeningEvent();
                         });
        }

        void Send(std::unique_ptr<char[]> msg, uint32_t msgSize)
        {
            if (!m_running)
            {
                return;
            }

            auto selfHandle(this->shared_from_this());

            Safir::Utilities::Internal::SharedCharArray msgSharedPtr(std::move(msg));

            boost::asio::dispatch(m_strand,
                        [this, selfHandle, msgSharedPtr, msgSize]()
                        {
                            for (auto it = m_sessions.begin(); it != m_sessions.end(); /* it incremented in loop */)
                            {
                                auto& sessionPtr = *it;  // alias for readability

                                if (!sessionPtr->IsOpen())
                                {
                                    it = m_sessions.erase(it);
                                    continue;
                                }

                                sessionPtr->Send(msgSharedPtr, msgSize);

                                ++it;
                            }
                        });
        }

    private:


        void StartListeningEvent()
        {
             TestPolicy::StartListeningEvent();
        }

        void StopListeningEvent()
        {
            TestPolicy::StopListeningEvent();
        }

        typedef Session<typename Acceptor::StreamPtr> SessionType;

        typedef std::shared_ptr<SessionType> SessionPtr;
        typedef std::shared_ptr<Acceptor> AcceptorPtr;

        std::atomic<bool>                   m_running;
        boost::asio::io_context::strand     m_strand;
        AcceptorPtr                         m_acceptor;
        std::set<SessionPtr>                m_sessions;
        std::function<void()>               m_subscriberConnectedCb;
        std::function<void()>               m_subscriberDisconnectedCb;
    };

    struct IpcPublisherNoTest
    {
        static void StartListeningEvent(){}
        static void StopListeningEvent(){}
    };

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    typedef IpcPublisherImpl<IpcPublisherNoTest, Safir::Utilities::Internal::Win32Acceptor> IpcPubImpl;
#elif defined(linux) || defined(__linux) || defined(__linux__)
    typedef IpcPublisherImpl<IpcPublisherNoTest, LinuxAcceptor> IpcPubImpl;
#endif

    /**
     * This class implements an Ipc publisher which can be used to send messages
     * that will be received by Ipc subscribers.
     *
     * The Ipc "channel" is identified by a name that the subscriber and publisher has
     * to somehow agree on.
     */
    class IpcPublisher
            : private boost::noncopyable
    {
    public:

        /**
         * Constructor
         *
         * @param io [in] - io_context that will be used as engine.
         * @param name [in] - Ipc identification.
         * @param subscriberConnectedCb [in] - Called when a subscriber connects.
         *                                     Provide a nullptr if no calback is wanted.
         * @param subscriberDisconnectedCb [in] - Called when a subscriber disconnects.
         *                                        Provide a nullptr if no callback is wanted.
         *                                        Note that detection of a disconnected subscriber is done only
         *                                        when the publisher is sending.
         */
        IpcPublisher(boost::asio::io_context&       io,
                     const std::string&             name,
                     const std::function<void()>&   subscriberConnectedCb,
                     const std::function<void()>&   subscriberDisconnectedCb)
            : m_pimpl(std::make_shared<IpcPubImpl>(io,
                                                     name,
                                                     subscriberConnectedCb,
                                                     subscriberDisconnectedCb))
        {
        }

        /**
         * Destructor
         */
        ~IpcPublisher()
        {
            m_pimpl->Stop();
        }

        /**
         * Start to accept connections from subscribers.
         */
        void Start()
        {
            m_pimpl->Start();
        }

        /**
         * Stop accepting new subscriber connections and terminate any existing connections.
         */
        void Stop()
        {
            m_pimpl->Stop();
        }

        /**
         * Send the given msg to all connected subscribers.
         *
         * @param msg [in] - Message to send.
         * @param msgSize [in] - Message size in bytes.
         */
        void Send(std::unique_ptr<char[]> msg, uint32_t msgSize)
        {
            m_pimpl->Send(std::move(msg), msgSize);
        }

    private:
        std::shared_ptr<IpcPubImpl> m_pimpl;

    };

}
}
}

#endif
