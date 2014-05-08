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
#ifndef __LLUF_IPC_PUBLISHER_H__
#define __LLUF_IPC_PUBLISHER_H__

#include <set>
#include <boost/cstdint.hpp>
#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/make_shared.hpp>
#include <boost/function.hpp>
#include <boost/filesystem.hpp>

#include <Safir/Utilities/Internal/IpcSession.h>

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
     * This class implements an Ipc publisher which can be used to send messages
     * that will be received by Ipc subscribers.
     *
     * The Ipc "channel" is identified by a name that the subscriber and publisher has
     * to somehow agree on.
     *
     * Although this is a class template there is a typedef that provides a "do nothing"
     * TestPolicy so a user can declare a publisher like:
     *
     * Safir::Utilities::Internal::IpcPublisher myPublisher;
     *
     * TODO: Currently, the send queues (one for each subscriber) have no upper limits and
     *       there is no concept of overflow when sending a message. We have to decide if this
     *       is ok, or if a more elaborated mechanism is needed.
     */
    template<typename TestPolicy, typename Acceptor>
    class IpcPublisherImpl
        : public boost::enable_shared_from_this<IpcPublisherImpl<TestPolicy, Acceptor>>,
          private boost::noncopyable
    {
    public:

        /**
         * Create an Ipc publisher.
         *
         * @param ioService [in] - io_service that will be used as engine.
         * @param name [in] - Ipc identification.
         */
        static boost::shared_ptr<IpcPublisherImpl>
            Create(boost::asio::io_service&        ioService,
                   const std::string&              name)
        {
                return boost::shared_ptr<IpcPublisherImpl>(new IpcPublisherImpl(ioService, name));
        }

        /**
         * Start to accept connections from subscribers.
         */
        void Start()
        {
            auto selfHandle(this->shared_from_this());

            m_strand.dispatch(
                        [this, selfHandle]()
                        {
                            if (m_acceptor->IsStarted())
                            {
                                return;
                            }

                            TestPolicy::StartListeningEvent();

                            m_acceptor->Start();
                        });
        }


        /**
         * Stop accepting new subscriber connectionsa and terminate any existing connections.
         */
        void Stop()
        {
            auto selfHandle(this->shared_from_this());

            m_strand.dispatch(
                        [this, selfHandle]()
                        {
                            m_sessions.clear();

                            m_acceptor->Stop();

                            TestPolicy::StopListeningEvent();
                        });
        }

        /**
         * Send the given msg to all connected subscribers.
         *
         * The message buffer must not be modified by the user after the call
         * to Send.
         *
         * @param msg [in] - Message to send.
         * @param msgSize [in] - Message size in bytes.
         */
        void Send(const boost::shared_ptr<char[]>& msg, boost::uint32_t msgSize)
        {
            auto selfHandle(this->shared_from_this());

            m_strand.dispatch(
                        [this, selfHandle, msg, msgSize]()
                        {
                            for (auto it = m_sessions.begin(); it != m_sessions.end(); /* it incremented in loop */)
                            {
                                auto& sessionPtr = *it;  // alias for readability

                                if (!sessionPtr->IsOpen())
                                {
                                    it = m_sessions.erase(it);
                                    continue;
                                }

                                sessionPtr->Send(msg, msgSize);

                                ++it;
                            }
                        });
        }

    private:

        IpcPublisherImpl(boost::asio::io_service&   ioService,
                         const std::string&         name)
            : m_strand(ioService),
              m_ioService(ioService),
              m_acceptor(boost::make_shared<Acceptor>(m_strand,
                                                      name,
                                                      [this](typename Acceptor::StreamPtr streamPtr)
                                                      {
                                                          m_sessions.insert(boost::make_shared<SessionType>(streamPtr, m_strand));
                                                          TestPolicy::ConnectionAcceptEvent();
                                                      })),
              m_sessions()
        {
        }

        typedef Session<TestPolicy, typename Acceptor::StreamPtr> SessionType;

        typedef boost::shared_ptr<SessionType> SessionPtr;
        typedef boost::shared_ptr<Acceptor> AcceptorPtr;

        boost::asio::io_service::strand     m_strand;
        boost::asio::io_service&            m_ioService;
        AcceptorPtr                         m_acceptor;
        std::set<SessionPtr>                m_sessions;

    };

    struct IpcPublisherNoTest{
        static void ConnectionAcceptEvent(){}
        static void StartListeningEvent(){}
        static void StopListeningEvent(){}
        static void SubscriberDisconnect(){}
    };

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    typedef IpcPublisherImpl<IpcPublisherNoTest, Safir::Utilities::Internal::Win32Acceptor> IpcPublisher;
#elif defined(linux) || defined(__linux) || defined(__linux__)
    typedef IpcPublisherImpl<IpcPublisherNoTest, LinuxAcceptor> IpcPublisher;
#endif

}
}
}

#endif
