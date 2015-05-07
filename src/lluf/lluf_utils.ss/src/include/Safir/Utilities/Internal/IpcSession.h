/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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
#ifndef __LLUF_IPC_SESSION_H__
#define __LLUF_IPC_SESSION_H__

#include <deque>
#include <vector>
#include <boost/cstdint.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

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

    struct Msg
    {
        boost::shared_ptr<char[]>   data;
        uint32_t                    size;
    };

    /**
     * Internal implementation class
     */
    template<typename StreamPtr>
    class Session
        : public boost::enable_shared_from_this<Session<StreamPtr>>
    {
    public:
        Session(const StreamPtr&                    streamPtr,
                boost::asio::io_service::strand&    strand,
                std::function<void()>               sessionClosedCb)
            : m_streamPtr(streamPtr),
              m_msgQueue(),
              m_strand(strand),
              m_sessionClosedCb(sessionClosedCb)
        {
        }

        ~Session()
        {
            if (m_streamPtr)
            {
                m_streamPtr->close();
            }
        }

        void Send(boost::shared_ptr<char[]> msg, uint32_t msgSize)
        {
            bool writeInProgress = !m_msgQueue.empty();
            m_msgQueue.push_back({msg, msgSize});
            if (m_msgQueue.size() > 1000)
            {
                throw std::logic_error("IpcSession send message queue overflow.");
            }
            if (!writeInProgress)
            {
              Write();
            }
        }

        bool IsOpen() const
        {
            return m_streamPtr && m_streamPtr->is_open();
        }

    private:

        void Write()
        {
            auto selfHandle(this->shared_from_this());

            auto buffers = std::vector<boost::asio::const_buffer>();

            const Msg& msg = m_msgQueue.front(); // make alias to increase readability

            buffers.push_back({&msg.size, sizeof(msg.size)}); // header
            buffers.push_back({msg.data.get(), msg.size});    // msg data

            boost::asio::async_write(*m_streamPtr,
                                     buffers,
                                     m_strand.wrap(
                [this, selfHandle](boost::system::error_code ec, size_t /*length*/)
                {
                    if (!ec)
                    {
                        m_msgQueue.pop_front();
                        if (!m_msgQueue.empty())
                        {
                            Write();
                        }
                    }
                    else
                    {
                        if (!m_streamPtr->is_open())
                        {
                            return;
                        }

                        m_streamPtr->close();

                        if (m_sessionClosedCb != nullptr)
                        {
                            m_sessionClosedCb();
                        }
                    }
                }));
        }

        StreamPtr                           m_streamPtr;
        std::deque<Msg>                     m_msgQueue;
        boost::asio::io_service::strand&    m_strand;
        std::function<void()>               m_sessionClosedCb;
    };

}
}
}

#endif
