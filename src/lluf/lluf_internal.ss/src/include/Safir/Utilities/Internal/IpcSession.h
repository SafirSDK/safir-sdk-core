/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
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
#include <cstdint>
#include <memory>
#include <functional>
#include <Safir/Utilities/Internal/SharedCharArray.h>
#include <Safir/Utilities/Internal/AsioStrandWrap.h>

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
        Msg(const Safir::Utilities::Internal::SharedCharArray& data_, const uint32_t size_ )
            : data(data_)
            , size(size_)
        {
        }

        Safir::Utilities::Internal::SharedCharArray   data;
        uint32_t                    size;
    };

    /**
     * Internal implementation class
     */
    template<typename StreamPtr>
    class Session
        : public std::enable_shared_from_this<Session<StreamPtr>>
    {
    public:
        Session(const std::string&                  name,
                const StreamPtr&                    streamPtr,
                boost::asio::io_context::strand&    strand,
                std::function<void()>&              sessionClosedCb)
            : m_name(name),
              m_streamPtr(streamPtr),
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

        void Send(const Safir::Utilities::Internal::SharedCharArray& msg, uint32_t msgSize)
        {
            bool writeInProgress = !m_msgQueue.empty();
            m_msgQueue.push_back(Msg(msg, msgSize));
            if (m_msgQueue.size() > 1000)
            {
                throw std::logic_error("IpcSession send message queue overflow for '" + m_name + "'.");
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

            buffers.push_back(boost::asio::const_buffer(&msg.size, sizeof(msg.size))); // header
            buffers.push_back(boost::asio::const_buffer(msg.data.get(), msg.size));    // msg data

            boost::asio::async_write(*m_streamPtr, buffers,
                Safir::Utilities::Internal::WrapInStrand(m_strand, [this, selfHandle](boost::system::error_code ec, size_t /*length*/)
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

        const std::string                   m_name;
        StreamPtr                           m_streamPtr;
        std::deque<Msg>                     m_msgQueue;
        boost::asio::io_context::strand&    m_strand;
        std::function<void()>               m_sessionClosedCb;
    };

}
}
}

#endif
