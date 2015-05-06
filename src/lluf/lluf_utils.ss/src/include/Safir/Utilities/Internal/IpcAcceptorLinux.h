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
#ifndef __LLUF_IPC_ACCEPTOR_LINUX_H__
#define __LLUF_IPC_ACCEPTOR_LINUX_H__

#if defined(linux) || defined(__linux) || defined(__linux__)

#include <set>
#include <boost/cstdint.hpp>
#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/make_shared.hpp>
#include <boost/function.hpp>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Utilities/Internal/IpcName.h>


namespace Safir
{
namespace Utilities
{
namespace Internal
{

    class LinuxAcceptor
        : public boost::enable_shared_from_this<LinuxAcceptor>
    {
    public:
        typedef boost::shared_ptr<boost::asio::local::stream_protocol::socket>  StreamPtr;
        typedef boost::function<void(StreamPtr)>                                StreamCreatedCallback;

        // The Acceptor class uses shared_from_this so remember to always access it via a shared pointer.
        LinuxAcceptor(boost::asio::io_service::strand& strand,
                      const std::string&               name,
                      const StreamCreatedCallback&     onStreamCreated)
            : m_strand(strand),
              m_ipcPath(GetIpcStreamId(name)),
              m_callback(onStreamCreated),
              m_endpoint(m_ipcPath),
              m_acceptor(strand.get_io_service())
        {
        }

        void Start()
        {
            if (IsStarted())
            {
                return;
            }

            ::unlink(m_ipcPath.c_str());

            m_acceptor.open(m_endpoint.protocol());
            m_acceptor.bind(m_endpoint);
            m_acceptor.listen();

            AcceptConnection();

        }

        void Stop()
        {
            m_acceptor.close();

            ::unlink(m_ipcPath.c_str());
        }

        bool IsStarted() const
        {
            return m_acceptor.is_open();
        }

    private:

        void AcceptConnection()
        {
            auto selfHandle(this->shared_from_this());

            auto streamPtr =
                    boost::make_shared<boost::asio::local::stream_protocol::socket>(m_strand.get_io_service());

            m_acceptor.async_accept(*streamPtr,
                                    m_strand.wrap(
                                        [this, selfHandle, streamPtr](boost::system::error_code ec)
            {
                if (!ec)
                {
                    m_callback(streamPtr);

                    AcceptConnection();
                }
                else if (ec == boost::asio::error::operation_aborted)
                {
                    // The operation is aborted. This is most likely caused by a normal Disconnect.
                }
                else
                {
                    std::ostringstream ostr;
                    ostr << "async_accept completed with fatal error " << ec.message().c_str()
                         << ". Endpoint: " << m_endpoint.path().c_str() << std::endl;
                    throw std::logic_error(ostr.str());
                }
            }));
        }

        boost::asio::io_service::strand&                    m_strand;
        std::string                                         m_ipcPath;
        const StreamCreatedCallback                         m_callback;
        boost::asio::local::stream_protocol::endpoint       m_endpoint;
        boost::asio::local::stream_protocol::acceptor       m_acceptor;
    };
}
}
}

#endif
#endif
