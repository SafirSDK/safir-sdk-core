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
#ifndef __LLUF_IPC_ACCEPTOR_WIN32_H__
#define __LLUF_IPC_ACCEPTOR_WIN32_H__

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)

#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/make_shared.hpp>
#include <boost/function.hpp>
#include <boost/system/error_code.hpp>
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
    class Win32Acceptor
            : public boost::enable_shared_from_this<Win32Acceptor>
    {
    public:
        typedef boost::shared_ptr<boost::asio::windows::stream_handle>  StreamPtr;
        typedef boost::function<void(StreamPtr)>                        StreamCreatedCallback;

        // The Acceptor class uses shared_from_this so remember to always access it via a shared pointer.
        Win32Acceptor(boost::asio::io_service::strand& strand,
                      const std::string&               name,
                      const StreamCreatedCallback&     onStreamCreated)
            : m_strand(strand),
              m_pipeName(GetIpcStreamId(name)),
              m_callback(onStreamCreated),
              m_currentConnectHandle()
        {
        }

        void Start()
        {
            if (IsStarted())
            {
                return;
            }
            AcceptConnection();
        }

        void Stop()
        {
            if (m_currentConnectHandle)
            {
                m_currentConnectHandle->close();
                m_currentConnectHandle.reset();
            }
        }

        bool IsStarted() const
        {
            return m_currentConnectHandle && m_currentConnectHandle->is_open();
        }


    private:

        void AcceptConnection()
        {
            auto selfHandle(this->shared_from_this());

            m_strand.dispatch(
                        [this, selfHandle]()
                        {
                            DWORD openMode = PIPE_ACCESS_OUTBOUND |     // outbound for publisher
                                             FILE_FLAG_OVERLAPPED;      // allow async operation

                            // Pipes are only run in byte mode
                            DWORD pipeMode = PIPE_TYPE_BYTE |
                                             PIPE_WAIT |
                                             PIPE_REJECT_REMOTE_CLIENTS;

                            // This is used for the local buffer size in the CreateNamedPipe call.
                            // The parameters are "advisory" according to MS documentation. Changing
                            // this size seems not to change any observable behaviour. (The size 8192
                            // was found in an MS example).
                            const DWORD localBufferSize = 8192;

                            // Create the named pipe.
                            HANDLE pipe = ::CreateNamedPipeA(m_pipeName.c_str(),
                                                             openMode,
                                                             pipeMode,
                                                             PIPE_UNLIMITED_INSTANCES,
                                                             localBufferSize,            // output buffer size (advisory)
                                                             localBufferSize,            // input buffer size (advisory)
                                                             0,                          // defult time-out
                                                             nullptr);                   // default security attributes TODO: Is this good enough?

                            // Throw if we got an error creating the pipe.
                            if (pipe == INVALID_HANDLE_VALUE)
                            {
                                std::ostringstream ostr;
                                ostr << "Error " << ::GetLastError() << " attempting to create named pipe: " << m_pipeName << std::endl;
                                throw std::logic_error(ostr.str());
                            }

                            // Save pointer to "current connect handle" so we can close it in case of a stop call
                            m_currentConnectHandle = boost::make_shared<boost::asio::windows::stream_handle>(m_strand.get_io_service(), pipe);

                            // Construct an overlapped_ptr object with a handler that is called when a subscriber has connected,
                            // that is, when the overlapped ConnectNamedPipe call has completed.
                            boost::asio::windows::overlapped_ptr overlappedPtr(m_strand.get_io_service(),
                                                                               m_strand.wrap(
                                [this, selfHandle](const boost::system::error_code ec, std::size_t)
                                {
                                    if (!IsStarted())
                                    {
                                        // Do nothing if the publisher has been stopped
                                        return;
                                    }

                                    if (!ec)
                                    {
                                        m_callback(m_currentConnectHandle);

                                        AcceptConnection();
                                    }
                                    else if (ec == boost::asio::error::broken_pipe)
                                    {
                                        // This is most likely caused by a normal Disconnect. Do nothing.
                                    }
                                    else if (ec.category() == boost::system::system_category() && ec.value() == ERROR_PIPE_CONNECTED)
                                    {
                                        // Under certain circumstances (unclear what) we get this when the publisher i stopped. Do nothing.
                                    }
                                    else
                                    {
                                        std::ostringstream ostr;
                                        ostr << "ConnectNamedPipe completed with fatal error " << ec.message().c_str()
                                             << ". Pipe: " << m_pipeName << std::endl;
                                        throw std::logic_error(ostr.str());
                                    }
                                }));

                            // Connect to the named pipe. If the connection doesn't succeed, then wait
                            // on the event for for a subscriber to connect. If the subscriber connected between
                            // the call to CreateNamedPipe and ConnectNamedPipe, we handle the ERROR_PIPE_CONNECTED error.
                            BOOL connected = ::ConnectNamedPipe(m_currentConnectHandle->native_handle(), overlappedPtr.get());
                            if (connected)
                            {
                                // The named pipe is created in overlapped (asynchronous) mode and according to the MS
                                // documentation ConnectNamedPipe should never return true in this case.
                                std::ostringstream ostr;
                                ostr << "ConnectNamedPipe returned true, which is not expected in overlapped mode!" << std::endl;
                                throw std::logic_error(ostr.str());
                            }

                            DWORD lastError = ::GetLastError();

                            if (lastError == ERROR_IO_PENDING)
                            {
                                // Overlapped operation successfully initiated.
                                // Release ownership of the overlapped object.
                                overlappedPtr.release();
                            }
                            else if (lastError == ERROR_PIPE_CONNECTED)
                            {
                                // A subscriber has connected between the CreateNamedPipeA call and the ConnectNamedPipe call.
                                // In this case we need to post a completion notification ourself.
                                boost::system::error_code ec(lastError,
                                                             boost::asio::error::get_system_category());
                                overlappedPtr.complete(ec, 0);
                            }
                            else
                            {
                                std::ostringstream ostr;
                                ostr << "GetLastError returned an unexpected error code: " << lastError << std::endl;
                                throw std::logic_error(ostr.str());
                            }

                        });
        }

        boost::asio::io_service::strand&                        m_strand;
        std::string                                             m_pipeName;
        const StreamCreatedCallback                             m_callback;
        boost::shared_ptr<boost::asio::windows::stream_handle>  m_currentConnectHandle;

    };
}
}
}

#endif
#endif
