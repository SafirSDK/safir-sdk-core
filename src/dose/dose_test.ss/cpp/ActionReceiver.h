/******************************************************************************
*
* Copyright Saab Systems AB, 2012 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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

#ifndef __ACTIONRECEIVER_H__
#define __ACTIONRECEIVER_H__

#include <cstdlib>
#include <iostream>
#include <boost/bind.hpp>
#include <boost/asio.hpp>
#include <Safir/Dob/Typesystem/BlobOperations.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <DoseTest/Action.h>

class ActionReceiver
{
    typedef boost::shared_ptr<boost::asio::ip::tcp::socket> SocketPtr;
public:
    ActionReceiver(boost::asio::io_service& ioService,
                   const boost::function<void(const DoseTest::ActionPtr&)>& actionCallback)
        : m_ioService(ioService)
        , m_port(-1)
        , m_actionCallback(actionCallback)
    {


    }
    
    void Open()
    {
        using namespace boost::asio::ip;

        std::wcout << "Opening ActionReceiver" << std::endl;
        const short startPort = 30000;
        for (short i = 0; i < 100; ++i)
        {
            try
            {
                m_acceptor.reset
                    (new tcp::acceptor(m_ioService,
                                       tcp::endpoint(tcp::v4(), 
                                                     startPort + i)));
                std::wcout << "accepting connections on port " << startPort + i << std::endl;
                m_port = startPort + i;
                break;
            }
            catch (const boost::system::system_error& error)
            {
                std::wcout << "Failed to accept on port " << startPort + i << ": " << error.what() << std::endl;
            }
        }
        
        if (m_acceptor == NULL)
        {
            throw std::logic_error("Failed to open any useful port!");
        }

        m_socket.reset(new tcp::socket(m_ioService));
        m_acceptor->async_accept(*m_socket,
                                 boost::bind(&ActionReceiver::HandleAccept, this,
                                             boost::asio::placeholders::error));
    }

    void Close()
    {
        std::wcout << "Closing ActionReceiver" << std::endl;
        m_acceptor.reset();
        m_socket.reset();
    }

    ~ActionReceiver()
    {
        Close();
    }

    short Port() const
    {
        return m_port;
    }

private:
    void HandleAccept(const boost::system::error_code& error)
    {
        if (!error)
        {
            m_data.resize(BLOB_HEADER_SIZE); 
            m_socket->async_receive(boost::asio::buffer(&m_data[0], m_data.size()),
                                    boost::bind(&ActionReceiver::HandleRead, this,
                                                boost::asio::placeholders::error,
                                                boost::asio::placeholders::bytes_transferred));
        }
        else if (m_acceptor != NULL) //expect a cancelled error when closing acceptor
        {
            std::wcout << "Error in HandleAccept: " << error << std::endl;
            //            throw std::logic_error("Error in HandleAccept!");
        }

    }

    void HandleRead(const boost::system::error_code& error,
                    const size_t /*bytes_transferred*/)
    {
        if (!error)
        {
            const Safir::Dob::Typesystem::Int32 blobSize = Safir::Dob::Typesystem::BlobOperations::GetSize(&m_data[0]);
            if (m_data.size() < static_cast<size_t>(blobSize))
            {
                m_data.resize(blobSize);
            }


            try
            {
                boost::asio::read(*m_socket,
                                  boost::asio::buffer(&m_data[BLOB_HEADER_SIZE], blobSize - BLOB_HEADER_SIZE));
            }
            catch (const boost::system::system_error&)
            {
                std::wcout << "Error in HandleRead receive: " << error << std::endl;
                Close();
            }

            DoseTest::ActionPtr action =
                boost::static_pointer_cast<DoseTest::Action>
                (Safir::Dob::Typesystem::Serialization::ToObject(m_data));

            //std::wcout << "Got an action " << std::endl;
            
            const bool actionAfterAck = action->ActionKind() == DoseTest::ActionEnum::Sleep;

            if (!actionAfterAck)
            {
                m_actionCallback(action);
            }

            try
            {
                //std::wcout << "writing ok" << std::endl;
                boost::asio::write(*m_socket, boost::asio::buffer("ok", 3));
            }
            catch (const boost::system::system_error&)
            {
                std::wcout << "writing failed" << std::endl;
            }

            //start next receive
            m_socket->async_receive(boost::asio::buffer(&m_data[0], BLOB_HEADER_SIZE),
                                    boost::bind(&ActionReceiver::HandleRead, this,
                                                boost::asio::placeholders::error,
                                                boost::asio::placeholders::bytes_transferred));

            if (actionAfterAck)
            {
                m_actionCallback(action);
            }
        }
        else
        {
            std::wcout << "Error in HandleRead: " << error << std::endl;
            Close();
        }
    }

private:
    boost::asio::io_service& m_ioService;
    boost::shared_ptr<boost::asio::ip::tcp::acceptor> m_acceptor;
    SocketPtr m_socket;
    std::vector<char> m_data;

    enum {BLOB_HEADER_SIZE = 16};
    
    short m_port;
    const boost::function<void(const DoseTest::ActionPtr&)> m_actionCallback;
};


#endif

