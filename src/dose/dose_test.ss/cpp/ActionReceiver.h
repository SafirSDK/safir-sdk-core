/******************************************************************************
*
* Copyright Saab Systems AB, 2012 (http://www.safirsdk.com)
*
* Created by: Lars Hagstr�m / lars@foldspace.nu
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

class ActionReceiver
{
public:
    explicit ActionReceiver(boost::asio::io_service& ioService)
        : m_ioService(ioService)
        , m_socket(ioService)
        , m_port(-1)
    {
        std::wcout << "Constructing ActionReceiver" << std::endl;
        const short startPort = 30000;
        for (short i = 0; i < 100; ++i)
        {
            try
            {
                m_acceptor.reset(new boost::asio::ip::tcp::acceptor(ioService, 
                                                                    boost::asio::ip::tcp::endpoint(boost::asio::ip::tcp::v4(), 
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

        m_acceptor->async_accept(m_socket,
                                 boost::bind(&ActionReceiver::HandleAccept, this,
                                             boost::asio::placeholders::error));

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
            m_socket.async_receive(boost::asio::buffer(&m_data[0], m_data.size()),
                                   boost::bind(&ActionReceiver::HandleRead, this,
                                               boost::asio::placeholders::error,
                                               boost::asio::placeholders::bytes_transferred));
        }
        else
        {
            std::wcout << "Error in HandleAccept: " << error << std::endl;
            throw std::logic_error("Error in HandleAccept!");
        }

    }

    void HandleRead(const boost::system::error_code& error,
                    const size_t bytes_transferred)
    {
        if (!error)
        {
            const Safir::Dob::Typesystem::Int32 blobSize = Safir::Dob::Typesystem::BlobOperations::GetSize(&m_data[0]);
            if (m_data.size() < static_cast<size_t>(blobSize))
            {
                m_data.resize(blobSize);
            }

            boost::system::error_code receiveError;
            boost::asio::read(m_socket,
                              boost::asio::buffer(&m_data[BLOB_HEADER_SIZE], m_data.size() - BLOB_HEADER_SIZE),
                              receiveError);
            
            if (receiveError)
            {
                std::wcout << "Error in HandleRead receive: " << error << std::endl;
                throw std::logic_error("Error in HandleRead receive!");
            }

            m_socket.async_receive(boost::asio::buffer(&m_data[0], BLOB_HEADER_SIZE),
                                   boost::bind(&ActionReceiver::HandleRead, this,
                                               boost::asio::placeholders::error,
                                               boost::asio::placeholders::bytes_transferred));
        }
        else
        {
            std::wcout << "Error in HandleRead: " << error << std::endl;
            throw std::logic_error("Error in HandleRead!");
        }
    }

private:
    boost::asio::io_service& m_ioService;
    boost::shared_ptr<boost::asio::ip::tcp::acceptor> m_acceptor;
    boost::asio::ip::tcp::socket m_socket;
    std::vector<char> m_data;

    enum {BLOB_HEADER_SIZE = 16};
    
    short m_port;
};


#endif

