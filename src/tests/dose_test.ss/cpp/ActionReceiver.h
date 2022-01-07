/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
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
#include <Safir/Dob/Typesystem/Internal/BlobOperations.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <DoseTest/Action.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif


namespace boost
{
namespace system
{
    //Since this is an overload for a boost class we need to put it in the boost namespace.
    static inline std::wostream& operator<<(std::wostream& out, const boost::system::error_code& error)
    {
        std::ostringstream tmp;
        tmp << error;
        return out << tmp.str().c_str();
    }

}
}

class ActionReceiver
{
    typedef boost::shared_ptr<boost::asio::ip::tcp::socket> SocketPtr;
public:
    ActionReceiver(boost::asio::io_service& ioService,
                   const boost::function<void(const DoseTest::ActionPtr&)>& actionCallback,
                   const int instance)
        : m_ioService(ioService)
        , m_port(-1)
        , m_actionCallback(actionCallback)
        , m_instance(instance)
    {


    }

    void Open()
    {
        using namespace boost::asio::ip;

        std::wcout << "Opening ActionReceiver" << std::endl;
        const short startPort = static_cast<short>(30000 + m_instance);
        for (short i = 0; i < 100; ++i)
        {
            const short port = startPort + i * 3;
            try
            {
                m_acceptor.reset
                    (new tcp::acceptor(m_ioService,
                                       tcp::endpoint(tcp::v4(),
                                                     port),
                                       false)); //no reuseaddr
                //reuseaddr doesnt work on windows. see:
                //http://stackoverflow.com/questions/7164879/boost-asio-why-dont-i-get-bind-address-already-in-use-in-windows-but-do-ge

                std::wcout << "accepting connections on port " << port << std::endl;
                m_port = port;
                break;
            }
            catch (const boost::system::system_error& error)
            {
                std::wcout << "Failed to accept on port " << port << ": " << error.what() << std::endl;
            }
        }

        if (m_acceptor == NULL)
        {
            throw std::logic_error("Failed to open any useful port!");
        }

        m_socket.reset(new tcp::socket(m_ioService));
        m_acceptor->async_accept(*m_socket,
                                 [this](const auto& error){HandleAccept(error);});
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
            m_socket->async_receive
                (boost::asio::buffer(&m_data[0], m_data.size()),
                 [this](const auto& error, const size_t bytes_transferred){HandleRead(error,bytes_transferred);});
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
            const Safir::Dob::Typesystem::Int32 blobSize = Safir::Dob::Typesystem::Internal::BlobOperations::GetSize(&m_data[0]);
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

            std::wcout << "Got action '" << DoseTest::ActionEnum::ToString(action->ActionKind()) << "'" << std::endl;

            const bool actionAfterAck = action->ActionKind() == DoseTest::ActionEnum::Sleep;

            std::wcout << "Got actionAfterAck = " << actionAfterAck << std::endl;

            if (!actionAfterAck)
            {
                std::wcout << "Performing action" << std::endl;
                m_actionCallback(action);
            }

            try
            {
                std::wcout << "writing ok" << std::endl;
                boost::asio::write(*m_socket, boost::asio::buffer("ok", 3));
            }
            catch (const boost::system::system_error&)
            {
                std::wcout << "writing failed" << std::endl;
            }

            //start next receive
            m_socket->async_receive
                (boost::asio::buffer(&m_data[0], BLOB_HEADER_SIZE),
                 [this](const auto& error, const size_t bytes_transferred){HandleRead(error,bytes_transferred);});

            if (actionAfterAck)
            {
                std::wcout << "Performing action" << std::endl;
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
    const int m_instance;
};


#endif
