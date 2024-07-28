/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include "Sender.h"

Sender::Sender(Com::ControlModeTag tag, boost::asio::io_context& ioContext, int64_t nodeId, int64_t nodeType)
    :Receiver(tag, ioContext, nodeId, nodeType)
    ,m_timerSend(ioContext)
    ,m_msgCount(0)
{
    m_timerSend.expires_after(std::chrono::milliseconds(10));
    m_timerSend.async_wait(boost::asio::bind_executor(m_strand, [this](const boost::system::error_code& /*error*/){if (m_running) Send();}));
}

Sender::Sender(Com::DataModeTag tag, boost::asio::io_context& ioContext, int64_t nodeId, int64_t nodeType)
    :Receiver(tag, ioContext, nodeId, nodeType)
    ,m_timerSend(ioContext)
    ,m_msgCount(0)
{
    m_timerSend.expires_after(std::chrono::milliseconds(3000));
    m_timerSend.async_wait(boost::asio::bind_executor(m_strand, [this](const boost::system::error_code& error){if (!error) Send();}));
}

void Sender::Stop()
{
    m_timerSend.cancel();
    Receiver::Stop();
}

void Sender::Print() const
{
    Receiver::Print();
    std::cout<<"Sent: "<<m_msgCount<<std::endl;
}

void Sender::Send()
{
    static const size_t SendQueueCapacity=m_com.SendQueueCapacity(0);

    //send a burst of messages each timeout
    for (int i=0; i<5; ++i)
    {
        if (m_com.NumberOfQueuedMessages(10)>=SendQueueCapacity || m_com.NumberOfQueuedMessages(11)>=SendQueueCapacity)
        {
            break;
        }

        size_t size=20+(rand()%30000); //random message size 20 bytes - 3 kb
        auto msg=Utilities::CreateMsg(++m_msgCount, size);
        if (!m_com.Send(0, 10, msg, size, 0, true)) //send to NodeType 0 (unicast)
        {
            std::cout<<"Send overflow to nodeType 0 "<<std::endl;

        }
        if (!m_com.Send(0, 11, msg, size, 0, true)) //send to NodeType 1 (multicast)
        {
            std::cout<<"Send overflow to nodeType 1 "<<std::endl;
        }
    }

    m_timerSend.expires_after(std::chrono::milliseconds(10));
    m_timerSend.async_wait(boost::asio::bind_executor(m_strand, [this](const boost::system::error_code& error){if (!error) Send();}));
}
