/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagstrom
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
#include <Safir/Application/Tracer.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <boost/lexical_cast.hpp>
#include <iostream>
#include <boost/asio/steady_timer.hpp>

//disable stupid incorrect microsoft warning.
#ifdef _MSC_VER
#pragma warning (disable : 4428)
#endif

class App
    : public Safir::Dob::StopHandler
{
public:
    App()
         : m_dispatcher(m_connection, m_ioContext)
         , m_timer(m_ioContext, std::chrono::milliseconds(10))
         , m_timerLarge(m_ioContext, std::chrono::seconds(20))
         , m_razor(L"Razor")
         , m_rb(L"Rymd-B\u00f6rje") //ö
         , m_large(L"")
    {}

    void OnStopOrder() override {m_ioContext.stop();}

    void Run()
    {
        for (int i = 0; i < 1000; ++i) //allow max 1000 instances
        {
            try
            {
                m_connection.Open(L"sender_connection_name",boost::lexical_cast<std::wstring>(i),0,this,&m_dispatcher);
                break; //connected, stop trying
            }
            catch (const Safir::Dob::NotOpenException&)
            {
                //retry connect in loop
            }
        }

        Safir::Application::TracerBackdoor::Start(m_connection);
        m_timer.async_wait([this](const auto& /*error*/){Timeout();});
        m_timerLarge.async_wait([this](const auto& /*error*/){TimeoutLarge();});
        m_ioContext.run();

        Safir::Application::TracerBackdoor::Stop();
        m_connection.Close();
    }

    void Timeout()
    {
        static int i = 0;
        m_timer.expires_after(std::chrono::milliseconds(10));
        m_timer.async_wait([this](const auto& /*error*/){Timeout();});
        m_razor << "foo" << "bar" << 1234 << std::endl;
        m_rb << L"\u00e5\u00e4\u00f6, blahonga, " << "blahonga, " << "blahonga" << std::endl;
        std::wcout << "Have logged " << i << " times." << std::endl;

        ++i;
    }

    void TimeoutLarge()
    {
        m_timerLarge.expires_after(std::chrono::minutes(1));
        m_timerLarge.async_wait([this](const auto& /*error*/){TimeoutLarge();});
        static const auto text =
            L"123456789012345678901234567890123456789012345678901234567890";

        for (int i = 0; i<30;++i) //should add up to more than 1472 bytes
        {
            m_large << text << std::endl;
        }
    }

private:
    boost::asio::io_context m_ioContext;
    Safir::Dob::Connection m_connection;
    Safir::Utilities::AsioDispatcher m_dispatcher;

    boost::asio::steady_timer m_timer;
    boost::asio::steady_timer m_timerLarge;

    Safir::Application::Tracer m_razor;
    Safir::Application::Tracer m_rb;
    Safir::Application::Tracer m_large;
};


int main()
{
    try
    {
        App app;
        app.Run();
        return 0;
    }
    catch (const std::exception& e)
    {
        std::wcout << "caught exception: " <<e.what() << std::endl;
        return 1;
    }
}
