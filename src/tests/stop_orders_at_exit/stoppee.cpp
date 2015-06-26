/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <Safir/Utilities/AsioDispatcher.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/NotOpenException.h>
#include <boost/lexical_cast.hpp>
#include <iostream>

class App
    : public Safir::Dob::StopHandler
{
public:
    App()
        : m_ioService()
        , m_work (new boost::asio::io_service::work(m_ioService))
        , m_dispatcher(m_connection, m_ioService) {}

    void OnStopOrder() {m_work.reset();}
    void Run()
    {
        for (int i = 0; i < 1000; ++i) //allow max 1000 instances
        {
            try
            {
                m_connection.Open(L"sender",boost::lexical_cast<std::wstring>(i),0,this,&m_dispatcher);
                break; //connected, stop trying
            }
            catch (const Safir::Dob::NotOpenException&)
            {
                //retry connect in loop
            }
        }

        std::wcout << "Connected sucessfully" << std::endl;
        m_ioService.run();

        m_connection.Close();
    }
private:
    boost::asio::io_service m_ioService;
    std::unique_ptr<boost::asio::io_service::work> m_work;// {new boost::asio::io_service::work(m_ioService)};
    Safir::Dob::Connection m_connection;
    Safir::Utilities::AsioDispatcher m_dispatcher;
};


int main()
{
    try
    {
        App app;
        app.Run();
        return 0;
    }
    catch (...)
    {
        std::wcout << "caught exception" << std::endl;
        return 1;
    }
}
