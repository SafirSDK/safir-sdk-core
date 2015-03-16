
/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://www.consoden.se)
*
* Created by: Lars Hagström <lars@foldspace.nu>
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
#include <boost/chrono.hpp>
#include <boost/asio/steady_timer.hpp>
#include <Safir/Dob/PersistentDataReady.h>

class StopHandler
    : public Safir::Dob::StopHandler
    , private boost::noncopyable
{
public:
    explicit StopHandler(boost::asio::io_service& ioService)
        : m_ioService(ioService) {}
    virtual void OnStopOrder() {m_ioService.stop();}
private:
    boost::asio::io_service& m_ioService;

};

class Requestor
    : public Safir::Dob::Requestor
    , private boost::noncopyable
{
public:
    // From Safir::Dob::Requestor
    void OnResponse(const Safir::Dob::ResponseProxy responseProxy) override {}
    void OnNotRequestOverflow() override {}

};

int main()
{
    std::wcout <<"starting" << std::endl;
    boost::asio::io_service ioService;
    Safir::Dob::Connection connection;
    Safir::Utilities::AsioDispatcher dispatcher(connection, ioService);
    StopHandler stopHandler(ioService);
    std::wcout <<"connecting" << std::endl;
    connection.Open(L"dummy_dope", L"0", -1000000, &stopHandler, &dispatcher);

    Requestor r;

    std::wcout <<"signalling" << std::endl;
    connection.ServiceRequest(Safir::Dob::PersistentDataReady::Create(),
                              Safir::Dob::Typesystem::HandlerId(),
                              &r);
    //set a timer
    boost::asio::steady_timer timer(ioService,boost::chrono::milliseconds(1000));
    timer.async_wait([](const boost::system::error_code&){});

    std::wcout <<"running" << std::endl;
    //ioService will only run until the timer has timed out, since that is all the work
    //that there is for it.
    ioService.run();

    return 0;
}
