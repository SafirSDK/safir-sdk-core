/******************************************************************************
*
* Copyright Saab AB, 2017 (http://safir.sourceforge.net)
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
#include <iostream>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <DoseTest/SynchronousPermanentEntity.h>
#include <boost/lexical_cast.hpp>


class StopHandler :
    public Safir::Dob::StopHandler
{
public:
    explicit StopHandler(boost::asio::io_service& ioService)
        : m_ioService(ioService) {}
    void OnStopOrder() override {m_ioService.stop();}
private:
    boost::asio::io_service& m_ioService;

};

class RequestSender
    : public Safir::Dob::Requestor
{
public:
    RequestSender()
    {
        using namespace Safir::Dob;
        m_connection.Attach();

        SendRequests();
    }

private:
    void OnResponse(const Safir::Dob::ResponseProxy responseProxy) override
    {
        auto response = boost::dynamic_pointer_cast<Safir::Dob::ErrorResponse>(responseProxy.GetResponse());
        if (response == nullptr)
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Got unexpected response!",__WFILE__,__LINE__);
        }

        if (response->Code() == L"SafirNotRegistered" && m_numResponses == 0)
        {
            return;
        }

        if (response->Code() == L"SafirTimeout")
        {
            return;
        }

        if (response->Code() != L"Blahonga")
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Got unexpected response code!",__WFILE__,__LINE__);
        }

        ++m_numResponses;
        if (m_numResponses % 500 == 0)
        {
            std::wcout << "Have sent " << m_numResponses << " requests and gotten responses to them" << std::endl;
        }
    }

    void OnNotRequestOverflow() override
    {
        SendRequests();
    }

    void SendRequests()
    {
        for(;;)
        {
            try
            {
                m_connection.CreateRequest(m_request, Safir::Dob::Typesystem::HandlerId(), this);
            }
            catch (const Safir::Dob::OverflowException&)
            {
                return;
            }
        }
    }

    Safir::Dob::SecondaryConnection m_connection;

    DoseTest::SynchronousPermanentEntityPtr m_request = DoseTest::SynchronousPermanentEntity::Create();
    int m_numResponses = 0;
};

int main()
{
    try
    {
        const std::wstring nameCommonPart = L"RequestSender";
        const std::wstring nameInstancePart = Safir::Dob::Typesystem::InstanceId::GenerateRandom().ToString();

        boost::asio::io_service ioService;

        StopHandler stopHandler(ioService);

        Safir::Dob::Connection connection;

        Safir::Utilities::AsioDispatcher dispatcher(connection,ioService);

        connection.Open(nameCommonPart,
                        nameInstancePart,
                        0, // Context
                        &stopHandler,
                        &dispatcher);
        RequestSender sender;
        boost::asio::io_service::work keepRunning(ioService);
        ioService.run();

        connection.Close();
    }
    catch(std::exception & e)
    {
        std::wcout << "Caught std::exception! Contents of exception is:" << std::endl
            << e.what()<<std::endl;
        return 1;
    }
    catch (...)
    {
        std::wcout << "Caught ... exception!" << std::endl;
        return 1;
    }

    std::wcout << "Exiting" << std::endl;

    return 0;
}
