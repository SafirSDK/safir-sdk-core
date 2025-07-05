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

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4244 4267 4100)
#endif

#include <boost/program_options.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif


std::wostream& operator<<(std::wostream& out, const boost::program_options::options_description& opt)
{
    std::ostringstream ostr;
    ostr << opt;
    return out << ostr.str().c_str();
}

class ProgramOptions
{
public:
    ProgramOptions(int argc, char* argv[])
        : parseOk(false)
    {
        using namespace boost::program_options;
        options_description options("Options");
        options.add_options()
            ("help,h", "show help message")
            ("handler",
                 value<std::int64_t>(&handler)->default_value(0, ""),
             "Handler to send requests to");

        variables_map vm;

        try
        {
            store(command_line_parser(argc, argv).
                  options(options).run(), vm);
            notify(vm);
        }
        catch (const std::exception& exc)
        {
            std::wcerr << "Error parsing command line: " << exc.what() << "\n" << std::endl;
            ShowHelp(options);
            return;
        }

        if (vm.count("help"))
        {
            ShowHelp(options);
            return;
        }

        parseOk = true;
    }
    bool parseOk;
    std::int64_t handler;
private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                   << "Usage: control [OPTIONS]\n"
                   << desc
                   << std::endl;
    }

};

class StopHandler :
    public Safir::Dob::StopHandler
{
public:
    explicit StopHandler(boost::asio::io_context& ioContext)
        : m_ioContext(ioContext) {}
    void OnStopOrder() override {m_ioContext.stop();}
private:
    boost::asio::io_context& m_ioContext;

};

class RequestSender
    : public Safir::Dob::Requestor
{
public:
    explicit RequestSender(const std::int64_t handler)
        : m_handler(handler)
    {
        using namespace Safir::Dob;
        m_connection.Attach();

        SendRequests();
    }

private:
    void OnResponse(const Safir::Dob::ResponseProxy responseProxy) override
    {
        auto response = std::dynamic_pointer_cast<Safir::Dob::ErrorResponse>(responseProxy.GetResponse());
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
                m_connection.CreateRequest(m_request, m_handler, this);
            }
            catch (const Safir::Dob::OverflowException&)
            {
                return;
            }
        }
    }

    Safir::Dob::SecondaryConnection m_connection;
    const Safir::Dob::Typesystem::HandlerId m_handler;

    DoseTest::SynchronousPermanentEntityPtr m_request = DoseTest::SynchronousPermanentEntity::Create();
    int m_numResponses = 0;
};

int main(int argc, char * argv[])
{
    const ProgramOptions options(argc, argv);
    if (!options.parseOk)
    {
        return 1;
    }

    try
    {
        const std::wstring nameCommonPart = L"RequestSender";
        const std::wstring nameInstancePart = Safir::Dob::Typesystem::InstanceId::GenerateRandom().ToString();

        boost::asio::io_context ioContext;

        StopHandler stopHandler(ioContext);

        Safir::Dob::Connection connection;

        Safir::Utilities::AsioDispatcher dispatcher(connection,ioContext);

        connection.Open(nameCommonPart,
                        nameInstancePart,
                        0, // Context
                        &stopHandler,
                        &dispatcher);
        RequestSender sender(options.handler);
        auto keepRunning = boost::asio::make_work_guard(ioContext);
        ioContext.run();

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
