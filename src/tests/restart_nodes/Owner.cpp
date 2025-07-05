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
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <DoseTest/SynchronousPermanentEntity.h>
#include <boost/lexical_cast.hpp>
#include <cstdlib>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4244 4267 4100)
#endif

#include <boost/program_options.hpp>
#include <boost/thread.hpp>
#include <boost/chrono.hpp>

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
            ("update-period",
                 value<int>(&period)->default_value(10, ""),
                 "Entity update period, in ms")
            ("handler",
                 value<std::int64_t>(&handler)->default_value(0, ""),
                 "Handler to register")
            ("update",
             "Update entities");

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

        update = vm.count("update") != 0;

        parseOk = true;
    }
    bool parseOk;
    int period;
    bool update;
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



class EntityOwner
    : public Safir::Dob::EntityHandlerInjection
{
public:
    EntityOwner(boost::asio::io_context& ioContext, const bool update, const int period, const std::int64_t handler)
        : m_timer(ioContext)
        , m_handler(handler)
        , m_update(update)
        , m_period(period)
    {
        using namespace Safir::Dob;
        m_connection.Attach();

        m_connection.RegisterEntityHandlerInjection(DoseTest::SynchronousPermanentEntity::ClassTypeId,
                                                    m_handler,
                                                    InstanceIdPolicy::HandlerDecidesInstanceId,
                                                    this);

        if (m_update)
        {
            for (int i = 0; i < 20; ++i)
            {
                auto ent = DoseTest::SynchronousPermanentEntity::Create();
                ent->Info() = L"test" + boost::lexical_cast<std::wstring>(Typesystem::InstanceId::GenerateRandom().GetRawValue());
                m_instances.push_back(Typesystem::InstanceId::GenerateRandom());
                m_connection.SetChanges(ent,
                                        m_instances.back(),
                                        m_handler);
            }

            m_timer.expires_from_now(boost::posix_time::seconds(1));
            m_timer.async_wait([this](const boost::system::error_code&){Update();});
        }
    }

private:
    void Update()
    {
        for (auto inst = m_instances.begin(); inst != m_instances.end(); ++inst)
        {
            auto ent = DoseTest::SynchronousPermanentEntity::Create();
            ent->Info() = L"test" + boost::lexical_cast<std::wstring>
                (Safir::Dob::Typesystem::InstanceId::GenerateRandom().GetRawValue());
            m_connection.SetChanges(ent,
                                    *inst,
                                    m_handler);
        }

        m_timer.expires_from_now(boost::posix_time::milliseconds(m_period));
        m_timer.async_wait([this](const boost::system::error_code&){Update();});
    }

    void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId,
        const Safir::Dob::Typesystem::HandlerId&) override {}

    void OnCreateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
                         Safir::Dob::ResponseSenderPtr        responseSender) override
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(std::rand() % 10));
        responseSender->Send(m_errorResponse);
    }

    void OnUpdateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
        Safir::Dob::ResponseSenderPtr        responseSender) override
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(std::rand() % 10));
        responseSender->Send(m_errorResponse);
    }

    void OnDeleteRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
        Safir::Dob::ResponseSenderPtr        responseSender) override
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(std::rand() % 10));
        responseSender->Send(m_errorResponse);
    }

    void OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy /*injectedEntityProxy*/) override
    {
    }

    void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId,
                                 const Safir::Dob::Typesystem::HandlerId&) override
    {

    }

    Safir::Dob::SecondaryConnection m_connection;
    boost::asio::deadline_timer m_timer;
    std::vector<Safir::Dob::Typesystem::InstanceId> m_instances;

    const Safir::Dob::Typesystem::HandlerId m_handler;
    const bool m_update;
    const int m_period;
    Safir::Dob::ErrorResponsePtr m_errorResponse = Safir::Dob::ErrorResponse::CreateErrorResponse(L"Blahonga",L"");
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
        const std::wstring nameCommonPart = L"Owner";
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
        EntityOwner owner(ioContext, options.update, options.period, options.handler);
        boost::asio::io_context::work keepRunning(ioContext);
        ioContext.run();

        connection.Close();
    }
    catch(const std::exception & e)
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
