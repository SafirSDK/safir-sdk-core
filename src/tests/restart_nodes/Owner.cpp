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
#include <Safir/Utilities/AsioDispatcher.h>
#include <DoseTest/SynchronousPermanentEntity.h>
#include <boost/lexical_cast.hpp>

class StopHandler :
    public Safir::Dob::StopHandler
{
public:
    explicit StopHandler(boost::asio::io_service& ioService)
        : m_ioService(ioService) {}
    virtual void OnStopOrder() {m_ioService.stop();}
private:
    boost::asio::io_service& m_ioService;

};

class EntityOwner
    : public Safir::Dob::EntityHandlerInjection
{
public:
    explicit EntityOwner(boost::asio::io_service& ioService)
        : m_ioService(ioService)
        , m_timer(ioService)
        , m_handler(Safir::Dob::Typesystem::InstanceId::GenerateRandom().GetRawValue())
    {
        using namespace Safir::Dob;
        m_connection.Attach();

        m_connection.RegisterEntityHandlerInjection(DoseTest::SynchronousPermanentEntity::ClassTypeId,
                                                    m_handler,
                                                    InstanceIdPolicy::HandlerDecidesInstanceId,
                                                    this);

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
        m_timer.async_wait([=](const boost::system::error_code&){Update();});
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

        m_timer.expires_from_now(boost::posix_time::milliseconds(10));
        m_timer.async_wait([=](const boost::system::error_code&){Update();});
    }
    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId,
        const Safir::Dob::Typesystem::HandlerId&) {}

    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
        Safir::Dob::ResponseSenderPtr        /*responseSender*/) {}

    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
        Safir::Dob::ResponseSenderPtr        /*responseSender*/) {}

    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
        Safir::Dob::ResponseSenderPtr        /*responseSender*/) {}

    virtual void OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy /*injectedEntityProxy*/)
    {
    }

    virtual void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId,
                                         const Safir::Dob::Typesystem::HandlerId&)
    {

    }

    Safir::Dob::SecondaryConnection m_connection;
    boost::asio::io_service& m_ioService;
    boost::asio::deadline_timer m_timer;
    std::vector<Safir::Dob::Typesystem::InstanceId> m_instances;

    const Safir::Dob::Typesystem::HandlerId m_handler;
};

int main()
{
    try
    {
        const std::wstring nameCommonPart = L"C++";
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
        EntityOwner owner(ioService);
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
