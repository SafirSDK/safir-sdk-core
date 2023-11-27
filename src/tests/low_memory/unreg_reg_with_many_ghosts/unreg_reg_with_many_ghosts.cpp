/******************************************************************************
*
* Copyright Saab AB, 2023 (http://safirsdkcore.com)
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
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/LowMemoryException.h>
#include <DoseTest/SynchronousVolatileEntity.h>
#include <boost/lexical_cast.hpp>
#include <iostream>
#include <boost/exception/diagnostic_information.hpp>
#include <boost/thread.hpp>
#include <Safir/Utilities/ProcessInfo.h>

class EntityOwner
    : public Safir::Dob::EntityHandlerInjection
    , public Safir::Dob::Internal::SharedMemoryObject
{
public:
    explicit EntityOwner()
    {
        m_connection.Attach();

    }

    void Register()
    {
        try
        {
            m_connection.RegisterEntityHandlerInjection(DoseTest::SynchronousVolatileEntity::ClassTypeId,
                                                        Safir::Dob::Typesystem::HandlerId(),
                                                        Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                                        this);
        }
        catch (const Safir::Dob::LowMemoryException&)
        {
            std::wcout << "Caught LowMemoryException during register" << std::endl;
            std::wcout << "  " << boost::current_exception_diagnostic_information().c_str() << std::endl;
        }
    }
    void Unregister()
    {
        try
        {
            m_connection.UnregisterHandler(DoseTest::SynchronousVolatileEntity::ClassTypeId,
                                           Safir::Dob::Typesystem::HandlerId());
        }
        catch (const Safir::Dob::LowMemoryException&)
        {
            std::wcout << "Caught LowMemoryException during register" << std::endl;
            std::wcout << "  " << boost::current_exception_diagnostic_information().c_str() << std::endl;
        }
    }

    void CreateNewUntilFull()
    {
        try
        {
            for (;; ++m_numCreated)
            {
                auto ent = DoseTest::SynchronousVolatileEntity::Create();
                ent->Info() = boost::lexical_cast<std::wstring>(m_numCreated);
                ent->MoreInfo() = L"A long string: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
                m_connection.SetAll(ent,
                                    Safir::Dob::Typesystem::InstanceId(m_numCreated),
                                    Safir::Dob::Typesystem::HandlerId());
                //logs may affect how the test runs. Do not leave the logs enabled.
                //std::wcout << "Created entity instance " << m_numCreated<< std::endl;
            }
        }
        catch (const Safir::Dob::LowMemoryException&)
        {
            std::wcout << "Caught LowMemoryException" << std::endl;
            std::wcout << "  " << boost::current_exception_diagnostic_information().c_str() << std::endl;
            --m_numCreated;
            return;
        }
    }

private:
    void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId,
        const Safir::Dob::Typesystem::HandlerId&) override {}

    void OnCreateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
                         Safir::Dob::ResponseSenderPtr        /*responseSender*/) override {}

    void OnUpdateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
        Safir::Dob::ResponseSenderPtr        /*responseSender*/) override {}

    void OnDeleteRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
        Safir::Dob::ResponseSenderPtr        /*responseSender*/) override {}

    void OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy) override
    {
        (void)injectedEntityProxy;
        //logs may affect how the test runs. Do not leave the logs enabled.
        //std::wcout << "OnInjectedNewEntity " << injectedEntityProxy.GetEntityId() << std::endl;
        ++m_numCreated;
#if 0
        while (GetMemoryLevelFuzzy() >= Safir::Dob::MemoryLevel::Low)
        {
            std::wcout << "Delaying resurrection due to memory pressure" << std::endl;
            boost::this_thread::sleep_for(boost::chrono::milliseconds(100));
        }
        m_connection.Delete(injectedEntityProxy.GetEntityId(), Safir::Dob::Typesystem::HandlerId());
#endif
    }


    void OnInjectedUpdatedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy) override
    {
        std::wcout << "OnInjectedUpdatedEntity " << injectedEntityProxy.GetEntityId() << std::endl;
    }

    void OnInjectedDeletedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy) override
    {
        std::wcout << "OnInjectedDeletedEntity " << injectedEntityProxy.GetEntityId() << std::endl;
    }

    void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId,
                                 const Safir::Dob::Typesystem::HandlerId& /*handlerId*/) override
    {
        std::wcout << "OnInitialInjectionsDone " << Safir::Dob::Typesystem::Operations::GetName(typeId) << std::endl;
    }


    Safir::Dob::SecondaryConnection m_connection;
    int m_numCreated = 0;
};


class App
    : public Safir::Dob::StopHandler
    , public Safir::Dob::Dispatcher
{
public:
    App()
        : m_misc(m_connection)
    {

    }


    void CheckLevel(const Safir::Dob::MemoryLevel::Enumeration expectedLevel)
    {
        const auto level = m_misc.GetSharedMemoryLevel();
        if (level != expectedLevel)
        {
            std::wcout << "Expected memory level "
                       << Safir::Dob::MemoryLevel::ToString(expectedLevel)
                       << " but got "
                       << Safir::Dob::MemoryLevel::ToString(level)
                       << std::endl;
            throw std::logic_error("Unexpected memory level");
        }
    }

    void OnStopOrder() override
    {
        std::wcout << "Got stop order!" << std::endl;
    }

    void OnDoDispatch() override
    {
    }

    int Run()
    {
        m_connection.Open(L"unregister_many",L"",0,this,this);

        m_entityOwner = std::make_unique<EntityOwner>();

        m_entityOwner->Register();
        m_connection.Dispatch();

        m_entityOwner->CreateNewUntilFull();
        m_connection.Dispatch();

        m_entityOwner->Unregister();
        m_connection.Dispatch();

        m_entityOwner->Register();
        m_connection.Dispatch();

        m_connection.Close();

        return 0;
    }
private:
    Safir::Dob::Connection m_connection;
    Safir::Dob::ConnectionAspectMisc m_misc;
    std::unique_ptr<EntityOwner> m_entityOwner;
};


int main()
{
    std::wcout << "Pid: " << Safir::Utilities::ProcessInfo::GetPid() << std::endl;
    try
    {
        App app;
        return app.Run();
    }
    catch (...)
    {
        std::wcout << "Caught exception: " << boost::current_exception_diagnostic_information().c_str() << std::endl;
        return 1;
    }
    std::wcout << "Done" << std::endl;
}
