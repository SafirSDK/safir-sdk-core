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
#include <Safir/Dob/NodeInfo.h>
#include <DoseTest/GlobalEntity.h>
#include <boost/lexical_cast.hpp>
#include <iostream>
#include <boost/exception/diagnostic_information.hpp>
#include <thread>

class EntityOwner
    : public Safir::Dob::EntityHandler
{
public:
    explicit EntityOwner()
    {
        m_connection.Attach();
        m_connection.RegisterEntityHandler(DoseTest::GlobalEntity::ClassTypeId,
                                           Safir::Dob::Typesystem::HandlerId(),
                                           Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                           this);
    }

    void CreateNewUntilFull()
    {
        try
        {
            for (;; ++m_numCreated)
            {
                auto ent = DoseTest::GlobalEntity::Create();
                ent->Info() = boost::lexical_cast<std::wstring>(m_numCreated);
                ent->MoreInfo() = L"Foobar a medium sized string";
                m_connection.SetAll(ent,
                                    Safir::Dob::Typesystem::InstanceId(m_numCreated),
                                    Safir::Dob::Typesystem::HandlerId());
                std::wcout << "Created entity instance " << m_numCreated<< std::endl;
            }
        }
        catch (const Safir::Dob::LowMemoryException&)
        {
            std::wcout << "Caught LowMemoryException" << std::endl;
            --m_numCreated;
            return;
        }
    }

    void CreateNew(const bool expectFailure)
    {
        try
        {
            auto ent = DoseTest::GlobalEntity::Create();
            ent->Info() = boost::lexical_cast<std::wstring>(m_numCreated);
            ent->MoreInfo() = L"Foobar a medium sized string";
            m_connection.SetAll(ent,
                                Safir::Dob::Typesystem::InstanceId(m_numCreated),
                                Safir::Dob::Typesystem::HandlerId());
            std::wcout << "Created entity instance " << m_numCreated<< std::endl;
            ++m_numCreated;

            if (expectFailure)
            {
                throw std::logic_error("Expected failure");
            }
        }
        catch (const Safir::Dob::LowMemoryException&)
        {
            std::wcout << "Caught LowMemoryException " << std::boolalpha << expectFailure << std::endl;
            if (!expectFailure)
            {
                throw;
            }
            return;
        }
    }

    void UpdateExisting(const int instance, const bool expectFailure)
    {
        try
        {
            if (instance >= m_numCreated)
            {
                throw std::logic_error("incorrect instance");
            }

            auto ent = DoseTest::GlobalEntity::Create();
            ent->Info() = boost::lexical_cast<std::wstring>(instance);
            ent->MoreInfo() = L"Foobar a laaaarge string aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
            m_connection.SetAll(ent,
                                Safir::Dob::Typesystem::InstanceId(instance),
                                Safir::Dob::Typesystem::HandlerId());
            std::wcout << "Updated entity instance " << instance << std::endl;

            if (expectFailure)
            {
                throw std::logic_error("Expected failure");
            }

        }
        catch (const Safir::Dob::LowMemoryException&)
        {
            std::wcout << "Caught LowMemoryException " << std::boolalpha << expectFailure << std::endl;
            if (!expectFailure)
            {
                throw;
            }
            return;
        }
    }


    void DeleteLast(bool expectFailure)
    {
        try
        {
            m_connection.Delete(Safir::Dob::Typesystem::EntityId(DoseTest::GlobalEntity::ClassTypeId,
                                                                 Safir::Dob::Typesystem::InstanceId(m_numCreated - 1)),
                                Safir::Dob::Typesystem::HandlerId());
            --m_numCreated;
            std::wcout << "Deleted entity instance " << m_numCreated<< std::endl;

            if (expectFailure)
            {
                throw std::logic_error("Expected failure");
            }
        }
        catch (const Safir::Dob::LowMemoryException&)
        {
            std::wcout << "Caught LowMemoryException " << std::boolalpha << expectFailure << std::endl;
            if (!expectFailure)
            {
                throw;
            }
            return;
        }
    }

    void Register(bool expectFailure)
    {
        try
        {
            m_connection.RegisterEntityHandler(DoseTest::GlobalEntity::ClassTypeId,
                                               Safir::Dob::Typesystem::HandlerId(L"Blahonga"),
                                               Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                               this);
            if (expectFailure)
            {
                throw std::logic_error("Expected failure");
            }
        }
        catch (const Safir::Dob::LowMemoryException&)
        {
            std::wcout << "Caught LowMemoryException " << std::boolalpha << expectFailure << std::endl;
            if (!expectFailure)
            {
                throw;
            }
            return;
        }
    }

    void Read(bool expectFailure)
    {
        try
        {
            m_connection.Read(Safir::Dob::Typesystem::EntityId(DoseTest::GlobalEntity::ClassTypeId,
                                                               Safir::Dob::Typesystem::InstanceId(0)));
            std::wcout << "Read entity instance " << 0 << std::endl;

            if (expectFailure)
            {
                throw std::logic_error("Expected failure");
            }
        }
        catch (const Safir::Dob::LowMemoryException&)
        {
            std::wcout << "Caught LowMemoryException " << std::boolalpha << expectFailure << std::endl;
            if (!expectFailure)
            {
                throw;
            }
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

    Safir::Dob::SecondaryConnection m_connection;
    int m_numCreated = 0;
};


class App
    : public Safir::Dob::StopHandler
    , public Safir::Dob::Internal::SharedMemoryObject
{
public:
    App()
        : m_ioContext()
        , m_work (boost::asio::make_work_guard(m_ioContext))
        , m_timer(m_ioContext)
        , m_misc(m_connection)
        , m_dispatcher(m_connection, m_ioContext)
    {
        m_shmem.reserve(10000);
    }

    void AllocateUntilLevel(const Safir::Dob::MemoryLevel::Enumeration expectedLevel)
    {
        while (GetMemoryLevel() < expectedLevel) //Not the fuzzy variant
        {
            m_shmem.push_back(ShmString(32*1024, 'a'));
        }
    }

    void CheckLevel(const Safir::Dob::MemoryLevel::Enumeration expectedLevel)
    {
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

        //Read the level from the ProcessInfo entity until it is right
        for(int i = 0;i < 12; ++i) // wait for max 12 seconds. The NodeInfo entity gets updated every 10 seconds
        {
            const auto nodeInfo = m_connection.Read(Safir::Dob::Typesystem::EntityId(Safir::Dob::NodeInfo::ClassTypeId,
                                                                                     Safir::Dob::Typesystem::InstanceId(m_misc.GetNodeId())));
            const auto level = std::dynamic_pointer_cast<Safir::Dob::NodeInfo>(nodeInfo.GetEntity())->MemoryLevel().GetVal();
            if (level == expectedLevel)
            {
                return;
            }
            std::this_thread::sleep_for(std::chrono::seconds(1));
        }

        throw std::logic_error("Did not get updated memory level in NodeInfo object");
    }

    void WaitForLevel(const Safir::Dob::MemoryLevel::Enumeration expectedLevel)
    {
        while(m_misc.GetSharedMemoryLevel() != expectedLevel)
        {
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
        }
    }

    void CreateUntilWarning()
    {
        for (;;)
        {
            if (m_misc.GetSharedMemoryLevel() >= Safir::Dob::MemoryLevel::Warning)
            {
                return;
            }

            m_entityOwner->CreateNew(false);
        }
    }

    void OnStopOrder() override {m_work.reset();}

    void Run()
    {
        m_connection.Open(L"memory_eater",L"",0,this,&m_dispatcher);

        m_entityOwner = std::make_unique<EntityOwner>();
        CheckLevel(Safir::Dob::MemoryLevel::Normal);
        m_entityOwner->CreateNew(false); // instance 0
        m_entityOwner->CreateNew(false); // instance 1
        m_entityOwner->CreateNew(false); // instance 2
        m_entityOwner->CreateNew(false); // instance 3
        m_entityOwner->CreateNew(false); // instance 4
        AllocateUntilLevel(Safir::Dob::MemoryLevel::Warning);
        m_entityOwner->CreateNew(false); // instance 5
        AllocateUntilLevel(Safir::Dob::MemoryLevel::Low);
        m_entityOwner->CreateNew(true);
        m_entityOwner->UpdateExisting(0, false);
        AllocateUntilLevel(Safir::Dob::MemoryLevel::VeryLow);
        m_entityOwner->UpdateExisting(1, true);
        CheckLevel(Safir::Dob::MemoryLevel::VeryLow);
        m_entityOwner->DeleteLast(true);
        m_shmem.clear();
        AllocateUntilLevel(Safir::Dob::MemoryLevel::Low);
        m_entityOwner->Read(false);
        m_entityOwner->DeleteLast(false);
        m_entityOwner->Register(false);
        AllocateUntilLevel(Safir::Dob::MemoryLevel::VeryLow);
        m_entityOwner->Register(true);
        m_entityOwner->Read(true);
        AllocateUntilLevel(Safir::Dob::MemoryLevel::ExtremelyLow);
        CheckLevel(Safir::Dob::MemoryLevel::ExtremelyLow);
        m_shmem.clear();
        CheckLevel(Safir::Dob::MemoryLevel::Normal);

        //We create many entitites, so that dose_main has a lot of cleanup to do after
        //us. Cleanup is always done by dose_main.
        CreateUntilWarning();

        std::wcout << "Done" << std::endl;

        //Exit after 3 seconds, but let the ioservice run, just in case something is going on...
        m_timer.expires_after(std::chrono::seconds(3));
        m_timer.async_wait([this](const boost::system::error_code&){m_work.reset();});

        m_ioContext.run();

        m_connection.Close();
    }
private:
    boost::asio::io_context m_ioContext;
    boost::asio::executor_work_guard<boost::asio::io_context::executor_type> m_work;
    boost::asio::steady_timer m_timer;
    Safir::Dob::Connection m_connection;
    Safir::Dob::ConnectionAspectMisc m_misc;
    Safir::Utilities::AsioDispatcher m_dispatcher;
    std::unique_ptr<EntityOwner> m_entityOwner;
    Containers<ShmString>::vector m_shmem;
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
        std::wcout << "Caught exception: " << boost::current_exception_diagnostic_information().c_str() << std::endl;
        return 1;
    }
}
