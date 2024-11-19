/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / stlrha
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
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <Safir/Utilities/Internal/AsioPeriodicTimer.h>
#include <DoseTest/ComplexGlobalEntity.h>
#include <DoseTest/GlobalEntity.h>
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/hex.hpp>

const int NUM_SMALL=10000;
const int NUM_BIG=100;

//disable stupid incorrect microsoft warning.
#ifdef _MSC_VER
#pragma warning (disable : 4428)
#endif

namespace
{
    std::wstring random_string( size_t length )
    {
        auto randchar = []() -> wchar_t
        {
            const wchar_t charset[] =
                L"0123456789"
                L"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                L"abcdefghijklmnopqrstuvwxyz";
            const size_t max_index = (sizeof(charset)/sizeof(wchar_t) - 1);
            return charset[ rand() % max_index ];
        };
        std::wstring str(length,0);
        std::generate_n( str.begin(), length, randchar );
        return str;
    }
}


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

class EntityOwner
    : public Safir::Dob::EntityHandler
{
public:
    explicit EntityOwner(boost::asio::io_service& ioService)
        : m_ioService(ioService)
    {
        m_connection.Attach();
        m_connection.RegisterEntityHandler(DoseTest::GlobalEntity::ClassTypeId,
                                           Safir::Dob::Typesystem::HandlerId(),
                                           Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                           this);
        m_connection.RegisterEntityHandler(DoseTest::ComplexGlobalEntity::ClassTypeId,
                                           Safir::Dob::Typesystem::HandlerId(),
                                           Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                           this);
    }

    void SetSmall()
    {
        for (int i = 0; i < NUM_SMALL; ++i)
        {
            auto ent = DoseTest::GlobalEntity::Create();
            ent->Info() = L"Some info";
            ent->MoreInfo() = random_string(rand()%256);
            m_connection.SetChanges(ent,
                                    Safir::Dob::Typesystem::InstanceId::GenerateRandom(),
                                    Safir::Dob::Typesystem::HandlerId());
        }
    }

    void UpdateSomeSmall()
    {
        static_assert(NUM_SMALL >= 100, "NUM_SMALL has to be more than 100");
        using namespace Safir::Dob;
        EntityIterator it = m_connection.GetEntityIterator(DoseTest::GlobalEntity::ClassTypeId,false);
        for (int i = 0; i < 100; ++i)
        {
            std::advance(it, rand() % (NUM_SMALL/100));
            auto ent = std::static_pointer_cast<DoseTest::GlobalEntity>(it->GetEntity());
            ent->MoreInfo() = random_string(rand()%256);
            m_connection.SetChanges(ent,
                                    it->GetInstanceId(),
                                    Safir::Dob::Typesystem::HandlerId());
        }
    }


    void SetBig()
    {
        for (int i = 0; i < NUM_BIG; ++i)
        {
            auto ent = DoseTest::ComplexGlobalEntity::Create();

            ent->Info() = L"Some other info";
            ent->MoreInfo() = random_string(rand()%256);

            //TODO: put some stuff in the members...

            m_connection.SetChanges(ent,
                                    Safir::Dob::Typesystem::InstanceId::GenerateRandom(),
                                    Safir::Dob::Typesystem::HandlerId());
        }
    }

    void UpdateSomeBig()
    {
        static_assert(NUM_BIG >= 10, "NUM_BIG has to be more than 10");
        using namespace Safir::Dob;
        EntityIterator it = m_connection.GetEntityIterator(DoseTest::ComplexGlobalEntity::ClassTypeId,false);
        for (int i = 0; i < 10; ++i)
        {
            std::advance(it, rand() % (NUM_BIG/10));
            auto ent = std::static_pointer_cast<DoseTest::ComplexGlobalEntity>(it->GetEntity());

            ent->MoreInfo() = random_string(rand()%256);

            //TODO update more stuff
            
            m_connection.SetChanges(ent,
                                    it->GetInstanceId(),
                                    Safir::Dob::Typesystem::HandlerId());
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
    boost::asio::io_service& m_ioService;
};

int main()
{
    try
    {
        const std::wstring nameCommonPart = L"C++";
        const std::wstring nameInstancePart = L"1";

        boost::asio::io_context ioService;

        StopHandler stopHandler(ioService);

        Safir::Dob::Connection connection;

        Safir::Utilities::AsioDispatcher dispatcher(connection,ioService);

        connection.Open(nameCommonPart,
                        nameInstancePart,
                        0, // Context
                        &stopHandler,
                        &dispatcher);

        EntityOwner owner(ioService);

        owner.SetSmall();
        owner.SetBig();

        Safir::Utilities::Internal::AsioPeriodicTimer updateSmall
            (ioService,std::chrono::seconds(1), [&owner](const boost::system::error_code& error){owner.UpdateSomeSmall();});
        updateSmall.Start();

        Safir::Utilities::Internal::AsioPeriodicTimer updateBig
            (ioService,std::chrono::seconds(1), [&owner](const boost::system::error_code& error){owner.UpdateSomeBig();});
        updateBig.Start();

        boost::asio::io_service::work keepRunning(ioService);
        ioService.run();

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
