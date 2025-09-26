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
#include <vector>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <Safir/Utilities/Internal/AsioPeriodicTimer.h>
#include <DoseTest/ComplexGlobalEntity.h>
#include <DoseTest/GlobalEntity.h>
#include <DoseTest/ComplexGlobalMessage.h>
#include <DoseTest/GlobalMessage.h>
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/hex.hpp>

const int NUM_SMALL_ENTITIES=10000;
const int NUM_BIG_ENTITIES=100;

const int NUM_SMALL_MESSAGES=10;
const int NUM_BIG_MESSAGES=1;

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
    explicit StopHandler(boost::asio::io_context& ioContext)
        : m_ioContext(ioContext) {}
    void OnStopOrder() override {m_ioContext.stop();}
private:
    boost::asio::io_context& m_ioContext;
};

class EntityOwner
    : public Safir::Dob::EntityHandler
    , public Safir::Dob::MessageSender
{
public:
    explicit EntityOwner()
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
        for (int i = 0; i < NUM_SMALL_ENTITIES; ++i)
        {
            m_smallEntityIds.push_back
                (Safir::Dob::Typesystem::EntityId(DoseTest::GlobalEntity::ClassTypeId,
                                                  Safir::Dob::Typesystem::InstanceId::GenerateRandom()));
            auto ent = DoseTest::GlobalEntity::Create();
            ent->Info() = L"Some info";
            ent->MoreInfo() = random_string(rand()%256);
            m_connection.SetChanges(ent,
                                    m_smallEntityIds.back().GetInstanceId(),
                                    Safir::Dob::Typesystem::HandlerId());
        }
    }

    void UpdateSomeSmall()
    {
        static_assert(NUM_SMALL_ENTITIES >= 100, "NUM_SMALL_ENTITIES has to be more than 100");
        using namespace Safir::Dob;
        for (int i = 0; i < 100; ++i)
        {
            const auto randomEid = m_smallEntityIds.at(rand() % m_smallEntityIds.size());
            auto ent = std::static_pointer_cast<DoseTest::GlobalEntity>(m_connection.Read(randomEid).GetEntity());
            ent->MoreInfo() = random_string(rand()%256);
            m_connection.SetChanges(ent,
                                    randomEid.GetInstanceId(),
                                    Safir::Dob::Typesystem::HandlerId());
        }
    }


    void SetBig()
    {
        for (int i = 0; i < NUM_BIG_ENTITIES; ++i)
        {
            m_bigEntityIds.push_back
                (Safir::Dob::Typesystem::EntityId(DoseTest::ComplexGlobalEntity::ClassTypeId,
                                                  Safir::Dob::Typesystem::InstanceId::GenerateRandom()));
            auto ent = DoseTest::ComplexGlobalEntity::Create();

            ent->Info() = L"Some other info";
            ent->MoreInfo() = random_string(rand()%256);

            //IMPROVEMENT: put some stuff in the members...

            m_connection.SetChanges(ent,
                                    m_bigEntityIds.back().GetInstanceId(),
                                    Safir::Dob::Typesystem::HandlerId());
        }
    }

    void UpdateSomeBig()
    {
        static_assert(NUM_BIG_ENTITIES >= 10, "NUM_BIG_ENTITIES has to be more than 10");
        using namespace Safir::Dob;
        for (int i = 0; i < 10; ++i)
        {
            const auto randomEid = m_bigEntityIds.at(rand() % m_bigEntityIds.size());
            auto ent = std::static_pointer_cast<DoseTest::ComplexGlobalEntity>(m_connection.Read(randomEid).GetEntity());

            ent->MoreInfo() = random_string(rand()%256);
            ent->EntityIdMember() = m_bigEntityIds.at(rand() % m_bigEntityIds.size());

            //IMPROVEMENT update more stuff

            m_connection.SetChanges(ent,
                                    randomEid.GetInstanceId(),
                                    Safir::Dob::Typesystem::HandlerId());
        }
    }

    void SendSmall()
    {
        try
        {
            for (int i = 0; i < NUM_SMALL_MESSAGES; ++i)
            {
                auto msg = DoseTest::GlobalMessage::Create();
                msg->Info() = random_string(rand()%256);
                m_connection.Send(msg,
                                  Safir::Dob::Typesystem::ChannelId(),
                                  this);
            }
        }
        catch(const Safir::Dob::OverflowException&)
        {

        }
    }

    void SendBig()
    {
        try
        {
            for (int i = 0; i < NUM_BIG_MESSAGES; ++i)
            {
                auto msg = DoseTest::ComplexGlobalMessage::Create();

                msg->Info() = random_string(rand()%256);

                //IMPROVEMENT: put some stuff in the members...

                m_connection.Send(msg,
                                  Safir::Dob::Typesystem::ChannelId(),
                                  this);
            }
        }
        catch(const Safir::Dob::OverflowException&)
        {

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

    void OnNotMessageOverflow() override {}

    Safir::Dob::SecondaryConnection m_connection;

    std::vector<Safir::Dob::Typesystem::EntityId> m_smallEntityIds;
    std::vector<Safir::Dob::Typesystem::EntityId> m_bigEntityIds;
};

int main()
{
    try
    {
        const std::wstring nameCommonPart = L"C++";
        const std::wstring nameInstancePart = L"1";

        boost::asio::io_context ioContext;

        StopHandler stopHandler(ioContext);

        Safir::Dob::Connection connection;

        Safir::Utilities::AsioDispatcher dispatcher(connection,ioContext);

        connection.Open(nameCommonPart,
                        nameInstancePart,
                        0, // Context
                        &stopHandler,
                        &dispatcher);

        EntityOwner owner;

        owner.SetSmall();
        owner.SetBig();

        Safir::Utilities::Internal::AsioPeriodicTimer updateSmall
            (ioContext,std::chrono::seconds(1), [&owner](const boost::system::error_code& /*error*/){owner.UpdateSomeSmall();});
        updateSmall.Start();

        Safir::Utilities::Internal::AsioPeriodicTimer updateBig
            (ioContext,std::chrono::seconds(1), [&owner](const boost::system::error_code& /*error*/){owner.UpdateSomeBig();});
        updateBig.Start();

        Safir::Utilities::Internal::AsioPeriodicTimer sendSmall
            (ioContext,std::chrono::milliseconds(10), [&owner](const boost::system::error_code& /*error*/){owner.SendSmall();});
        sendSmall.Start();

        Safir::Utilities::Internal::AsioPeriodicTimer sendBig
            (ioContext,std::chrono::milliseconds(100), [&owner](const boost::system::error_code& /*error*/){owner.SendBig();});
        sendBig.Start();

        auto keepRunning = boost::asio::make_work_guard(ioContext);
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
