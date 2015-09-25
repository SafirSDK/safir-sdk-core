/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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
#include <DopeTest/SmallEntity.h>
#include <DopeTest/BigEntity.h>
#include <boost/lexical_cast.hpp>

#ifdef NDEBUG
const int NUM_SMALL=100;
const int NUM_BIG=10;
#else
const int NUM_SMALL=10;
const int NUM_BIG=2;
#endif


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
        , m_gotAllSmall(false)
        , m_gotAllBig(false)
    {
        m_connection.Attach();
        m_connection.RegisterEntityHandlerInjection(DopeTest::SmallEntity::ClassTypeId,
                                                    Safir::Dob::Typesystem::HandlerId(),
                                                    Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                                    this);
        m_connection.RegisterEntityHandlerInjection(DopeTest::BigEntity::ClassTypeId,
                                                    Safir::Dob::Typesystem::HandlerId(),
                                                    Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                                    this);
    }

    void SetSmall()
    {
        for (int i = 0; i < NUM_SMALL; ++i)
        {
            DopeTest::SmallEntityPtr ent = DopeTest::SmallEntity::Create();
            ent->Name() = L"testelitest\n\u00e4\u203d."; //newline, ä, interrobang"
            ent->Kind() = i;
            ent->Blahonga() = 123.4f;
            ent->Show() = true;
            m_connection.SetChanges(ent,
                                    Safir::Dob::Typesystem::InstanceId(i),
                                    Safir::Dob::Typesystem::HandlerId());
        }
    }

    void UpdateSmall()
    {
        using namespace Safir::Dob;
        for (EntityIterator it = m_connection.GetEntityIterator(DopeTest::SmallEntity::ClassTypeId,false);
             it != EntityIterator(); ++it)
        {
            DopeTest::SmallEntityPtr ent = boost::static_pointer_cast<DopeTest::SmallEntity>(it->GetEntity());
            ent->Name() = L"name is changed\n\u00e4\u203d."; //newline, ä, interrobang
            m_connection.SetChanges(ent,
                                    it->GetInstanceId(),
                                    Safir::Dob::Typesystem::HandlerId());
        }
    }


    void SetBig()
    {
        for (int i = 0; i < NUM_BIG; ++i)
        {
            DopeTest::BigEntityPtr ent = DopeTest::BigEntity::Create();
            for (int j = 0; j < DopeTest::BigEntity::NumberArraySize(); ++j)
            {
                ent->Number()[j].SetVal(j);
            }
            m_connection.SetChanges(ent,
                                    Safir::Dob::Typesystem::InstanceId(i),
                                    Safir::Dob::Typesystem::HandlerId());
        }
    }

    void UpdateBig()
    {
        using namespace Safir::Dob;
        for (EntityIterator it = m_connection.GetEntityIterator(DopeTest::BigEntity::ClassTypeId,false);
             it != EntityIterator(); ++it)
        {
            DopeTest::BigEntityPtr ent = boost::static_pointer_cast<DopeTest::BigEntity>(it->GetEntity());
            ent->Number()[0].SetVal(99999999);
            m_connection.SetChanges(ent,
                                    it->GetInstanceId(),
                                    Safir::Dob::Typesystem::HandlerId());
        }
    }
private:
    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId,
        const Safir::Dob::Typesystem::HandlerId&) {}

    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender) {}

    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender) {}

    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender) {}

    virtual void OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy)
    {
        //output only the first 200 chars of the xml
        std::wcout << "OnInjectedNewEntity "
                   << injectedEntityProxy.GetEntityId() << ": "
                   << Safir::Dob::Typesystem::Serialization::ToXml(injectedEntityProxy.GetInjectionBlob()).substr(0,200)
                   << std::endl;

        const auto small = boost::dynamic_pointer_cast<DopeTest::SmallEntity>(injectedEntityProxy.GetInjection());
        if (small != nullptr)
        {
            if (small->Name() == L"testelitest\n\u00e4\u203d." || small->Name() == L"name is changed\n\u00e4\u203d.")
            {
                std::wcout << "Correct string!" << std::endl;
            }
            else
            {
                std::wcout << "Incorrect string!" << std::endl;
            }
        }
    }

    virtual void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId,
                                         const Safir::Dob::Typesystem::HandlerId&)
    {
        if (typeId == DopeTest::SmallEntity::ClassTypeId)
        {
            m_gotAllSmall = true;
        }
        if (typeId == DopeTest::BigEntity::ClassTypeId)
        {
            m_gotAllBig = true;
        }

        if (m_gotAllBig && m_gotAllSmall)
        {
            m_ioService.stop();
        }
    }


    Safir::Dob::SecondaryConnection m_connection;
    boost::asio::io_service& m_ioService;
    bool m_gotAllSmall;
    bool m_gotAllBig;
};

int main(int argc, char* argv[])
{

    std::vector<std::string> args(argv+1, argv+argc);
    if (args.size() != 1 || (args[0] != "set" && args[0] != "accept" && args[0] != "update" && args[0] != "num"))
    {
        std::wcout << "Arg must be either set, accept or update" << std::endl;
        return 1;
    }

    if (args[0] == "num")
    {
        std::wcout << "NUM_SMALL = " << NUM_SMALL << std::endl;
        std::wcout << "NUM_BIG = " << NUM_BIG << std::endl;
        return 0;
    }

    const bool set = args[0] == "set";
    const bool update = args[0] == "update";

    try
    {
        const std::wstring nameCommonPart = L"C++";
        const std::wstring nameInstancePart = L"1";

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
        if (set)
        {
            owner.SetSmall();
            owner.SetBig();
        }
        else
        {
            boost::asio::io_service::work keepRunning(ioService);
            ioService.run();

            if (update)
            {
                owner.UpdateSmall();
                owner.UpdateBig();
            }
        }

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
