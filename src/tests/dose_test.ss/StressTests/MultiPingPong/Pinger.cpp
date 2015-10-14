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

#include "Pinger.h"
#include "CommandLine.h"
#include <Safir/Dob/OverflowException.h>
#include "../common/ErrorReporter.h"
#include <DoseStressTest/Pong.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <sstream>

void GeneratePayload(const int size, std::wstring& payload)
{
    payload.clear();
    payload.reserve(size);
    for (int i = 0; i < size; ++i)
    {
        payload.push_back(L'a');
    }
}

Pinger::Pinger():
    m_handler(Safir::Dob::Typesystem::InstanceId::GenerateRandom().GetRawValue())
{
    m_connection.Attach();

    m_connection.RegisterEntityHandler(DoseStressTest::Ping::ClassTypeId,
                                       m_handler,
                                       Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                       this);

    m_entity = DoseStressTest::Ping::Create();

    for (int i = 0; i < CommandLine::Instance().NumInstances(); ++i)
    {
        const Safir::Dob::Typesystem::InstanceId inst = Safir::Dob::Typesystem::InstanceId::GenerateRandom();
        m_pingPongTable.insert(std::make_pair(inst,PingPongData()));
        Ping(inst);
    }

    m_connection.SubscribeEntity(DoseStressTest::Pong::ClassTypeId,
                                 this);

    m_connection.SubscribeRegistration(DoseStressTest::Pong::ClassTypeId,
                                       Safir::Dob::Typesystem::HandlerId::ALL_HANDLERS,
                                       false,
                                       false,
                                       this);

}

void Pinger::Ping(const Safir::Dob::Typesystem::InstanceId& instance)
{
    //    std::wcout<< "Ping "<< instance << std::endl;
    PingPongTable::iterator findIt = m_pingPongTable.find(instance);
    assert(findIt != m_pingPongTable.end());
    m_entity->Number() = ++findIt->second.number;
    findIt->second.pongers.clear();

    if (CommandLine::Instance().Payload())
    {
        static std::wstring pl;
        GeneratePayload(rand() % DoseStressTest::Ping::PayloadMaxStringLength(),pl);
        m_entity->Payload() = pl;
    }

    findIt->second.pingTime = boost::posix_time::second_clock::local_time();
    m_connection.SetAll(m_entity,instance,m_handler);
}


void Pinger::HandlePong(const Safir::Dob::EntityProxy& entityProxy)
{
    DoseStressTest::PongPtr pong = boost::static_pointer_cast<DoseStressTest::Pong>(entityProxy.GetEntity());
    PingPongTable::iterator findIt = m_pingPongTable.find(pong->WhichPing().GetVal());
    if (findIt != m_pingPongTable.end())
    {
        if (pong->Number() == findIt->second.number)
        {
            const bool alreadyPonged = !(findIt->second.pongers.insert(entityProxy.GetOwner()).second);

            if (alreadyPonged)
            {
                std::wostringstream ostr;
                ostr << "Got a double pong for handler "
                     << entityProxy.GetOwner()
                     << " - " << entityProxy.GetOwnerConnectionInfo()->ConnectionName().GetVal();

                ErrorReporter::Log(Safir::Dob::Typesystem::Utilities::ToUtf8(ostr.str()));
                std::wcout << ostr.str() << std::endl;

            }

            if (std::includes(findIt->second.pongers.begin(),
                              findIt->second.pongers.end(),
                              m_pongers.begin(),
                              m_pongers.end()))
            {
                Ping(findIt->first);
            }
        }
        /*        else
        {
            std::wostringstream ostr;
            ostr << "Got a spurious pong for handler "
                 << entityProxy.GetOwner()
                 << " - " << entityProxy.GetOwnerConnectionInfo()->ConnectionName().GetVal() << std::endl
                 << "Expected " << findIt->second.number << " but got " << pong->Number() <<std::endl
                 << "I have received the following (expected) pongs:" << std::endl;


            for (Pongers::iterator pit = findIt->second.pongers.begin();
                 pit != findIt->second.pongers.end(); ++pit)
            {
                ostr << "  " << *pit << "  -  ";

                for (Safir::Dob::EntityIterator eit = m_connection.GetEntityIterator(DoseStressTest::Pong::ClassTypeId,false);
                     eit != Safir::Dob::EntityIterator(); ++eit)
                {
                    if (eit->GetOwner() == *pit)
                    {
                        Safir::Dob::ConnectionInfoPtr connInfo = eit->GetOwnerConnectionInfo();

                        if (connInfo->ConnectionName().IsNull())
                        {
                            ostr << "Connection Name is MISSING, node = '"
                                 << connInfo->NodeNumber().GetVal() << std::endl;
                        }
                        else
                        {
                            ostr << "Connection Name = '"
                                 << connInfo->ConnectionName().GetVal() << std::endl;
                        }
                    }
                }
            }


            ErrorReporter::Log(Safir::Dob::Typesystem::Utilities::ToUtf8(ostr.str()));
            std::wcout << ostr.str() << std::endl;

            }*/
    }
}

void Pinger::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId /*typeId*/,
                                   const Safir::Dob::Typesystem::HandlerId& handlerId)
{
    std::wostringstream ostr;
    ostr << "Someone overregistered handler "
         << handlerId
         << " for DoseStressTest.Ping";

    ErrorReporter::Log(Safir::Dob::Typesystem::Utilities::ToUtf8(ostr.str()));
    std::wcout << ostr.str() << std::endl;
}


void Pinger::OnRegistered(const Safir::Dob::Typesystem::TypeId      /*typeId*/,
                          const Safir::Dob::Typesystem::HandlerId&  handlerId)
{
    //    std::wcout << "Adding ponger " << handlerId << std::endl;
    m_pongers.insert(handlerId);
}

void Pinger::OnUnregistered(const Safir::Dob::Typesystem::TypeId      /*typeId*/,
                            const Safir::Dob::Typesystem::HandlerId&  handlerId)
{
    //   std::wcout << "Removing ponger " << handlerId << std::endl;
    m_pongers.erase(handlerId);

    for (PingPongTable::iterator it = m_pingPongTable.begin();
         it != m_pingPongTable.end(); ++it)
    {
        if (std::includes(it->second.pongers.begin(),
                          it->second.pongers.end(),
                          m_pongers.begin(),
                          m_pongers.end()))
        {
            //            std::wcout << "Have all pongs" << std::endl;
            Ping(it->first);
        }
        else
        {
            //            std::wcout << "Waiting for more pongs" << std::endl;
        }
    }
}

void Pinger::CheckForTimeouts()
{
    const boost::posix_time::ptime now = boost::posix_time::second_clock::local_time();
    for (PingPongTable::const_iterator it = m_pingPongTable.begin();
         it != m_pingPongTable.end(); ++it)
    {
        if((now - it->second.pingTime).total_seconds() > CommandLine::Instance().Timeout())
        {
            std::wostringstream ostr;
            ostr << "Ping instance " << it->first
                 << " timed out! Have not received all pongs for ping " << it->second.number << " within "
                 << CommandLine::Instance().Timeout() << " seconds!"  <<std::endl
                 << "Expecting " << m_pongers.size() << " pongs, but have " << it->second.pongers.size() << std::endl
                 << "Node name " << Safir::Dob::ThisNodeParameters::Name() << std::endl;/*
                 << "The following are missing:" << std::endl;


            Pongers missing;
            std::set_difference(m_pongers.begin(),m_pongers.end(),
                                it->second.pongers.begin(), it->second.pongers.end(),
                                std::inserter(missing, missing.end()));

            for (Pongers::iterator pit = missing.begin();
                 pit != missing.end(); ++pit)
            {
                ostr << "  " << *pit << "  -  ";

                int numFound = 0;
                for (Safir::Dob::EntityIterator eit = m_connection.GetEntityIterator(DoseStressTest::Pong::ClassTypeId,false);
                     eit != Safir::Dob::EntityIterator(); ++eit)
                {
                    if (eit->GetOwner() == *pit)
                    {
                        ++numFound;
                        Safir::Dob::ConnectionInfoPtr connInfo = eit->GetOwnerConnectionInfo();

                        if (connInfo->ConnectionName().IsNull())
                        {
                            ostr << "Connection Name is MISSING, node = '"
                                 << connInfo->NodeNumber().GetVal() << std::endl;
                        }
                        else
                        {
                            ostr << "Connection Name = '"
                                 << connInfo->ConnectionName().GetVal() << std::endl;
                        }
                    }
                }

                if (numFound == 0)
                {
                    ostr << "Handler doesnt appear to own any Pong instances!!!" << std::endl;
                }
                else if (numFound > 1)
                {
                    ostr << "Handler owns " << numFound << " Pong instances." << std::endl;
                }
            }
                                                                                                */
            ErrorReporter::Log(Safir::Dob::Typesystem::Utilities::ToUtf8(ostr.str()));
            std::wcout << ostr.str() << std::endl;
            Ping(it->first);
        }
    }
}
