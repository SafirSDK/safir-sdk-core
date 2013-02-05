/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

#include "Ponger.h"
#include "CommandLine.h"
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <DoseStressTest/Ping.h>
#include "../common/ErrorReporter.h"
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <sstream>


Ponger::Ponger():
    m_handler(Safir::Dob::Typesystem::InstanceId::GenerateRandom().GetRawValue())
{
    m_connection.Attach();

    m_connection.RegisterEntityHandler(DoseStressTest::Pong::ClassTypeId,
                                       m_handler,
                                       Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                       this);

    m_entity = DoseStressTest::Pong::Create();

    m_connection.SubscribeEntity(DoseStressTest::Ping::ClassTypeId,
                                 this);
}


void Ponger::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    Pong(entityProxy);
}

void Ponger::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{
    Pong(entityProxy);
}

void Ponger::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy,
                             const bool                    /*deletedByOwner*/)
{
    const Safir::Dob::Typesystem::InstanceId instance = entityProxy.GetInstanceId();
    PingPongTable::iterator findIt = m_pingPongTable.find(instance);
    if (findIt == m_pingPongTable.end())
    {
        std::wostringstream ostr;

        ostr << "Got a delete for an instance that I haven't seen before! instanceId = " << instance;

        ErrorReporter::Log(Safir::Dob::Typesystem::Utilities::ToUtf8(ostr.str()));
        std::wcout << ostr.str() << std::endl;
        return;
    }

    m_connection.Delete(Safir::Dob::Typesystem::EntityId(DoseStressTest::Pong::ClassTypeId,findIt->second),m_handler);
    m_pingPongTable.erase(findIt);
}


void Ponger::Pong(const Safir::Dob::EntityProxy& entityProxy)
{
    const Safir::Dob::Typesystem::InstanceId instance = entityProxy.GetInstanceId();
    //    std::wcout << "Pong " << instance << std::endl;

    PingPongTable::iterator findIt = m_pingPongTable.find(instance);

    //if it is not in the table it is the first time we've seen it, so add to the table
    if (findIt == m_pingPongTable.end())
    {
        findIt = m_pingPongTable.insert(std::make_pair(instance,Safir::Dob::Typesystem::InstanceId::GenerateRandom())).first;
    }

    m_entity->Number() = boost::static_pointer_cast<DoseStressTest::Ping>(entityProxy.GetEntity())->Number();
    m_entity->WhichPing() = instance;

    m_connection.SetAll(m_entity,findIt->second,m_handler);
}



void Ponger::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId /*typeId*/,
                                   const Safir::Dob::Typesystem::HandlerId& handlerId)
{
    std::wcout << "GAAH: Someone overregistered handler " << handlerId << " for DoseStressTest.Pong" << std::endl;
}
