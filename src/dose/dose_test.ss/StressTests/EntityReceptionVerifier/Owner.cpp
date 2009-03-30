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

#include "Owner.h"
#include "CommandLine.h"
#include <Safir/Dob/OverflowException.h>
#include <DoseStressTest/EntityWithAck.h>
#include <DoseStressTest/EntityWithoutAck.h>
#include <DoseStressTest/EntityWithAckLarge.h>
#include <DoseStressTest/EntityWithoutAckLarge.h>
#include <ace/OS_NS_unistd.h>
Owner::Owner():
    m_setStat(StatisticsCollection::Instance().AddHzCollector(L"Set Entity")),
    m_currentInstance(0)
{
    m_connection.Attach();

    m_entity = DoseStressTest::EntityWithAck::Create();


    m_entity->SequenceNumber().SetVal(-1);
    if (CommandLine::Instance().AttachPayload())
    {
        m_entity->Payload().SetPtr(DoseStressTest::RootEntity::DefaultPayload());
    }

    std::wcout << "Using an entity of size " << m_entity->CalculateBlobSize() << " bytes" << std::endl;
    std::wcout << "Using instances 0 .. " << CommandLine::Instance().NumInstances() -1 << std::endl;

    m_connection.RegisterEntityHandler(m_entity->GetTypeId(),
                                       Safir::Dob::Typesystem::HandlerId(),
                                       Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                       this);
}

void Owner::Set()
{
    m_entity->SequenceNumber().SetVal(m_entity->SequenceNumber().GetVal() + 1);

    if (m_entity->SequenceNumber().GetVal() == CommandLine::Instance().NumSets())
    {
        std::wcout << "Have performed " << m_entity->SequenceNumber().GetVal() << " sets, sleeping 10s and exiting" << std::endl;
        ACE_OS::sleep(10);
        exit(0);
    }

    ++m_currentInstance;
    if (m_currentInstance >= CommandLine::Instance().NumInstances())
    {
        m_currentInstance = 0;
    }

    m_connection.SetAll(m_entity,
                        Safir::Dob::Typesystem::InstanceId(m_currentInstance),
                        Safir::Dob::Typesystem::HandlerId());
    m_setStat->Tick();
}


