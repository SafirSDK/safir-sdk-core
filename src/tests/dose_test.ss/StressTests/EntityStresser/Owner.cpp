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

#include "Owner.h"
#include "CommandLine.h"
#include <Safir/Dob/OverflowException.h>

#include <DoseStressTest/EntityWithAck.h>
#include <DoseStressTest/EntityWithoutAck.h>
#include <DoseStressTest/EntityWithAckLarge.h>
#include <DoseStressTest/EntityWithoutAckLarge.h>
#include <boost/lexical_cast.hpp>

Owner::Owner():
    m_setStat(StatisticsCollection::Instance().AddHzCollector(L"Set Entity")),
    m_nextInstance(0)
{
    m_connection.Attach();

    if (CommandLine::Instance().Ack())
    {
        if (CommandLine::Instance().Large())
        {
            DoseStressTest::EntityWithAckLargePtr largeEnt = DoseStressTest::EntityWithAckLarge::Create();
            for (Safir::Dob::Typesystem::Int32ContainerArray::iterator it = largeEnt->Dummy().begin();
                it != largeEnt->Dummy().end(); ++it)
            {
                it->SetVal(static_cast<Safir::Dob::Typesystem::Int32>(std::distance(largeEnt->Dummy().begin(), it)));
            }
            m_entity = largeEnt;
        }
        else
        {
            m_entity = DoseStressTest::EntityWithAck::Create();
        }
    }
    else
    {
        if (CommandLine::Instance().Large())
        {
            DoseStressTest::EntityWithoutAckLargePtr largeEnt = DoseStressTest::EntityWithoutAckLarge::Create();
            for (Safir::Dob::Typesystem::Int32ContainerArray::iterator it = largeEnt->Dummy().begin();
                it != largeEnt->Dummy().end(); ++it)
            {
                it->SetVal(static_cast<Safir::Dob::Typesystem::Int32>(std::distance(largeEnt->Dummy().begin(), it)));
            }
            m_entity = largeEnt;
        }
        else
        {
            m_entity = DoseStressTest::EntityWithoutAck::Create();
        }
    }

    m_entity->SequenceNumber().SetVal(-1);
    if (CommandLine::Instance().AttachPayload())
    {
        m_entity->Payload().SetPtr(DoseStressTest::RootEntity::DefaultPayload());
    }

    std::wcout << "Using an entity of size " << CalculateBlobSize(m_entity) << " bytes" << std::endl;
    if (CommandLine::Instance().NumInstances() != 0)
    {
        std::wcout << "Using instances '0' .. '" << CommandLine::Instance().NumInstances() -1 << "'." << std::endl;
    }
    else
    {
        std::wcout << "Using instance '0'" << std::endl;
    }
    m_connection.RegisterEntityHandlerInjection(m_entity->GetTypeId(),Safir::Dob::Typesystem::HandlerId(), Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,this);
}

void Owner::Set()
{
    m_entity->SequenceNumber().SetVal(m_entity->SequenceNumber().GetVal() + 1);

    ++m_nextInstance;
    if (m_nextInstance >= CommandLine::Instance().NumInstances())
    {
        m_nextInstance = 0;
    }

    m_connection.SetAll(m_entity,
                        Safir::Dob::Typesystem::InstanceId(boost::lexical_cast<std::wstring>(m_nextInstance)),
                        Safir::Dob::Typesystem::HandlerId());
    m_setStat->Tick();
    /*
    try
    {
        for(;;)
        {
            m_entity->SequenceNumber().SetVal(m_entity->SequenceNumber().GetVal() + 1);
            m_connection.Send(m_entity,this);
            m_sendStat->Tick();
        }
    }
    catch (const Safir::Dob::OverflowException &)
    {
        m_entity->SequenceNumber().SetVal(m_entity->SequenceNumber().GetVal() - 1);
        m_overflowStat->Tick();
    }*/
}


