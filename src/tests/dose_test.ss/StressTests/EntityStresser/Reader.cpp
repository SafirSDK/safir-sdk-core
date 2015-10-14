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

#include "Reader.h"
#include "CommandLine.h"
#include <DoseStressTest/EntityWithAck.h>
#include <DoseStressTest/EntityWithoutAck.h>
#include <DoseStressTest/EntityWithAckLarge.h>
#include <DoseStressTest/EntityWithoutAckLarge.h>

Reader::Reader():
    m_readWithAckStat(StatisticsCollection::Instance().AddHzCollector(L"Read With Ack")),
    m_missedWithAckStat(StatisticsCollection::Instance().AddPercentageCollector(L"Missed", m_readWithAckStat)),
    m_readWithoutAckStat(StatisticsCollection::Instance().AddHzCollector(L"Read Without Ack")),
    m_missedWithoutAckStat(StatisticsCollection::Instance().AddPercentageCollector(L"Missed",m_readWithoutAckStat)),
    m_readWithAckLargeStat(StatisticsCollection::Instance().AddHzCollector(L"Read With Ack Large")),
    m_missedWithAckLargeStat(StatisticsCollection::Instance().AddPercentageCollector(L"Missed", m_readWithAckLargeStat)),
    m_readWithoutAckLargeStat(StatisticsCollection::Instance().AddHzCollector(L"Read Without Ack Large")),
    m_missedWithoutAckLargeStat(StatisticsCollection::Instance().AddPercentageCollector(L"Missed",m_readWithoutAckLargeStat)),
    m_lastSequenceNumberWithAck(-1),
    m_lastSequenceNumberWithoutAck(-1),
    m_lastSequenceNumberWithAckLarge(-1),
    m_lastSequenceNumberWithoutAckLarge(-1)
{
    m_connection.Attach();

    namespace sdt = Safir::Dob::Typesystem;

    if (CommandLine::Instance().Ack())
    {
        if (CommandLine::Instance().Large())
        {
            m_entityId = sdt::EntityId(DoseStressTest::EntityWithAckLarge::ClassTypeId,sdt::InstanceId(0));
        }
        else
        {
            m_entityId = sdt::EntityId(DoseStressTest::EntityWithAck::ClassTypeId,sdt::InstanceId(0));
        }
    }
    else
    {
        if (CommandLine::Instance().Large())
        {
            m_entityId = sdt::EntityId(DoseStressTest::EntityWithoutAckLarge::ClassTypeId,sdt::InstanceId(0));
        }
        else
        {
            m_entityId = sdt::EntityId(DoseStressTest::EntityWithoutAck::ClassTypeId,sdt::InstanceId(0));
        }
    }
}


void Reader::Read()
{
    if (!m_connection.IsCreated(m_entityId))
    {
        return;
    }
    DoseStressTest::RootEntityPtr rootEnt =
        boost::static_pointer_cast<DoseStressTest::RootEntity>(m_connection.Read(m_entityId).GetEntity());

    switch (rootEnt->GetTypeId())
    {
    case DoseStressTest::EntityWithAckLarge::ClassTypeId:
        {
            m_readWithAckLargeStat->Tick();

            if (rootEnt->SequenceNumber().GetVal() != m_lastSequenceNumberWithAckLarge + 1)
            {
                //tick up number of lost entities
                m_missedWithAckLargeStat->Tick(rootEnt->SequenceNumber().GetVal() - m_lastSequenceNumberWithAckLarge);
            }
            m_lastSequenceNumberWithAckLarge = rootEnt->SequenceNumber().GetVal();
        }
        break;

    case DoseStressTest::EntityWithAck::ClassTypeId:
        {
            m_readWithAckStat->Tick();

            if (rootEnt->SequenceNumber().GetVal() != m_lastSequenceNumberWithAck + 1)
            {
                //tick up number of lost entities
                m_missedWithAckStat->Tick(rootEnt->SequenceNumber().GetVal() - m_lastSequenceNumberWithAck);
            }
            m_lastSequenceNumberWithAck = rootEnt->SequenceNumber().GetVal();
        }
        break;

    case DoseStressTest::EntityWithoutAckLarge::ClassTypeId:
        {
            m_readWithoutAckLargeStat->Tick();

            if (rootEnt->SequenceNumber().GetVal() != m_lastSequenceNumberWithoutAckLarge + 1)
            {
                //tick up number of lost entities
                m_missedWithoutAckLargeStat->Tick(rootEnt->SequenceNumber().GetVal() - m_lastSequenceNumberWithoutAckLarge);
            }
            m_lastSequenceNumberWithoutAckLarge = rootEnt->SequenceNumber().GetVal();
        }
        break;

    case DoseStressTest::EntityWithoutAck::ClassTypeId:
        {
            m_readWithoutAckStat->Tick();

            if (rootEnt->SequenceNumber().GetVal() != m_lastSequenceNumberWithoutAck + 1)
            {
                //tick up number of lost entities
                m_missedWithoutAckStat->Tick(rootEnt->SequenceNumber().GetVal() - m_lastSequenceNumberWithoutAck);
            }
            m_lastSequenceNumberWithoutAck = rootEnt->SequenceNumber().GetVal();
        }
        break;

    }
}
