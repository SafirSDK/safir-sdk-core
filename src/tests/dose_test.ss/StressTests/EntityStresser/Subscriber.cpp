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

#include "Subscriber.h"
#include "CommandLine.h"
#include <DoseStressTest/EntityWithAck.h>
#include <DoseStressTest/EntityWithoutAck.h>
#include <DoseStressTest/EntityWithAckLarge.h>
#include <DoseStressTest/EntityWithoutAckLarge.h>

Subscriber::Subscriber():
    m_receivedWithAckStat(StatisticsCollection::Instance().AddHzCollector(L"Received With Ack")),
    m_missedWithAckStat(StatisticsCollection::Instance().AddPercentageCollector(L"Missed", m_receivedWithAckStat)),
    m_receivedWithoutAckStat(StatisticsCollection::Instance().AddHzCollector(L"Received Without Ack")),
    m_missedWithoutAckStat(StatisticsCollection::Instance().AddPercentageCollector(L"Missed",m_receivedWithoutAckStat)),
    m_receivedWithAckLargeStat(StatisticsCollection::Instance().AddHzCollector(L"Received With Ack Large")),
    m_missedWithAckLargeStat(StatisticsCollection::Instance().AddPercentageCollector(L"Missed", m_receivedWithAckLargeStat)),
    m_receivedWithoutAckLargeStat(StatisticsCollection::Instance().AddHzCollector(L"Received Without Ack Large")),
    m_missedWithoutAckLargeStat(StatisticsCollection::Instance().AddPercentageCollector(L"Missed",m_receivedWithoutAckLargeStat)),
    m_lastSequenceNumberWithAck(-1),
    m_lastSequenceNumberWithoutAck(-1),
    m_lastSequenceNumberWithAckLarge(-1),
    m_lastSequenceNumberWithoutAckLarge(-1)
{
    m_connection.Attach();

    namespace sdt = Safir::Dob::Typesystem;
    m_connection.SubscribeEntity(DoseStressTest::RootEntity::ClassTypeId,
                                 this);
}

void Subscriber::HandleEntity(const Safir::Dob::EntityProxy& entityProxy)
{
    DoseStressTest::RootEntityPtr rootEnt =
        boost::static_pointer_cast<DoseStressTest::RootEntity>(entityProxy.GetEntity());

    switch (rootEnt->GetTypeId())
    {
    case DoseStressTest::EntityWithAckLarge::ClassTypeId:
        {
            m_receivedWithAckLargeStat->Tick();

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
            m_receivedWithAckStat->Tick();

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
            m_receivedWithoutAckLargeStat->Tick();

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
            m_receivedWithoutAckStat->Tick();

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
