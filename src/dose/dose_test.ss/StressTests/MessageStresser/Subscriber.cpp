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

#include "Subscriber.h"
#include <DoseStressTest/MessageWithAck.h>
#include <DoseStressTest/MessageWithoutAck.h>
#include <DoseStressTest/MessageWithAckLarge.h>
#include <DoseStressTest/MessageWithoutAckLarge.h>

#ifdef GetMessage
#undef GetMessage
#endif

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

    m_connection.SubscribeMessage(DoseStressTest::RootMessage::ClassTypeId,
                                  Safir::Dob::Typesystem::ChannelId(), this);
}

void Subscriber::OnMessage(Safir::Dob::MessageProxy messageProxy)
{
    DoseStressTest::RootMessagePtr rootMsg =
        boost::static_pointer_cast<DoseStressTest::RootMessage>(messageProxy.GetMessage());

    switch (rootMsg->GetTypeId())
    {
    case DoseStressTest::MessageWithAckLarge::ClassTypeId:
        {
            m_receivedWithAckLargeStat->Tick();

            if (rootMsg->SequenceNumber().GetVal() != m_lastSequenceNumberWithAckLarge + 1)
            {
                //tick up number of lost messages
                m_missedWithAckLargeStat->Tick(rootMsg->SequenceNumber().GetVal() - m_lastSequenceNumberWithAckLarge);
            }
            m_lastSequenceNumberWithAckLarge = rootMsg->SequenceNumber().GetVal();
        }
        break;

    case DoseStressTest::MessageWithAck::ClassTypeId:
        {
            m_receivedWithAckStat->Tick();

            if (rootMsg->SequenceNumber().GetVal() != m_lastSequenceNumberWithAck + 1)
            {
                //tick up number of lost messages
                m_missedWithAckStat->Tick(rootMsg->SequenceNumber().GetVal() - m_lastSequenceNumberWithAck);
            }
            m_lastSequenceNumberWithAck = rootMsg->SequenceNumber().GetVal();
        }
        break;

    case DoseStressTest::MessageWithoutAckLarge::ClassTypeId:
        {
            m_receivedWithoutAckLargeStat->Tick();

            if (rootMsg->SequenceNumber().GetVal() != m_lastSequenceNumberWithoutAckLarge + 1)
            {
                //tick up number of lost messages
                m_missedWithoutAckLargeStat->Tick(rootMsg->SequenceNumber().GetVal() - m_lastSequenceNumberWithoutAckLarge);
            }
            m_lastSequenceNumberWithoutAckLarge = rootMsg->SequenceNumber().GetVal();
        }
        break;

    case DoseStressTest::MessageWithoutAck::ClassTypeId:
        {
            m_receivedWithoutAckStat->Tick();

            if (rootMsg->SequenceNumber().GetVal() != m_lastSequenceNumberWithoutAck + 1)
            {
                //tick up number of lost messages
                m_missedWithoutAckStat->Tick(rootMsg->SequenceNumber().GetVal() - m_lastSequenceNumberWithoutAck);
            }
            m_lastSequenceNumberWithoutAck = rootMsg->SequenceNumber().GetVal();
        }
        break;

    }
}
