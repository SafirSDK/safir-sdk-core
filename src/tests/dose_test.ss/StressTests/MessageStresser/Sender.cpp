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

#include "Sender.h"
#include "CommandLine.h"
#include <Safir/Dob/OverflowException.h>

#include <DoseStressTest/MessageWithAck.h>
#include <DoseStressTest/MessageWithoutAck.h>
#include <DoseStressTest/MessageWithAckLarge.h>
#include <DoseStressTest/MessageWithoutAckLarge.h>

Sender::Sender():
m_sendStat(StatisticsCollection::Instance().AddHzCollector(L"Send Message")),
    m_overflowStat(StatisticsCollection::Instance().AddPercentageCollector(L"Overflow", m_sendStat))
{
}

void Sender::Start()
{
    m_connection.Attach();

    if (CommandLine::Instance().Ack())
    {
        if (CommandLine::Instance().Large())
        {
            DoseStressTest::MessageWithAckLargePtr largeMsg = DoseStressTest::MessageWithAckLarge::Create();
            for (Safir::Dob::Typesystem::Int32ContainerArray::iterator it = largeMsg->Dummy().begin();
                it != largeMsg->Dummy().end(); ++it)
            {
                it->SetVal(static_cast<Safir::Dob::Typesystem::Int32>(std::distance(largeMsg->Dummy().begin(), it)));
            }
            m_message = largeMsg;
        }
        else
        {
            m_message = DoseStressTest::MessageWithAck::Create();
        }
    }
    else
    {
        if (CommandLine::Instance().Large())
        {
            DoseStressTest::MessageWithoutAckLargePtr largeMsg = DoseStressTest::MessageWithoutAckLarge::Create();
            for (Safir::Dob::Typesystem::Int32ContainerArray::iterator it = largeMsg->Dummy().begin();
                it != largeMsg->Dummy().end(); ++it)
            {
                it->SetVal(static_cast<Safir::Dob::Typesystem::Int32>(std::distance(largeMsg->Dummy().begin(), it)));
            }
            m_message = largeMsg;
        }
        else
        {
            m_message = DoseStressTest::MessageWithoutAck::Create();
        }
    }

    m_message->SequenceNumber().SetVal(-1);

    std::wcout << "Using a message of size " << CalculateBlobSize(m_message) << " bytes" << std::endl;

}

void Sender::SendSome()
{
    try
    {
        for(;;)
        {
            ++m_message->SequenceNumber();
            m_connection.Send(m_message,Safir::Dob::Typesystem::ChannelId(),this);
            m_sendStat->Tick();
        }
    }
    catch (const Safir::Dob::OverflowException &)
    {
        --m_message->SequenceNumber();
        m_overflowStat->Tick();
    }
}

void Sender::OnNotMessageOverflow()
{
    SendSome();
}
