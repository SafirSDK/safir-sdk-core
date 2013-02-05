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

#include "Sender.h"
#include "CommandLine.h"
#include <Safir/Dob/OverflowException.h>

#include <DoseStressTest/MessageWithAck.h>
#include <DoseStressTest/MessageWithoutAck.h>
#include <DoseStressTest/MessageWithAckLarge.h>
#include <DoseStressTest/MessageWithoutAckLarge.h>

Sender::Sender():
    m_overflowLatencyStat(StatisticsCollection::Instance().AddLatencyCollector(L"OnNotOverflow latency")),
    m_overflow(false)
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

    std::wcout << "Using a message of size " << m_message->CalculateBlobSize() << " bytes" << std::endl;
}

void Sender::SendSome()
{
    if (m_overflow)
    {
        return;
    }

    try
    {
        for(;;)
        {
            m_message->SequenceNumber().SetVal(m_message->SequenceNumber().GetVal() + 1);
            m_connection.Send(m_message,Safir::Dob::Typesystem::ChannelId(), this);
        }
    }
    catch (const Safir::Dob::OverflowException &)
    {
        m_overflowLatencyStat->Begin();
        m_message->SequenceNumber().SetVal(m_message->SequenceNumber().GetVal() - 1);
        m_overflow = true;
    }
}

void Sender::OnNotMessageOverflow()
{
    m_overflowLatencyStat->End();
    m_overflow = false;
}
