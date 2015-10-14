/******************************************************************************
*
* Copyright Saab AB, 2006-2015 (http://safirsdkcore.com)
*
* Created by: Samuel Waxin / samuel.waxin@consoden.se
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



namespace
{
    void FillBinaryMemberInternal(Safir::Dob::Typesystem::BinaryContainer & cont)
    {
        //we're only supposed to fill it if it is null
        if (cont.IsNull())
        {
            std::wcout << " - Filling a binary member!" << std::endl;
            Safir::Dob::Typesystem::Binary b;
            const size_t size = 10 * 1024 * 1024; //10 Mb of data!
            b.reserve(size);
            char val = 0;
            for (size_t i = 0; i < size ; ++i)
            {
                b.push_back(val);
                ++val;
            }
            cont.SetVal(b);
        }
    }
}

Sender::Sender(int sendCount, int timeOut, const boost::function<void(void)> & finishedCallback):
    m_sendCount(sendCount),
    m_timeOut(timeOut),
    m_noSentMessages(0),
    m_finishCallback(finishedCallback)
{
}

void Sender::Start()
{
    m_connection.Attach();

    DoseTest::ComplexGlobalMessagePtr largeMsg = DoseTest::ComplexGlobalMessage::Create();

    FillBinaryMemberInternal(largeMsg->BinaryMember().GetContainer());

    m_message = largeMsg;

    std::wcout << "Using a message of size " << CalculateBlobSize(m_message) << " bytes" << std::endl;

}

void Sender::SendSome()
{
    try
    {
        while(m_noSentMessages < m_sendCount)
        {
            m_connection.Send(m_message,Safir::Dob::Typesystem::ChannelId(),this);
            std::wcout << "Sent message " << std::endl;
            m_noSentMessages++;

            boost::this_thread::sleep_for(boost::chrono::seconds(m_timeOut));
        }

        std::wcout << "Finished sending " << m_noSentMessages << " messages" << std::endl;

        m_finishCallback();
    }
    catch (const Safir::Dob::OverflowException &)
    {
    }


}

void Sender::OnNotMessageOverflow()
{
    SendSome();
}
