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

#include "Requestor.h"
#include "CommandLine.h"
#include <Safir/Dob/OverflowException.h>

#include <DoseStressTest/ServiceLarge.h>

Requestor::Requestor():
    m_overflowLatencyStat(StatisticsCollection::Instance().AddLatencyCollector(L"NotOverflow latency")),
    //m_responseLatencyStat(StatisticsCollection::Instance().AddLatencyCollector(L"Response latency")),
    m_overflow(false)
{
    m_connection.Attach();

    if (CommandLine::Instance().Large())
    {
        DoseStressTest::ServiceLargePtr largeSvc = DoseStressTest::ServiceLarge::Create();
        for (Safir::Dob::Typesystem::Int32ContainerArray::iterator it = largeSvc->Dummy().begin();
            it != largeSvc->Dummy().end(); ++it)
        {
            it->SetVal(static_cast<Safir::Dob::Typesystem::Int32>(std::distance(largeSvc->Dummy().begin(), it)));
        }
        m_service = largeSvc;
    }
    else
    {
        m_service = DoseStressTest::Service::Create();
    }

    m_service->SequenceNumber().SetVal(-1);

    std::wcout << "Using a ServiceRequest of size " << m_service->CalculateBlobSize() << " bytes" << std::endl;
}

void Requestor::SendSome()
{
    if (m_overflow)
    {
        return;
    }
    try
    {
        for(;;)
        {
            m_service->SequenceNumber().SetVal(m_service->SequenceNumber().GetVal() + 1);
            m_connection.ServiceRequest(m_service,Safir::Dob::Typesystem::HandlerId(),this);
        }
    }
    catch (const Safir::Dob::OverflowException &)
    {
        m_overflow = true;
        m_service->SequenceNumber().SetVal(m_service->SequenceNumber().GetVal() - 1);
        m_overflowLatencyStat->Begin();
    }
}

void Requestor::OnResponse(const Safir::Dob::ResponseProxy /*responseProxy*/)
{

}

void Requestor::OnNotRequestOverflow()
{
    m_overflowLatencyStat->End();
    m_overflow = false;
}
