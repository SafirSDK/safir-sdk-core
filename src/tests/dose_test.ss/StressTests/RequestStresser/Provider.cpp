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

#include "Provider.h"
#include <DoseStressTest/Service.h>
#include <DoseStressTest/ServiceLarge.h>
#include <Safir/Dob/SuccessResponse.h>
#include <iostream>

Provider::Provider():
    m_receivedStat(StatisticsCollection::Instance().AddHzCollector(L"Received With Ack")),
    m_receivedLargeStat(StatisticsCollection::Instance().AddHzCollector(L"Received With Ack Large")),
    m_lastSequenceNumber(-1),
    m_lastSequenceNumberLarge(-1)
{
    m_connection.Attach();

    namespace sdt = Safir::Dob::Typesystem;
    m_connection.RegisterServiceHandler(DoseStressTest::Service::ClassTypeId,
                                        Safir::Dob::Typesystem::HandlerId(), this);
    m_connection.RegisterServiceHandler(DoseStressTest::ServiceLarge::ClassTypeId,
                                        Safir::Dob::Typesystem::HandlerId(), this);
}

void Provider::OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                Safir::Dob::ResponseSenderPtr         responseSender)
{
    DoseStressTest::ServicePtr svc =
        boost::static_pointer_cast<DoseStressTest::Service>(serviceRequestProxy.GetRequest());

    switch (svc->GetTypeId())
    {
    case DoseStressTest::ServiceLarge::ClassTypeId:
        {
            m_receivedLargeStat->Tick();

            if (svc->SequenceNumber().GetVal() != m_lastSequenceNumberLarge + 1)
            {
                std::wcout << "Missed at least one request!" << std::endl;
            }
            m_lastSequenceNumberLarge = svc->SequenceNumber().GetVal();
        }
        break;

    case DoseStressTest::Service::ClassTypeId:
        {
            m_receivedStat->Tick();

            if (svc->SequenceNumber().GetVal() != m_lastSequenceNumber + 1)
            {
                std::wcout << "Missed at least one request!" << std::endl;
            }
            m_lastSequenceNumber = svc->SequenceNumber().GetVal();
        }
        break;

    }

    responseSender->Send(Safir::Dob::SuccessResponse::Create());
}
