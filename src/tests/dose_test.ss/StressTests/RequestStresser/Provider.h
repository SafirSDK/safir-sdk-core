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

#ifndef __PROVIDER_H__
#define __PROVIDER_H__

#include <Safir/Dob/Connection.h>
#include "../common/StatisticsCollection.h"

class Provider :
    public Safir::Dob::ServiceHandler
{
public:
    Provider();
private:
    void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     /*typeId*/,
                               const Safir::Dob::Typesystem::HandlerId& /*handlerId*/) override {}

    void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                          Safir::Dob::ResponseSenderPtr         responseSender) override;


    Safir::Dob::SecondaryConnection m_connection;

    HzCollector * m_receivedStat;

    HzCollector * m_receivedLargeStat;

    Safir::Dob::Typesystem::Int32 m_lastSequenceNumber;
    Safir::Dob::Typesystem::Int32 m_lastSequenceNumberLarge;
};

#endif

