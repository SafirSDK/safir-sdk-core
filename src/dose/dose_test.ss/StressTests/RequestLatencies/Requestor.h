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

#ifndef __REQUESTOR_H__
#define __REQUESTOR_H__

#include <Safir/Dob/Connection.h>
#include "../common/StatisticsCollection.h"

#include <DoseStressTest/Service.h>

class Requestor :
    public Safir::Dob::Requestor
{
public:
    Requestor();

    void SendSome();

protected:

    virtual void OnResponse(const Safir::Dob::ResponseProxy responseProxy);
    virtual void OnNotRequestOverflow();

    Safir::Dob::SecondaryConnection m_connection;

    LatencyCollector * m_overflowLatencyStat;
    LatencyCollector * m_responseLatencyStat;

    DoseStressTest::ServicePtr m_service;
    bool m_overflow;
};

#endif

