/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#ifndef __SENDER_H__
#define __SENDER_H__

#include <Safir/Dob/Connection.h>
#include "../common/StatisticsCollection.h"

#include <DoseStressTest/RootEntity.h>

class Owner :
    public Safir::Dob::EntityHandlerInjection
{
public:
    Owner();

    void Set();

protected:

    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     /*typeId*/,
                                       const Safir::Dob::Typesystem::HandlerId& /*handlerId*/) {}

    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
                                 Safir::Dob::ResponseSenderPtr        /*responseSender*/) {}

    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
                                 Safir::Dob::ResponseSenderPtr        /*responseSender*/) {}

    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
                                 Safir::Dob::ResponseSenderPtr        /*responseSender*/) {}

    Safir::Dob::SecondaryConnection m_connection;

    HzCollector * m_setStat;

    DoseStressTest::RootEntityPtr m_entity;
    int m_nextInstance;
};

#endif

