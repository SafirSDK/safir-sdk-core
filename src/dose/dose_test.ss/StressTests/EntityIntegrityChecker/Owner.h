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

#ifndef __SENDER_H__
#define __SENDER_H__

#include <Safir/Dob/Connection.h>

#include <DoseIntegrityCheck/RootEntity.h>

class Owner :
    public Safir::Dob::EntityHandler,
    private boost::noncopyable
{
public:
    Owner();

    void Set();

private:

    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     ,
                                       const Safir::Dob::Typesystem::HandlerId& ) {}

    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy ,
                                 Safir::Dob::ResponseSenderPtr        ) {}

    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy ,
                                 Safir::Dob::ResponseSenderPtr        ) {}

    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy ,
                                 Safir::Dob::ResponseSenderPtr        ) {}

    Safir::Dob::SecondaryConnection m_connection;


    DoseIntegrityCheck::RootEntityPtr m_entity;

    const std::wstring & GenerateString(const int instance) const;

    const Safir::Dob::Typesystem::TypeId m_typeId;
    const int m_numInstances;
    const int m_instancesPerNode;
    const int m_myFirst;
    Safir::Dob::Typesystem::Int64 m_numUpdates;

    int m_nextInstance;

    const Safir::Dob::Typesystem::HandlerId m_handler;
};

#endif

