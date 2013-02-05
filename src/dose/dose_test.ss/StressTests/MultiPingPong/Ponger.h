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

#ifndef __PONGER_H__
#define __PONGER_H__

#include <Safir/Dob/Connection.h>

#include <DoseStressTest/Pong.h>
#include <map>

class Ponger :
    public Safir::Dob::EntityHandler,
    public Safir::Dob::EntitySubscriber,
    //    public Safir::Dob::RegistrationSubscriber,
    private boost::noncopyable
{
public:
    Ponger();

    //    void Timeout();

private:

    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId,
                                       const Safir::Dob::Typesystem::HandlerId& handlerId);

    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy ,
                                 Safir::Dob::ResponseSenderPtr        ) {}

    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy ,
                                 Safir::Dob::ResponseSenderPtr        ) {}

    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy ,
                                 Safir::Dob::ResponseSenderPtr        ) {}

    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy);

    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy);

    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy,
                                 const bool                    deletedByOwner);

    /*    virtual void OnRegistered(const Safir::Dob::Typesystem::TypeId      typeId,
                              const Safir::Dob::Typesystem::HandlerId&  handlerId);

    virtual void OnUnregistered(const Safir::Dob::Typesystem::TypeId      typeId,
    const Safir::Dob::Typesystem::HandlerId&  handlerId);*/

    void Pong(const Safir::Dob::EntityProxy& entityProxy);

    Safir::Dob::SecondaryConnection m_connection;

    const Safir::Dob::Typesystem::HandlerId m_handler;
    typedef std::map<Safir::Dob::Typesystem::InstanceId,Safir::Dob::Typesystem::InstanceId> PingPongTable;

    PingPongTable m_pingPongTable;
    DoseStressTest::PongPtr m_entity;
};

#endif

