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

#ifndef __PINGER_H__
#define __PINGER_H__

#include <Safir/Dob/Connection.h>
#include <map>
#include <set>
#include <DoseStressTest/Ping.h>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4127)
#endif

#include <boost/date_time/posix_time/posix_time.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif

class Pinger :
    public Safir::Dob::EntityHandler,
    public Safir::Dob::EntitySubscriber,
    public Safir::Dob::RegistrationSubscriber,
    private boost::noncopyable
{
public:
    Pinger();

    void CheckForTimeouts();

private:
    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId,
                                       const Safir::Dob::Typesystem::HandlerId& handlerId);

    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy ,
                                 Safir::Dob::ResponseSenderPtr        ) {}

    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy ,
                                 Safir::Dob::ResponseSenderPtr        ) {}

    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy ,
                                 Safir::Dob::ResponseSenderPtr        ) {}

    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
    {HandlePong(entityProxy);}

    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
    {HandlePong(entityProxy);}

    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy /*entityProxy*/,
                                 const bool                    /*deletedByOwner*/) {}

    virtual void OnRegistered(const Safir::Dob::Typesystem::TypeId      typeId,
                              const Safir::Dob::Typesystem::HandlerId&  handlerId);

    virtual void OnUnregistered(const Safir::Dob::Typesystem::TypeId      typeId,
                                const Safir::Dob::Typesystem::HandlerId&  handlerId);


    void Ping(const Safir::Dob::Typesystem::InstanceId& instance);

    void HandlePong(const Safir::Dob::EntityProxy& entityProxy);

    Safir::Dob::SecondaryConnection m_connection;

    const Safir::Dob::Typesystem::HandlerId m_handler;

    typedef std::set<Safir::Dob::Typesystem::HandlerId> Pongers;

    struct PingPongData
    {
        explicit PingPongData():number(-1),pongers() {}
        Safir::Dob::Typesystem::Int64 number;
        Pongers pongers;
        boost::posix_time::ptime pingTime;
    };

    typedef std::map<Safir::Dob::Typesystem::InstanceId,PingPongData> PingPongTable;

    PingPongTable m_pingPongTable;
    DoseStressTest::PingPtr m_entity;

    Pongers m_pongers;
};

#endif

