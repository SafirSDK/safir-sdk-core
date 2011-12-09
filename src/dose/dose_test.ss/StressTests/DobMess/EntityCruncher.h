/******************************************************************************
*
* Copyright Saab AB, 2007-2011 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / joot
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
#ifndef _EntityCruncher_h_
#define _EntityCruncher_h_

#include <Safir/Dob/Connection.h>
#include <DoseStressTest/Ping.h>
#include "SimpleReactor.h"
#include "Supervisor.h"

class EntityCruncher :  public Safir::Dob::EntityHandler,
                        public Safir::Dob::EntitySubscriber,
                        public Safir::Dob::Requestor,
                        public Safir::Dob::StopHandler,
                        public SimpleReactor                     
{
public:
    EntityCruncher(int maxNumEntities, bool createEnabled, bool deleteEnabled, bool verbose);
    ~EntityCruncher(void);

private:
    int m_pid;
    Safir::Dob::Connection m_con;
    Safir::Dob::Typesystem::HandlerId m_handler;
    Supervisor m_supervisor;
    int m_numberOfEntities;
    int m_maxNumberOfEntities; 
    bool m_entityCreateEnabled;
    bool m_entityDeleteEnabled;
    bool m_verbose;

    //SimpleReactor
    virtual void DoDispatch();
    virtual void OnTimeout(int timerId);

    //StopHandler
    virtual void OnStopOrder();

    //EntityHandler
    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId& handlerId);
    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy requestProxy,Safir::Dob::ResponseSenderPtr responseSender);
    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy requestProxy, Safir::Dob::ResponseSenderPtr responseSender);
    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy requestProxy, Safir::Dob::ResponseSenderPtr responseSender);

    //EntitySubscriber
    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy);
    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy);
    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool deletedByOwner);

    //Requestor
    virtual void OnResponse(const Safir::Dob::ResponseProxy responseProxy);    
    virtual void OnNotRequestOverflow();

    void SendRequest(const Safir::Dob::EntityProxy& entityProxy);
    
    static const int EntityShowerSize = 10;    //10 new entities in each shower
    static const int EntityShowerInterval = 1000; //1 seconds between showers
    static const int CreateEntityTimerId = 100;
    

};

#endif
