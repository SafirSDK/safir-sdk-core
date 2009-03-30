/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Erik Adolfsson / sterad
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
#pragma once
#include <Safir/Dob/Consumer.h>

class Consumer : public Safir::Dob::EntitySubscriber,
                 public Safir::Dob::EntityHandler,
                 public Safir::Dob::StopHandler
{
public:
    Consumer(void);
    ~Consumer(void);

    void OnNewEntity(const Safir::Dob::EntityProxy entityProxy) { OnUpdatedEntity( entityProxy ); };
    void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy);
    void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool deletedByOwner);

    void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId  typeId, const Safir::Dob::Typesystem::HandlerId& handlerId);

    void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender);

    void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender);

    void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr        responseSender);

    void OnStopOrder(){};
};
