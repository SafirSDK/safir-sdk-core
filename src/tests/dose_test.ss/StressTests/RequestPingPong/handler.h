/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
*
* Created by: Mikael Wennerberg/ stmiwn
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


#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/Connection.h>

class Handler: 
    public Safir::Dob::EntityHandlerInjection
{
public:
    void Start();

protected:
    void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
        const Safir::Dob::Typesystem::HandlerId& handlerId) override;

    void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr responseSender) override;

    void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr responseSender) override;

    void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
        Safir::Dob::ResponseSenderPtr responseSender) override;

    void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId,
        const Safir::Dob::Typesystem::HandlerId& handlerId) override;


private:
    Safir::Dob::SecondaryConnection m_Connection;
    Safir::Dob::Typesystem::HandlerId m_handlerId;

};
