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

#ifndef __INJECTION_TIMESTAMP_HANDLER_H__
#define __INJECTION_TIMESTAMP_HANDLER_H__

#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/SecondaryConnection.h>


class InjectionTimestampHandler:
    public Safir::Dob::EntityHandlerInjection,
    private boost::noncopyable
{
public:
    InjectionTimestampHandler();

private:
    void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                               const Safir::Dob::Typesystem::HandlerId& handlerId) override;

    void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                         Safir::Dob::ResponseSenderPtr        responseSender) override;

    void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                         Safir::Dob::ResponseSenderPtr        responseSender) override;

    void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                         Safir::Dob::ResponseSenderPtr        responseSender) override;

    void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId,
                                 const Safir::Dob::Typesystem::HandlerId& handlerId) override;

    void OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy /*injectedEntityProxy*/) override
    {m_hasInstance = true;}

    Safir::Dob::SecondaryConnection m_connection;

    bool m_hasInstance;


};

#endif

