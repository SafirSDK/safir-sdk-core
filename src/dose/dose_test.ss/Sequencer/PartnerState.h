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
#ifndef __PARTNERSTATE_H__
#define __PARTNERSTATE_H__

#include <DoseTest/Partner.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ErrorResponse.h>
#include <vector>

typedef std::vector<std::string> Languages;




class PartnerState:
    public Safir::Dob::EntityHandler,
    public Safir::Dob::EntitySubscriber,
    public Safir::Dob::MessageSender,
    private boost::noncopyable
{
public:
    PartnerState(const Languages & languages,
                 const int contextId);

    bool IsActive(const int which) const;

    void Reset(const int which);
    bool IsReady() const;
    bool IsReady(const int which) const;

    void SetNotReady();

private:
    void HandlePartnerChange(const DoseTest::PartnerPtr & partner, const int instance);

    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy);
    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy);
    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy,
                                 const bool                    deletedByOwner);

    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                       const Safir::Dob::Typesystem::HandlerId& handlerId);
    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender)
    {responseSender->Send(Safir::Dob::ErrorResponse::Create());}
    
    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender)
    {responseSender->Send(Safir::Dob::ErrorResponse::Create());}

    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender)
    {responseSender->Send(Safir::Dob::ErrorResponse::Create());}

    virtual void OnNotMessageOverflow(){}

    struct PartnerInfo
    {
    public:
        PartnerInfo():m_incarnation(-1), m_ready(false),m_active(false) {}

        bool IsActive() const {return m_active;}
        void SetActive(const bool active) {m_active = active;}

        bool IsReady()const {return IsActive() && m_ready;}
        void SetReady(const bool ready) {m_ready = ready;}

        int     m_incarnation;
    private:
        bool    m_ready;
        bool    m_active;
    };

    typedef std::vector<PartnerInfo> PartnerInfoTable;
    PartnerInfoTable m_partnerInfoTable;

    Safir::Dob::SecondaryConnection m_connection;
    const Languages m_languages;

};
#endif

