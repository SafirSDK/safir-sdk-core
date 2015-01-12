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
#ifndef __PARTNERSTATE_H__
#define __PARTNERSTATE_H__

#include <DoseTest/Partner.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ErrorResponse.h>
#include <vector>
#include <boost/function.hpp>
#include "ActionSender.h"

typedef std::vector<std::string> Languages;

class PartnerState:
    public Safir::Dob::EntityHandler,
    public Safir::Dob::EntitySubscriber,
    private boost::noncopyable
{
public:
    PartnerState(const Languages & languages,
                 const int contextId,
                 ActionSender& actionSender,
                 const boost::function<void()>& stateChangedCallback);
    //callback is called whenever something happens that might change states
    void Reset();
    bool IsReady() const;
    bool IsActive() const;
    const std::string& Address(const int which) const;
    short Port(const int which) const;
    
private:




    bool IsReady(const int which) const;
    
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

    struct PartnerInfo
    {
    public:
        PartnerInfo():m_incarnation(-1), m_ready(false),m_active(false) {}

        bool IsActive() const {return m_active;}
        void SetActive(const bool active) {m_active = active;}

        bool IsReady()const {return IsActive() && m_ready;}
        void SetReady(const bool ready) {m_ready = ready;}

        int         m_incarnation;
        std::string m_address;
        short       m_port;
    private:
        bool    m_ready;
        bool    m_active;
    };

    typedef std::vector<PartnerInfo> PartnerInfoTable;
    PartnerInfoTable m_partnerInfoTable;

    Safir::Dob::SecondaryConnection m_connection;
    const Languages m_languages;

    const boost::function<void()> m_stateChangedCallback;

    ActionSender& m_actionSender;
};
#endif

