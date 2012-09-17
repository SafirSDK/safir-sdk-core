/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagstr�m / stlrha
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
#include "PartnerState.h"

#include <DoseTest/Sequencer.h>
#include <boost/bind.hpp>
#include <iostream>

#if 0

#include <DoseTest/Action.h>
#include <Safir/Dob/OverflowException.h>
#endif

PartnerState::PartnerState(const Languages & languages):
    m_partnerInfoTable(3),
    m_languages(languages)
{
    m_connection.Attach();
    m_connection.SubscribeEntity(DoseTest::Partner::ClassTypeId,this);
    m_connection.RegisterEntityHandler(DoseTest::Sequencer::ClassTypeId,
                                       Safir::Dob::Typesystem::HandlerId(),
                                       Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                       this);
    DoseTest::SequencerPtr seq = DoseTest::Sequencer::Create();
    seq->Partners()[0].SetVal(Safir::Dob::Typesystem::Utilities::ToWstring(languages[0]));
    seq->Partners()[1].SetVal(Safir::Dob::Typesystem::Utilities::ToWstring(languages[1]));
    seq->Partners()[2].SetVal(Safir::Dob::Typesystem::Utilities::ToWstring(languages[2]));

    m_connection.SetAll(seq,
                        Safir::Dob::Typesystem::InstanceId(),
                        Safir::Dob::Typesystem::HandlerId());
}



bool
PartnerState::IsReady() const
{
    return std::find_if(m_partnerInfoTable.begin(),m_partnerInfoTable.end(),!boost::bind(&PartnerInfo::IsReady,_1)) == m_partnerInfoTable.end();
}


bool
PartnerState::IsReady(const int which) const
{
    return m_partnerInfoTable.at(which).IsReady();
}


void
PartnerState::SetNotReady()
{
    std::for_each(m_partnerInfoTable.begin(),m_partnerInfoTable.end(),boost::bind(&PartnerInfo::SetReady,_1,false));
}

#if 0
void
PartnerState::Activate(const int which, const int contextId)
{
    if (IsActive(which))
    {
        std::wcerr << "Partner " << which << " is already Active!" << std::endl;
    }
    else
    {
        m_partnerInfoTable.at(which).m_incarnation = -1;
        DoseTest::ActionPtr activate = DoseTest::Action::Create();
        activate->ActionKind().SetVal(DoseTest::ActionEnum::Activate);
        activate->Identifier().SetVal(Safir::Dob::Typesystem::Utilities::ToWstring(m_languages.at(which)));
        activate->Context().SetVal(contextId);

        try
        {
            m_connection.Send(activate,Safir::Dob::Typesystem::ChannelId(which),this);
        }
        catch (const Safir::Dob::OverflowException &)
        {
            std::wcerr << "Overflow in message out queue when sending Activate to Partner " << which << "." << std::endl;
        }
    }

}

void
PartnerState::Deactivate(const int which)
{
    if (!IsActive(which))
    {
        std::wcerr << "Partner " << which << " is not Active!" << std::endl;
    }
    else
    {
        m_partnerInfoTable.at(which).m_incarnation = -1;
        DoseTest::ActionPtr deactivate = DoseTest::Action::Create();
        deactivate->ActionKind().SetVal(DoseTest::ActionEnum::Deactivate);
        deactivate->Identifier().SetVal(Safir::Dob::Typesystem::Utilities::ToWstring(m_languages.at(which)));

        try
        {
            m_connection.Send(deactivate,Safir::Dob::Typesystem::ChannelId(which),this);
        }
        catch (const Safir::Dob::OverflowException &)
        {
            std::wcerr << "Overflow in message out queue when sending Deactivate to Partner " << which << "." << std::endl;
        }
    }

}
#endif

bool
PartnerState::IsActive(const int which) const
{
    return m_partnerInfoTable.at(which).IsActive();
}

#if 0
void
PartnerState::Reset(const int which)
{
    if (IsReady(which))
    {
        std::wcerr << "Partner " << which << " is already Ready!" << std::endl;
    }
    else
    {
        DoseTest::ActionPtr reset = DoseTest::Action::Create();
        reset->ActionKind().SetVal(DoseTest::ActionEnum::Reset);

        try
        {
            m_connection.Send(reset,Safir::Dob::Typesystem::ChannelId(which),this);
        }
        catch (const Safir::Dob::OverflowException &)
        {
            std::wcerr << "Overflow in message out queue when sending Reset to Partner " << which << "." << std::endl;
        }
    }

}
#endif

void
PartnerState::HandlePartnerChange(const DoseTest::PartnerPtr & partner, const int instance)
{
    PartnerInfo & thePartner = m_partnerInfoTable[instance];

    if (partner->Identifier().IsChanged())
    {
        if (partner->Identifier() == Safir::Dob::Typesystem::Utilities::ToWstring(m_languages.at(instance)))
        {
            thePartner.SetActive(true);
            std::wcout << "Partner " << instance << " is activated!" << std::endl;
        }
        else
        {
            std::wcerr << "Partner " << instance << " is not running in the right language!" <<std::endl;
            return;
        }
    }

    assert(thePartner.IsActive());

    if (partner->Incarnation().IsChanged())
    {
        if (partner->Incarnation() > thePartner.m_incarnation)
        {
            thePartner.SetReady(true);
            thePartner.m_incarnation = partner->Incarnation();
        }
    }
}


void PartnerState::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    HandlePartnerChange(boost::static_pointer_cast<DoseTest::Partner>(entityProxy.GetEntityWithChangeInfo()),
                        static_cast<int>(entityProxy.GetInstanceId().GetRawValue()));
}

void PartnerState::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{
    HandlePartnerChange(boost::static_pointer_cast<DoseTest::Partner>(entityProxy.GetEntityWithChangeInfo()),
                        static_cast<int>(entityProxy.GetInstanceId().GetRawValue()));
}

void PartnerState::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy,
                                   const bool                    deletedByOwner)
{
    if (!deletedByOwner)
    {
        return;
    }

    const int instance = static_cast<int>(entityProxy.GetInstanceId().GetRawValue());
    m_partnerInfoTable[instance].SetReady(false);
    m_partnerInfoTable[instance].SetActive(false);
    m_partnerInfoTable[instance].m_incarnation = -1;
}




void PartnerState::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                         const Safir::Dob::Typesystem::HandlerId& handlerId)
{
    throw std::logic_error("Someone revoked my registrations!!! Don't like that!");
}


