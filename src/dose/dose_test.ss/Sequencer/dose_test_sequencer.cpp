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

#include "dose_test_sequencer.h"
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/NotFoundException.h>
#include <DoseTest/Partner.h>
#include <DoseTest/PartnerResponseMessage.h>
#include <DoseTest/RootEntity.h>
#include <DoseTest/RootMessage.h>
#include <DoseTest/Dump.h>
#include <DoseTest/DumpResult.h>
#include <iostream>
#include <sstream>
#include <time.h>
#include <boost/bind.hpp>
#include <Safir/Dob/Typesystem/Members.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/lexical_cast.hpp>
#include <ace/OS_NS_unistd.h>


#ifdef SendMessage
#undef SendMessage
#endif

#ifdef GetMessage
#undef GetMessage
#endif

Sequencer::Sequencer(const int startTc, const int stopTc, const Languages & languages):
    m_partnerState(languages),
    m_currentCaseNo(startTc),
    m_stopTc(stopTc),
    m_state(Created),
    m_languages(languages),
    m_isDumpRequested(false)
{
    m_connection.Attach();
}

Sequencer::~Sequencer()
{

}


void Sequencer::PrepareTestcaseSetup()
{
    m_currentActionNo=-1;

    for(;;)
    {
        m_currentCase = TestCaseReader::Instance().GetTestCase(m_currentCaseNo);

        if(m_currentCase == NULL)
        {//no such testcase, try next one.
            ++m_currentCaseNo;
            if (m_currentCaseNo > m_stopTc)
            {
                return;
            }
        }
        else
        {
            break;
        }
    }

    std::wcout << std::endl
        << "Running testcase: " << m_currentCaseNo << std::endl;

    if (!m_currentCase->AlwaysMarkAsFailed().IsNull() && m_currentCase->AlwaysMarkAsFailed().GetVal())
    {
        std::wcout << "NOT FULLY IMPLEMENTED!!!" <<std::endl;
    }


    //Send some printing data to partners
    std::wostringstream out;

    out << "=========================================================================="<<std::endl
        << "TESTCASE " << m_currentCaseNo << std::endl
        << "Description: "
        << m_currentCase->Description().GetVal()
        << std::endl;
    out << "Expectation: "
        << m_currentCase->Expectation().GetVal();

    if (!m_currentCase->AlwaysMarkAsFailed().IsNull() && m_currentCase->AlwaysMarkAsFailed().GetVal())
    {
        out << std::endl << "------------------------- NOT FULLY IMPLEMENTED!!! ----------------------" << rand();
    }

    out << std::endl << "--------- Setup -----------";

    DoseTest::ActionPtr msg = DoseTest::Action::Create();
    msg->ActionType().SetVal(DoseTest::ActionEnum::Print);
    msg->PrintString().SetVal(out.str());
    m_connection.Send(msg,Safir::Dob::Typesystem::ChannelId(),this);
}
void Sequencer::PrepareTestcaseExecution()
{
    DoseTest::ActionPtr msg = DoseTest::Action::Create();
    msg->ActionType().SetVal(DoseTest::ActionEnum::Print);
    msg->PrintString().SetVal(L"--------- Test  -----------");
    m_connection.Send(msg,Safir::Dob::Typesystem::ChannelId(),this);
    m_currentActionNo=-1;
}

void Sequencer::OnNotMessageOverflow()
{
    std::wcout << "On_Not_Message_Overflow"
               << std::endl;
}


void Sequencer::ExecuteCurrentAction()
{
    if (!VerifyAction(m_currentAction))
    {
        std::wcerr << " in testcase " << m_currentCaseNo << " action " << m_currentActionNo << std::endl;
        return;
    }
    if (m_currentAction->Partner().IsNull() && m_currentAction->ActionType() == DoseTest::ActionEnum::Sleep)
    {
        // No partner given so it's a sequencer sleep.
        std::wcout << "Sleeping " << m_currentAction->SleepDuration().GetVal() << " seconds"<<std::endl;
        const double decimals = m_currentAction->SleepDuration() - static_cast<unsigned int>(m_currentAction->SleepDuration());

        const ACE_Time_Value time(static_cast<unsigned int>(m_currentAction->SleepDuration()),
                                  static_cast<unsigned int>(decimals * 1000000));
        ACE_OS::sleep(time);
    }
    else
    {
        m_connection.Send(m_currentAction,m_currentAction->Partner(),this);
    }
}

void Sequencer::SetState(const State newState)
{
    m_state = newState;
}

void Sequencer::Tick()
{
    /*
    if (!m_partnerState.IsReady())
    {
        std::wcout << "Some partner is not ready!" <<std::endl;
        m_state = Created;
        return;
    }
    else
    {
        std::wcout << "All partners are ready!" <<std::endl;
    }*/

    switch (m_state)
    {
    case Created:
        {
            SetState(ActivatingPartners);
        }
        break;

    case ActivatingPartners:
        {
            bool allStarted = true;
            for (int i = 0; i < 3; ++i)
            {
                if (!m_partnerState.IsActive(i))
                {
                    allStarted = false;
                    m_partnerState.Activate(i);
                    std::wcout << "Sending Activate to " << i << std::endl;
                }
            }

            if (allStarted)
            {
                SetState(ResetPartners);
            }
        }
        break;

    case ResetPartners:
        {
            m_partnerState.SetNotReady();
            for (int i = 0; i < 3; ++i)
            {
                m_partnerState.Reset(i);
                std::wcout << "Sending Reset to " << i << std::endl;
            }

            SetState(PreparingTestcaseSetup);
        }
        break;

    case PreparingTestcaseSetup:
        {
            if (m_partnerState.IsReady())
            {
                PrepareTestcaseSetup();
                SetState(RunningSetupAction);
            }
            else
            {
                std::wcout << "Not all partners are ready yet! Trying to Reset them again." <<std::endl;
                for (int i = 0; i < 3; ++i)
                {
                    std::wcout << "Partner " << i << " is ready: " << m_partnerState.IsReady(i) << std::endl;
                }
                SetState(ResetPartners);
            }
        }
        break;
    case RunningSetupAction:
        {
            if (m_partnerState.IsReady())
            {
                m_currentActionNo++;
                if (m_currentCase->TestCaseSetupActions()[m_currentActionNo].IsNull())
                {
                    SetState(PreparingTestcaseExecution);
                }
                else
                {
                    m_currentAction=m_currentCase->TestCaseSetupActions()[m_currentActionNo].GetPtr();
                    ExecuteCurrentAction();
                }
            }
            else
            {
                SetState(ActivatingPartners);
            }
        }
        break;

    case PreparingTestcaseExecution:
        {
            if (m_partnerState.IsReady())
            {
                PrepareTestcaseExecution();
                SetState(RunningTestAction);
            }
            else
            {
                SetState(ActivatingPartners);
            }
        }
        break;
    case RunningTestAction:
        {
            if (m_partnerState.IsReady())
            {
                m_currentActionNo++;
                if (m_currentCase->TestActions()[m_currentActionNo].IsNull())
                {
                    SetState(CleaningUpTestcase);

                }
                else
                {
                    m_currentAction=m_currentCase->TestActions()[m_currentActionNo].GetPtr();
                    ExecuteCurrentAction();
                }
            }
            else
            {
                SetState(ActivatingPartners);
            }
        }
        break;
    case CleaningUpTestcase:
        {
            ACE_OS::sleep(ACE_Time_Value(0,300000));
            std::wcout << "Test completed" <<std::endl;
            SetState(ResetPartners);
            m_currentCaseNo++;
        }
        break;
    }
}

bool Sequencer::DeactivateAll()
{
    bool allDeactivated = true;
    for (int i = 0; i < 3; ++i)
    {
        if (m_partnerState.IsActive(i))
        {
            allDeactivated = false;
            m_partnerState.Deactivate(i);
            std::wcout << "Sending deactivate to " << i << std::endl;
        }
    }

    return allDeactivated;
}

bool Sequencer::VerifyAction(DoseTest::ActionPtr action)
{
    if (action->Partner().IsNull() && action->ActionType() != DoseTest::ActionEnum::Sleep)
    {
        std::wcerr << "No partner specified";
        return false;
    }

    switch (action->ActionType().GetVal())
    {
    case DoseTest::ActionEnum::SendMessage:
        {
            if (action->Object().IsNull())
            {
                std::wcerr << L"No message to send supplied";
                return false;
            }
            if (NULL == boost::dynamic_pointer_cast<Safir::Dob::Message>(action->Object().GetPtr()))
            {
                std::wcerr << L"Obj is not a Message";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::SetAll:
    case DoseTest::ActionEnum::InitialSet:
        {
            if (action->Object().IsNull())
            {
                std::wcerr << L"No entity to set supplied";
                return false;
            }
            if (NULL == boost::dynamic_pointer_cast<Safir::Dob::Entity>(action->Object().GetPtr()))
            {
                std::wcerr << L"Obj is not an Entity";
                return false;
            }
            if (action->Instance().IsNull())
            {
                std::wcerr << "Instance missing in Set";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::Delete:
        {
            if (action->EntityId().IsNull())
            {
                std::wcerr << "EntityId missing in Delete";
                return false;
            }
            if (action->Handler().IsNull())
            {
                std::wcerr << "Handler missing in Delete";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::InjectDelete:
        {
            if (action->EntityId().IsNull())
            {
                std::wcerr << "EntityId missing in DeleteWithTimestamp";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::Postpone:
        {
            if (action->RedispatchCurrent().IsNull())
            {
                std::wcerr << "RedispatchCurrent missing in Postpone";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::DeleteAllInstances:
        {
            if (action->TypeId().IsNull())
            {
                std::wcerr << "TypeId missing in DeleteAllInstances";
                return false;
            }
            if (action->Handler().IsNull())
            {
                std::wcerr << "Handler missing in DeleteAllInstances";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::RegisterEntityHandler:
    case DoseTest::ActionEnum::RegisterEntityHandlerInjection:
    case DoseTest::ActionEnum::RegisterEntityHandlerPending:
        {
            if (action->TypeId().IsNull())
            {
                std::wcerr << "Type missing in RegisterEntityHandler...";
                return false;
            }
            if (action->Handler().IsNull())
            {
                std::wcerr << "Handler missing in RegisterEntityHandler...";
                return false;
            }
            if (action->InstanceIdPolicy().IsNull())
            {
                std::wcerr << "InstanceIdPolicy missing in RegisterEntityHandler...";
                return false;
            }

        }
        break;

    case DoseTest::ActionEnum::RegisterServiceHandler:
        {
            if (action->TypeId().IsNull())
            {
                std::wcerr << "Type missing in RegisterServiceHandler";
                return false;
            }

            if (action->Handler().IsNull())
            {
                std::wcerr << "Handler missing in RegisterServiceHandler";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::UnregisterHandler:
        {
            if (action->TypeId().IsNull())
            {
                std::wcerr << "Type missing in UnregisterHandler";
                return false;
            }

            if (action->Handler().IsNull())
            {
                std::wcerr << "Handler missing in UnregisterHandler";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::CreateRequest:
        {
            if (action->Object().IsNull())
            {
                std::wcerr << L"No entity to create supplied";
                return false;
            }
            if (NULL == boost::dynamic_pointer_cast<Safir::Dob::Entity>(action->Object().GetPtr()))
            {
                std::wcerr << L"Obj is not an Entity";
                return false;
            }
            if (action->Handler().IsNull())
            {
                std::wcerr << "Handler missing in CreateRequest";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::UpdateRequest:
        {
            if (action->Object().IsNull())
            {
                std::wcerr << L"No entity to update supplied";
                return false;
            }
            if (NULL == boost::dynamic_pointer_cast<Safir::Dob::Entity>(action->Object().GetPtr()))
            {
                std::wcerr << L"Obj is not an Entity";
                return false;
            }
            if (action->Instance().IsNull())
            {
                std::wcerr << "Instance missing in UpdateRequest";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::SubscribeRegistration:
        {
            if (action->TypeId().IsNull())
            {
                std::wcerr << "Type missing in SubscribeRegistration";
                return false;
            }
            if (action->Handler().IsNull())
            {
                std::wcerr << "Handler missing in SubscribeRegistration";
                return false;
            }
            if (action->RestartSubscription().IsNull())
            {
                std::wcerr << "RestartSubscription missing in SubscribeRegistration";
                return false;
            }
            if (action->IncludeSubclasses().IsNull())
            {
                std::wcerr << "IncludeSubclasses missing in SubscribeRegistration";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::UnsubscribeRegistration:
        {
            if (action->TypeId().IsNull())
            {
                std::wcerr << "Type missing in UnsubscribeRegistration";
                return false;
            }

            if (action->Handler().IsNull())
            {
                std::wcerr << "Handler missing in UnsubscribeRegistration";
                return false;
            }
            if (action->IncludeSubclasses().IsNull())
            {
                std::wcerr << "IncludeSubclasses missing in UnsubscribeRegistration";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::SubscribeEntity:
        {
            if (action->TypeId().IsNull() && action->EntityId().IsNull())
            {
                std::wcerr << "TypeId/EntityId missing in SubscribeEntity";
                return false;
            }
            if (action->IncludeUpdates().IsNull())
            {
                std::wcerr << "IncludeUpdates missing in SubscribeEntity";
                return false;
            }
            if (action->RestartSubscription().IsNull())
            {
                std::wcerr << "RestartSubscription missing in SubscribeEntity";
                return false;
            }
            if (!action->TypeId().IsNull() && action->IncludeSubclasses().IsNull())
            {
                std::wcerr << "IncludeSubclasses missing in SubscribeEntity";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::InjectorSubscribeEntity:
        {
            if (action->TypeId().IsNull())
            {
                std::wcerr << "TypeId missing in SpecialSubscribeEntity";
                return false;
            }
            if (action->IncludeUpdates().IsNull())
            {
                std::wcerr << "IncludeUpdates missing in SpecialSubscribeEntity";
                return false;
            }
            if (action->RestartSubscription().IsNull())
            {
                std::wcerr << "RestartSubscription missing in SpecialSubscribeEntity";
                return false;
            }
            if (action->IncludeSubclasses().IsNull())
            {
                std::wcerr << "IncludeSubclasses missing in SpecialSubscribeEntity";
                return false;
            }
            if (action->WantsGhostDelete().IsNull())
            {
                std::wcerr << "WantsGhostDelete missing in SpecialSubscribeEntity";
                return false;
            }
            if (action->WantsLastState().IsNull())
            {
                std::wcerr << "WantsLastState missing in SpecialSubscribeEntity";
                return false;
            }
            if (action->DoesntWantSourceIsPermanentStore().IsNull())
            {
                std::wcerr << "DoesntWantSourceIsPermanentStore missing in SpecialSubscribeEntity";
                return false;
            }
            if (action->WantsAllStateChanges().IsNull())
            {
                std::wcerr << "WantsAllStateChanges missing in SpecialSubscribeEntity";
                return false;
            }
            if (action->TimestampChangeInfo().IsNull())
            {
                std::wcerr << "TimestampChangeInfo missing in SpecialSubscribeEntity";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::UnsubscribeEntity:
        {
            if (action->TypeId().IsNull() && action->EntityId().IsNull())
            {
                std::wcerr << "TypeId/EntityId missing in UnsubscribeEntity";
                return false;
            }
            if (!action->TypeId().IsNull() && action->IncludeSubclasses().IsNull())
            {
                std::wcerr << "IncludeSubclasses missing in UnsubscribeEntity";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::SimulateOverflows:
        {
            if (action->InQueues().IsNull() || action->OutQueues().IsNull())
            {
                std::wcerr << "InQueues or OutQueues flag missing in SimulateOverflows";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::SetChanges:
    case DoseTest::ActionEnum::InjectChanges:
        {
            if (action->Object().IsNull())
            {
                std::wcerr << "Set missing in Set";
                return false;
            }

            if (NULL == boost::dynamic_pointer_cast<Safir::Dob::Entity>(action->Object().GetPtr()))
            {
                std::wcerr << L"Obj is not an Entity";
                return false;
            }

            //Setting all members to changed
            Safir::Dob::EntityPtr newEntityPtr =
                boost::static_pointer_cast<Safir::Dob::Entity>(action->Object().GetPtr());

            const int noOfMembers =
                Safir::Dob::Typesystem::Members::GetNumberOfMembers(newEntityPtr->GetTypeId());

            for (int i = 0; i < noOfMembers; ++i)
            {
                if (!newEntityPtr->GetMember(i, 0).IsNull())
                {
                    newEntityPtr->GetMember(i, 0).SetChanged(true);
                }
            }
        }
        break;

    case DoseTest::ActionEnum::GetEntityIterator:
        {
            if (action->TypeId().IsNull())
            {
                std::wcerr << "Type missing in GetIterator";
                return false;
            }
            if (action->IncludeSubclasses().IsNull())
            {
                std::wcerr << "IncludeSubclasses missing in GetIterator";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::IsCreated:
        {
            if (action->EntityId().IsNull())
            {
                std::wcerr << L"No EntityId specified in IsCreated";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::GetNumberOfInstances:
        {
            if (action->Handler().IsNull())
            {
                std::wcerr << L"No Handler specified in IsCreated";
                return false;
            }
            if (action->IncludeSubclasses().IsNull())
            {
                std::wcerr << L"No IncludeSubclasses specified in IsCreated";
                return false;
            }
            if (action->TypeId().IsNull())
            {
                std::wcerr << L"No TypeId specified in IsCreated";
                return false;
            }
        }
        break;

    case DoseTest::ActionEnum::GetQueueCapacity:
    case DoseTest::ActionEnum::GetQueueSize:
        {
            if (action->ConnectionQueueId().IsNull())
            {
                std::wcerr << "No ConnectionQueueId specified in Queue call";
                return false;
            }
        }
        break;

    default:
        return true;
    }
    return true;
}


void
Sequencer::GetTestResults(const int fileNumber)
{
    assert(!m_isDumpRequested);

    m_fileNumber = fileNumber;
    DoseTest::DumpPtr request = DoseTest::Dump::Create();
    for (int i = 0; i < 3; ++i)
    {
        const Safir::Dob::RequestId reqId = m_connection.ServiceRequest(request,
            Safir::Dob::Typesystem::HandlerId(i),this);
        m_dumpRequestIds[reqId] = i; //insert into map
    }
    m_isDumpRequested = true;

}

bool
Sequencer::GotTestResults() const
{
    return m_isDumpRequested && m_dumpRequestIds.empty();
}

void
Sequencer::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
{
    const int partner = m_dumpRequestIds.find(responseProxy.GetRequestId())->second;

    boost::filesystem::path directory = "run" + boost::lexical_cast<std::string>(m_fileNumber)
        + "-" + m_languages.at(0) + "-" + m_languages.at(1) + "-" + m_languages.at(2);
    boost::filesystem::create_directory(directory);

    std::ostringstream filename;
    filename << "partner"
        << partner
        << ".txt";
    boost::filesystem::wofstream file(directory / filename.str());

    DoseTest::DumpResultPtr result = boost::dynamic_pointer_cast<DoseTest::DumpResult>(responseProxy.GetResponse());

    if (result == NULL)
    {
        std::wcout << "GOT BAD RESPONSE FROM PARTNER" << partner << std::endl;

        file << "GOT BAD RESPONSE FROM PARTNER" << partner << std::endl
            << Safir::Dob::Typesystem::Serialization::ToXml(responseProxy.GetBlob());
    }
    else
    {
        std::wcout << "Got result from partner " << partner << std::endl;
        file << result->Result().GetVal();
    }
    file.close();

    m_dumpRequestIds.erase(responseProxy.GetRequestId());
}
