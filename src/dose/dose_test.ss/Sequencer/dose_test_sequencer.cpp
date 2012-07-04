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
#include <Safir/Dob/DistributionChannel.h>
#include <Safir/Dob/DistributionChannelParameters.h>
#include <Safir/Dob/NodeParameters.h>
#include <DoseTest/Partner.h>
#include <DoseTest/PartnerResponseMessage.h>
#include <DoseTest/RootEntity.h>
#include <DoseTest/RootMessage.h>
#include <DoseTest/Dump.h>
#include <DoseTest/DumpResult.h>
#include <DoseTest/ComplexGlobalMessage.h>
#include <DoseTest/ComplexGlobalEntity.h>
#include <DoseTest/ComplexGlobalService.h>
#include <iostream>
#include <sstream>
#include <time.h>
#include <boost/bind.hpp>
#include <Safir/Dob/Typesystem/Members.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/thread.hpp>


#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4702)
#endif

#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif


#ifdef SendMessage
#undef SendMessage
#endif

#ifdef GetMessage
#undef GetMessage
#endif

Sequencer::Sequencer(const int startTc,
                     const int stopTc,
                     const Languages & languages,
                     const bool noTimeout,
                     const int contextId,
                     const std::string& multicastNic,
                     boost::asio::io_service& ioService):
    m_partnerState(languages),
    m_currentCaseNo(startTc),
    m_actionSender(multicastNic,ioService),
    m_stopTc(stopTc),
    m_state(Created),
    m_lastCleanupTime(boost::posix_time::second_clock::universal_time()),
    m_languages(languages),
    m_noTimeout(noTimeout),
    m_isDumpRequested(false),
    m_contextId(contextId),
    m_testConfig()
{
    // Find out if we are running in standalone or multinod configuration
    Safir::Dob::DistributionChannelPtr systemChannel = Safir::Dob::DistributionChannelParameters::DistributionChannels(0);
    if (systemChannel->MulticastAddress().Utf8String() == "127.0.0.1")
    {
        m_testConfig = DoseTest::TestConfigEnum::StandAlone;
    }
    else
    {
        m_testConfig = DoseTest::TestConfigEnum::Multinode;
    }

    m_connection.Attach();
}

Sequencer::~Sequencer()
{

}

void FillBinaryMemberInternal(Safir::Dob::Typesystem::BinaryContainer & cont)
{
    //we're only supposed to fill it if it is null
    if (cont.IsNull())
    {
        std::wcout << "Filling a binary member!" << std::endl;
        Safir::Dob::Typesystem::Binary b;
        const size_t size = 10 * 1024 * 1024; //10 Mb of data!
        b.reserve(size);
        char val = 0;
        for (size_t i = 0; i < size ; ++i)
        {
            b.push_back(val);
            ++val;
        }
        cont.SetVal(b);
    }
}

void MaybeFillBinaryMember(const Safir::Dob::Typesystem::ObjectPtr & object)
{
    if (object->GetTypeId() == DoseTest::ComplexGlobalMessage::ClassTypeId)
    {
        FillBinaryMemberInternal(boost::static_pointer_cast<DoseTest::ComplexGlobalMessage>(object)->BinaryMember().GetContainer());
    }
    else if (object->GetTypeId() == DoseTest::ComplexGlobalEntity::ClassTypeId)
    {
        FillBinaryMemberInternal(boost::static_pointer_cast<DoseTest::ComplexGlobalEntity>(object)->BinaryMember().GetContainer());
        //in the entity we use the binary array as well
        for (int i = 0; i < DoseTest::ComplexGlobalEntity::BinaryArrayMemberArraySize(); ++i)
        {
            FillBinaryMemberInternal(boost::static_pointer_cast<DoseTest::ComplexGlobalEntity>(object)->BinaryArrayMember()[i]);
        }
    }
    else if (object->GetTypeId() == DoseTest::ComplexGlobalService::ClassTypeId)
    {
        FillBinaryMemberInternal(boost::static_pointer_cast<DoseTest::ComplexGlobalService>(object)->BinaryMember().GetContainer());
    }
}



void Sequencer::PrepareTestcaseSetup()
{
    m_currentActionNo=-1;

    for(;;)
    {
        m_currentCase = TestCaseReader::Instance().GetTestCase(m_currentCaseNo);

        if (m_currentCase == NULL)
        {
            //no such testcase, try next one.
            if (++m_currentCaseNo > m_stopTc) return;
        }
        else if (!m_currentCase->TestConfig().IsNull() && m_currentCase->TestConfig().GetVal() != m_testConfig)
        {
            std::wcout << std::endl
                << "Skipping testcase " << m_currentCaseNo << " since it isn't applicable in "
                << DoseTest::TestConfigEnum::ToString(m_testConfig) << " mode." << std::endl;
            if (++m_currentCaseNo > m_stopTc) return;
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
    msg->ActionKind().SetVal(DoseTest::ActionEnum::Print);
    msg->PrintString().SetVal(out.str());
    m_actionSender.Send(msg);
}
void Sequencer::PrepareTestcaseExecution()
{
    DoseTest::ActionPtr msg = DoseTest::Action::Create();
    msg->ActionKind().SetVal(DoseTest::ActionEnum::Print);
    msg->PrintString().SetVal(L"--------- Test  -----------");
    m_actionSender.Send(msg);
    m_currentActionNo=-1;
}

void Sequencer::ExecuteCurrentAction()
{
    if (!VerifyAction(m_currentAction))
    {
        std::wcerr << " in testcase " << m_currentCaseNo << " action " << m_currentActionNo << std::endl;
        return;
    }
    if (m_currentAction->Partner().IsNull() && m_currentAction->ActionKind() == DoseTest::ActionEnum::Sleep)
    {
        std::wcout << "Sleeping " << m_currentAction->SleepDuration() << " seconds"<<std::endl;
        boost::this_thread::sleep(boost::posix_time::microseconds
                                  (static_cast<boost::int64_t>(m_currentAction->SleepDuration() * 1e6)));
    }
    else
    {
        //if it is a Complex* type we may need to put stuff in the binary member
        if (!m_currentAction->Object().IsNull())
        {
            MaybeFillBinaryMember(m_currentAction->Object().GetPtr());
        }

        m_actionSender.Send(m_currentAction);


        if (m_currentAction->ActionKind() == DoseTest::ActionEnum::CheckReferences ||
            m_currentAction->ActionKind() == DoseTest::ActionEnum::CloseAndCheckReferences ||
            m_currentAction->ActionKind() == DoseTest::ActionEnum::RunGarbageCollector)
        {
            // These actions will force the garbage collector to be run by the partner (if
            // the partner is of a GC type) so we wait a while before moving on.

            const unsigned int sleepDuration = 1;
            std::wcout << "Sleeping " << sleepDuration << " seconds" << std::endl;

            boost::this_thread::sleep(boost::posix_time::seconds(sleepDuration));
        }
    }
}

void Sequencer::SetState(const State newState)
{
    m_state = newState;
    if (newState == CleaningUpTestcase)
    {
        m_lastCleanupTime = boost::posix_time::second_clock::universal_time();
    }
}

void Sequencer::Tick()
{
    if (!m_noTimeout)
    {
        const boost::posix_time::ptime now = boost::posix_time::second_clock::universal_time();
        if (now - m_lastCleanupTime > boost::posix_time::minutes(10))
        {
            std::wcout << "TIMEOUT: Sequencer has not been in CleaningUpTestcase state for 10 minutes!"
                       << "Maybe a partner crashed? "
                       << "(" << m_languages.at(0).c_str() << ","
                       <<        m_languages.at(1).c_str() << ","
                       <<        m_languages.at(2).c_str() << ")"
                       << std::endl
                       << "Exiting." << std::endl;
            exit(1);
        }
    }

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
                    m_partnerState.Activate(i, m_contextId);
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
            boost::this_thread::sleep(boost::posix_time::milliseconds(300));
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
    if (action->Partner().IsNull() && action->ActionKind() != DoseTest::ActionEnum::Sleep)
    {
        std::wcerr << "No partner specified";
        return false;
    }

    switch (action->ActionKind().GetVal())
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

    // Create context dir if not context 0
    if (m_contextId > 0)
    {
        directory = directory / ("context_" + boost::lexical_cast<std::string>(m_contextId));
        boost::filesystem::create_directory(directory);
    }

    std::ostringstream filename;
    filename << "partner"
        << partner
        << ".txt";
    boost::filesystem::wofstream file(directory / filename.str());

    DoseTest::DumpResultPtr result = boost::dynamic_pointer_cast<DoseTest::DumpResult>(responseProxy.GetResponse());

    if (result == NULL)
    {
        std::wcout << "GOT BAD RESPONSE FROM PARTNER " << partner << std::endl;

        file << "GOT BAD RESPONSE FROM PARTNER " << partner << std::endl
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
