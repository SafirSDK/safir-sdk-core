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
 #include "Sequencer.h"
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

namespace 
{
    void FillBinaryMemberInternal(Safir::Dob::Typesystem::BinaryContainer & cont)
    {
        //we're only supposed to fill it if it is null
        if (cont.IsNull())
        {
            std::wcout << " - Filling a binary member!" << std::endl;
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

}

#ifdef _MSC_VER
   #pragma warning(push)
   #pragma warning(disable: 4355)
#endif


Sequencer::Sequencer(const int startTc,
                     const int stopTc,
                     const Languages & languages,
                     const bool noTimeout,
                     const int contextId,
                     boost::asio::io_service& ioService):
    m_ioService(ioService),
    m_actionSender(ioService),
    m_partnerState(languages,contextId,m_actionSender,boost::bind(&Sequencer::PostTick,this)),
    m_currentCaseNo(startTc),
    m_currentActionNo(0),
    m_stopTc(stopTc),
    m_state(SequencerStates::Created),
    m_lastCleanupTime(boost::posix_time::second_clock::universal_time()),
    m_languages(languages),
    m_noTimeout(noTimeout),
    m_isDumpRequested(false),
    m_contextId(contextId),
    m_testConfig()
{
    // Find out if we are running in standalone or multinode configuration
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


#ifdef _MSC_VER
   #pragma warning(pop)
#endif



bool Sequencer::PrepareTestcaseSetup()
{
    m_currentActionNo=-1;

    while(m_currentCaseNo <= m_stopTc)
    {
        m_currentCase = TestCaseReader::Instance().GetTestCase(m_currentCaseNo);

        if (m_currentCase == NULL)
        {
            //no such testcase, try next one.
            ++m_currentCaseNo;
        }
        else if (!m_currentCase->TestConfig().IsNull() && m_currentCase->TestConfig().GetVal() != m_testConfig)
        {
            std::wcout << "Skipping testcase " << m_currentCaseNo << " since it isn't applicable in "
                       << DoseTest::TestConfigEnum::ToString(m_testConfig) << " mode." << std::endl;
            ++m_currentCaseNo;
        }
        else
        {
            break;
        }
    }
    
    if (m_currentCaseNo > m_stopTc)
    {
        return false;
    }

    std::wcout << "Running testcase: " << m_currentCaseNo << std::endl;

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

    return true;
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
    if (m_currentAction->Partner().IsNull() && m_currentAction->ActionKind() == DoseTest::ActionEnum::Sleep)
    {
        std::wcout << " - Sleeping " << m_currentAction->SleepDuration() << " seconds"<<std::endl;
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

        /*
        if (m_currentAction->ActionKind() == DoseTest::ActionEnum::CheckReferences ||
            m_currentAction->ActionKind() == DoseTest::ActionEnum::CloseAndCheckReferences ||
            m_currentAction->ActionKind() == DoseTest::ActionEnum::RunGarbageCollector)
        {
            // These actions will force the garbage collector to be run by the partner (if
            // the partner is of a GC type) so we wait a while before moving on.

            const unsigned int sleepDuration = 1;
            std::wcout << "Sleeping " << sleepDuration << " seconds" << std::endl;

            boost::this_thread::sleep(boost::posix_time::seconds(sleepDuration));
            }*/
    }
}

void Sequencer::SetState(const SequencerStates::State newState)
{
    //std::wcout << "Changing state from " << SequencerStates::StateNames[m_state] << " to " << SequencerStates::StateNames[newState] << std::endl;
    m_state = newState;
    if (newState == SequencerStates::CleaningUpTestcase)
    {
        m_lastCleanupTime = boost::posix_time::second_clock::universal_time();
    }
}

void Sequencer::Tick()
{
    //std::wcout << "Tick!" << std::endl;
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


    //TODO what to do if a partner becomes not ready?

    const SequencerStates::State oldState = m_state;
    switch (m_state)
    {
    case SequencerStates::Created:
        {
            SetState(SequencerStates::ActivatingPartners);
        }
        break;

    case SequencerStates::ActivatingPartners:
        {
            if (m_partnerState.IsActive())
            {
                SetState(SequencerStates::ResetPartners);
                m_actionSender.Open(m_partnerState.Address(0),m_partnerState.Port(0),
                                    m_partnerState.Address(1),m_partnerState.Port(1),
                                    m_partnerState.Address(2),m_partnerState.Port(2));
            }
        }
        break;

    case SequencerStates::ResetPartners:
        {
            m_partnerState.Reset();
            SetState(SequencerStates::PreparingTestcaseSetup);
        }
        break;

    case SequencerStates::PreparingTestcaseSetup:
        {
            if (m_partnerState.IsReady())
            {
                const bool finished = !PrepareTestcaseSetup();
                if (finished)
                {
                    SetState(SequencerStates::CollectingResults);
                }
                else
                {
                    SetState(SequencerStates::RunningSetupAction);
                }
            }
        }
        break;

    case SequencerStates::RunningSetupAction:
        {
            ++m_currentActionNo;
            if (m_currentCase->TestCaseSetupActions()[m_currentActionNo].IsNull())
            {
                SetState(SequencerStates::PreparingTestcaseExecution);
            }
            else
            {
                m_currentAction=m_currentCase->TestCaseSetupActions()[m_currentActionNo].GetPtr();
                ExecuteCurrentAction();

                PostTick();
            }
        }
        break;
    case SequencerStates::PreparingTestcaseExecution:
        {
            PrepareTestcaseExecution();
            SetState(SequencerStates::RunningTestAction);
        }
        break;
    case SequencerStates::RunningTestAction:
        {
            ++m_currentActionNo;
            if (m_currentCase->TestActions()[m_currentActionNo].IsNull())
            {
                SetState(SequencerStates::CleaningUpTestcase);
            }
            else
            {
                m_currentAction=m_currentCase->TestActions()[m_currentActionNo].GetPtr();
                ExecuteCurrentAction();

                PostTick();
            }
        }
        break;
    case SequencerStates::CleaningUpTestcase:
        {
            boost::this_thread::sleep(boost::posix_time::milliseconds(150));
            SetState(SequencerStates::ResetPartners);
            m_currentCaseNo++;
        }
        break;
    case SequencerStates::CollectingResults:
        {
            if (!m_isDumpRequested)
            {
                GetTestResults(0);
            }
            else if (m_dumpRequestIds.empty())
            {
                m_ioService.stop();
            }
        }
        break;

    default:
        std::wcout << "Have no code for handling this state: " << SequencerStates::StateNames[m_state] << std::endl;
        break;
    }

    if (m_state != oldState)
    {
        PostTick();
    }
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

    PostTick();
}

void Sequencer::PostTick()
{
    m_ioService.post(boost::bind(&Sequencer::Tick,this));
}
