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

#ifndef __DOSE_TEST_SEQUENCER_H__
#define __DOSE_TEST_SEQUENCER_H__

#include <Safir/Dob/Connection.h>
#include "PartnerState.h"
#include <string>
#include <DoseTest/Items/TestCase.h>
#include <boost/noncopyable.hpp>
#include "InjectionTimestampHandler.h"
#include "ActionSender.h"
#include "TestCaseReader.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace SequencerStates
{
    enum State
    {
        Created,
        ActivatingPartners,
        ResetPartners,
        PreparingTestcaseSetup,
        RunningSetupAction,
        PreparingTestcaseExecution,
        RunningTestAction,
        CleaningUpTestcase,
        CollectingResults,
    };


    char const * const StateNames []=
    {
        "Created",
        "ActivatingPartners",
        "ResetPartners",
        "PreparingTestcaseSetup",
        "RunningSetupAction",
        "PreparingTestcaseExecution",
        "RunningTestAction",
        "CleaningUpTestcase",
        "CollectingResults",
    };
}


class Sequencer :
    public Safir::Dob::Requestor,
    private boost::noncopyable
{
public:
    Sequencer(const int startTc,
              const int stopTc,
              const Languages & languages,
              const bool multinode,
              const bool noTimeout,
              const bool endlessMode,
              const int contextId,
              boost::asio::io_service& ioService);


    void FindNextTestcase();
private:
    virtual void OnResponse(const Safir::Dob::ResponseProxy responseProxy);
    virtual void OnNotRequestOverflow() {}

    void PostTick();

    void Tick();

    void SetState(const SequencerStates::State newState);

    bool PrepareTestcaseSetup(); //return false if no more tc to run
    void PrepareTestcaseExecution();

    void ExecuteCurrentAction();

    void GetTestResults(const int fileNumber);


    boost::asio::io_service& m_ioService;
    Safir::Dob::SecondaryConnection m_connection;

    ActionSender m_actionSender;

    PartnerState m_partnerState;

    Safir::Dob::Typesystem::Int32      m_currentCaseNo;
    DoseTest::Items::TestCasePtr       m_currentCase;

    int                   m_currentActionNo;
    DoseTest::ActionPtr   m_currentAction;


    const int m_stopTc;
    const int m_startTc;

    const bool m_endlessMode;

    SequencerStates::State m_state;
    boost::chrono::steady_clock::time_point m_lastCleanupTime;

    const Languages m_languages;
    const bool m_noTimeout;

    std::map<Safir::Dob::RequestId,int> m_dumpRequestIds;

    bool m_isDumpRequested;
    int m_fileNumber;

    const int m_contextId;
    //no need to do anything with this. Constructor sets up everything.
    InjectionTimestampHandler m_injectionTimestampHandler;

    DoseTest::TestConfigEnum::Enumeration m_testConfig;

};

#endif

