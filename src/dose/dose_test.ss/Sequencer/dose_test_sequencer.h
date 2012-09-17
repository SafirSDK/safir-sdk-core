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

#ifndef __DOSE_TEST_SEQUENCER_H__
#define __DOSE_TEST_SEQUENCER_H__

#include <boost/asio.hpp>
#include <Safir/Dob/Connection.h>
#include "PartnerState.h"
#include <string>
#include <DoseTest/Items/TestCase.h>
#include <boost/noncopyable.hpp>
#include "InjectionTimestampHandler.h"

#if 0
#include <DoseTest/TestConfigEnum.h>
#include <iostream>
#include <fstream>
#include <list>

#include <map>
#include "TestCaseReader.h"

#include "ActionSender.h"

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4702)
  #pragma warning (disable : 4127)
#endif
#include <boost/date_time/posix_time/posix_time.hpp>
#if defined _MSC_VER
  #pragma warning (pop)
#endif

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
              const bool noTimeout,
              const int contextId,
              boost::asio::io_service& ioService);

#if 0
    void Tick();


    bool IsFinished() const
    {return m_currentCaseNo >= TestCaseReader::Instance().NumberOfTestCases() || m_currentCaseNo > m_stopTc;}

    void GetTestResults(const int fileNumber);
    bool GotTestResults() const;


    bool DeactivateAll();
#endif
private:
    virtual void OnResponse(const Safir::Dob::ResponseProxy responseProxy);
    virtual void OnNotRequestOverflow() {}

#if 0

    void SetState(const SequencerState newState);

    static bool VerifyAction(DoseTest::ActionPtr Action);

    void PrepareTestcaseSetup();
    void PrepareTestcaseExecution();
    void ExecuteCurrentAction();
#endif
    Safir::Dob::SecondaryConnection m_connection;


    PartnerState m_partnerState;

    Safir::Dob::Typesystem::Int32      m_currentCaseNo;
    DoseTest::Items::TestCasePtr       m_currentCase;

    int                   m_currentActionNo;
    DoseTest::ActionPtr   m_currentAction;
#if 0
    ActionSender m_actionSender;
#endif
    const int m_stopTc;

    SequencerStates::State m_state;
    boost::posix_time::ptime m_lastCleanupTime;

    const Languages m_languages;
    const bool m_noTimeout;

    std::map<Safir::Dob::RequestId,int> m_dumpRequestIds;

    bool m_isDumpRequested;
    int m_fileNumber;

    const int m_contextId;
    //no need to do anything with this. Constructor sets up everything.
    InjectionTimestampHandler m_injectionTimestampHandler;

};

#endif

