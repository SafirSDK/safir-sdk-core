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

#include "PartnerState.h"
#include <string>
#include <Safir/Dob/Connection.h>
#include <DoseTest/Items/TestCase.h>
#include <DoseTest/TestConfigEnum.h>
#include <iostream>
#include <fstream>
#include <list>
#include <boost/noncopyable.hpp>
#include <map>
#include "TestCaseReader.h"
#include "InjectionTimestampHandler.h"
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
              const std::string& multicastNic);
    ~Sequencer();

    void Tick();


    bool IsFinished() const
    {return m_currentCaseNo >= TestCaseReader::Instance().NumberOfTestCases() || m_currentCaseNo > m_stopTc;}

    void GetTestResults(const int fileNumber);
    bool GotTestResults() const;


    bool DeactivateAll();
private:
    virtual void OnResponse(const Safir::Dob::ResponseProxy responseProxy);
    virtual void OnNotRequestOverflow() {}

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

    void SetState(const State newState);

    static bool VerifyAction(DoseTest::ActionPtr Action);

    void PrepareTestcaseSetup();
    void PrepareTestcaseExecution();
    void ExecuteCurrentAction();

    Safir::Dob::SecondaryConnection m_connection;

    PartnerState m_partnerState;

    Safir::Dob::Typesystem::Int32      m_currentCaseNo;
    DoseTest::Items::TestCasePtr       m_currentCase;

    int                   m_currentActionNo;
    DoseTest::ActionPtr   m_currentAction;

    ActionSender m_actionSender;

    const int m_stopTc;

    State m_state;
    boost::posix_time::ptime m_lastCleanupTime;

    const Languages m_languages;
    const bool m_noTimeout;

    std::map<Safir::Dob::RequestId,int> m_dumpRequestIds;

    bool m_isDumpRequested;
    int m_fileNumber;

    //no need to do anything with this. Constructor sets up everything.
    InjectionTimestampHandler m_injectionTimestampHandler;

    int m_contextId;

    DoseTest::TestConfigEnum::Enumeration m_testConfig;
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

#endif

