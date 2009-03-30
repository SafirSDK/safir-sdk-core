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

#ifndef __DOSE_TEST_SEQUENCER_H__
#define __DOSE_TEST_SEQUENCER_H__

#include "PartnerState.h"
#include <string>
#include <Safir/Dob/Connection.h>
#include <DoseTest/Items/TestCase.h>
#include <iostream>
#include <fstream>
#include <list>
#include <boost/noncopyable.hpp>
#include <map>
#include "TestCaseReader.h"
#include "InjectionTimestampHandler.h"

class Sequencer :
    public Safir::Dob::MessageSender,
    public Safir::Dob::Requestor,
    private boost::noncopyable
{
public:
    Sequencer(const int startTc, const int stopTc, const Languages & languages);
    ~Sequencer();

    void Tick();


    bool IsFinished() const
    {return m_currentCaseNo >= TestCaseReader::Instance().NumberOfTestCases() || m_currentCaseNo > m_stopTc;}

    void GetTestResults(const int fileNumber);
    bool GotTestResults() const;


    bool DeactivateAll();
private:
    virtual void OnNotMessageOverflow();

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

    const int m_stopTc;

    State m_state;

    const Languages m_languages;

    std::map<Safir::Dob::RequestId,int> m_dumpRequestIds;

    bool m_isDumpRequested;
    int m_fileNumber;

    //no need to do anything with this. Constructor sets up everything.
    InjectionTimestampHandler m_injectionTimestampHandler;
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

