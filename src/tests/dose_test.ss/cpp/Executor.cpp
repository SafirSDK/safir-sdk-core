/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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

#include "Executor.h"
#include <iostream>

#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <DoseTest/Partner.h>
#include "Logger.h"
#include <DoseTest/Dump.h>
#include <DoseTest/DumpResult.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/NodeInfo.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/NotFoundException.h>

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4702)
  #pragma warning(disable: 4100)
#endif

#include <boost/lexical_cast.hpp>
#include <boost/thread.hpp>
#include <algorithm>
#include <chrono>
#include <cstdint>

using namespace std::placeholders;

Executor::Executor(const std::vector<std::string> & commandLine):
    m_identifier(L"cpp"),
    m_instance(boost::lexical_cast<int>(commandLine.at(1))),
    m_instanceString(boost::lexical_cast<std::wstring>(m_instance)),
    m_controlConnectionName(m_identifier + L"_control"),
    m_testConnectionName(L"partner_test_connection"),
    m_partnerEntityId(DoseTest::Partner::ClassTypeId,Safir::Dob::Typesystem::InstanceId(m_instance)),
    m_isDone(false),
    m_isActive(false),
    m_dispatchTestConnection(true),
    m_testDispatcher([this]{DispatchTestConnection();}, m_ioContext),
    m_controlDispatcher([this]{DispatchControlConnection();}, m_ioContext),
    m_actionReceiver(m_ioContext, [this](const auto& action){HandleAction(action);},m_instance),
    m_callbackCounts(Safir::Dob::CallbackId::Size(), 0),
    m_isWaitingForCallback(false),
    m_waitingForCallback(Safir::Dob::CallbackId::OnDoDispatch),
    m_waitForCallbackTimer(m_ioContext),
    m_callbackActions(Safir::Dob::CallbackId::Size()),
    m_defaultContext(0)
{
    m_controlConnection.Open(m_controlConnectionName, m_instanceString, 0, this, &m_controlDispatcher);

    m_controlConnection.SubscribeEntity(DoseTest::Sequencer::ClassTypeId,this);
}
#ifdef _MSC_VER
#  pragma warning(pop)
#endif

void Executor::OnStopOrder()
{
    lout << "Got stop order" << std::endl;
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnStopOrder);
    m_isDone = true;
    m_ioContext.stop();
}

void Executor::HandleAction(DoseTest::ActionPtr action)

{
    if (!action->Partner().IsNull() && action->Partner().GetVal() != Safir::Dob::Typesystem::ChannelId(m_instance))
    {
        // Not meant for this partner
        std::wcout << "Got action that was not meant for this partner!" << std::endl;
        return;
    }

    if (action->Consumer().IsNull())
    {//No consumer set, meant for the executor.
        std::wcout << "No consumer set, meant for the executor" << std::endl;

        if (action->ActionCallback().IsNull()) //it is a normal action
        {
            std::wcout << "Calling ExecuteAction" << std::endl;
            ExecuteAction(action);
        }
        else if (m_isActive)
        {
            std::wcout << "Calling AddCallbackAction" << std::endl;
            AddCallbackAction(action);
        }
    }
    else if (m_isActive)
    {
        std::wcout << "Meant for consumer " << action->Consumer() << std::endl;
        Consumer & theConsumer = *m_consumers.at(action->Consumer().GetVal());

        if (action->ActionCallback().IsNull()) //it is a normal action
        {
            std::wcout << "Calling ExecuteAction" << std::endl;
            theConsumer.ExecuteAction(action);
        }
        else
        {
            std::wcout << "Calling AddCallbackAction" << std::endl;
            theConsumer.AddCallbackAction(action);
        }
    }

}

void
Executor::ExecuteAction(DoseTest::ActionPtr action)
{
    std::wcout << "In ExecuteAction" << std::endl;
    switch (action->ActionKind().GetVal())
    {
    case DoseTest::ActionEnum::Reset:
        {
            std::wcout << "Performing Reset" << std::endl;
            if (m_isActive)
            {
                std::wcout << "Calling Close" << std::endl;
                m_testConnection.Close();

                std::wcout << "Calling Open" << std::endl;
                m_testConnection.Open(m_testConnectionName,m_instanceString,m_defaultContext,NULL,&m_testDispatcher);
                std::wcout << "Open completed" << std::endl;

                DoseTest::PartnerPtr partner =
                    std::static_pointer_cast<DoseTest::Partner>
                    (m_controlConnection.Read(m_partnerEntityId).GetEntity());
                if (partner->Incarnation().IsNull())
                {
                    partner->Incarnation() = 0;
                }
                else
                {
                    ++partner->Incarnation();
                }
                std::wcout << "Calling SetChanges" << std::endl;
                m_controlConnection.SetChanges(partner,
                                               m_partnerEntityId.GetInstanceId(),
                                               Safir::Dob::Typesystem::HandlerId(m_instance));

                std::wcout << "Creating new consumers" << std::endl;
                std::vector<ConsumerPtr> newConsumers;
                for (int i = 0; i < 3; ++i)
                {
                    newConsumers.push_back(ConsumerPtr(new Consumer(i,
                                                                    m_testConnectionName,
                                                                    m_instanceString,
                                                                    [this](const auto callback){NotifyCallback(callback);})));
                }
                std::wcout << "Swapping consumers" << std::endl;
                m_consumers.swap(newConsumers);

                std::wcout << "Deleting old consumers" << std::endl;
                newConsumers.clear();

                std::wcout << "Clearing callback actions" << std::endl;
                std::for_each(m_callbackActions.begin(),m_callbackActions.end(),
                              [](auto& action){action.clear();});

                ResetCallbackCounts();
            }
            std::wcout << "Reset complete" << std::endl;
        }
        break;

    case DoseTest::ActionEnum::Open:
        {
            if (m_isActive)
            {
                Safir::Dob::Typesystem::Int32 context = m_defaultContext;
                if (!action->Context().IsNull())
                {
                    context = action->Context();
                }
                std::wstring connName = m_testConnectionName;
                if (!action->ConnectionName().IsNull())
                {
                    connName = action->ConnectionName();
                }
                m_testConnection.Open(connName,m_instanceString,context,NULL,&m_testDispatcher);
            }
        }
        break;

    case DoseTest::ActionEnum::Close:
        {
            if (m_isActive)
            {
                m_testConnection.Close();
            }
        }
        break;

    case DoseTest::ActionEnum::InhibitDispatch:
        {
            if (m_isActive)
            {
                m_dispatchTestConnection = !action->Inhibit().GetVal();
                lout << "InhibitDispatch set to " << m_dispatchTestConnection << std::endl;
            }
        }
        break;

    case DoseTest::ActionEnum::Print:
        {
            if (m_isActive)
            {
                lout << action->PrintString().GetVal() << std::endl;
            }
        }
        break;

    case DoseTest::ActionEnum::ResetCallbackActions:
        {
            std::for_each(m_callbackActions.begin(),m_callbackActions.end(),
                          [](auto& action){action.clear();});
        }
        break;

    case DoseTest::ActionEnum::WaitForCallback:
        {
            BeginWaitForCallback(action);
        }
        break;

    case DoseTest::ActionEnum::Sleep:
        {
            if (m_isActive)
            {
                std::wcout << "Sleeping " << action->SleepDuration() << " seconds"<<std::endl;
                boost::this_thread::sleep_for(boost::chrono::microseconds
                                          (static_cast<std::int64_t>(action->SleepDuration() * 1e6)));
            }
        }
        break;

    case DoseTest::ActionEnum::CheckReferences:
    case DoseTest::ActionEnum::CloseAndCheckReferences:
    case DoseTest::ActionEnum::RunGarbageCollector:
        {
            // These actions are for garbage collected languages only.
        }
        break;

    default:
        {
            lout << "Got unexpected action " << DoseTest::ActionEnum::ToString(action->ActionKind().GetVal())<<std::endl;
        }
    }

    std::wcout << "Leaving ExecuteAction" << std::endl;
}
void
Executor::AddCallbackAction(DoseTest::ActionPtr action)
{
    m_callbackActions[action->ActionCallback().GetVal()].push_back(action);
}


void
Executor::ExecuteCallbackActions(const Safir::Dob::CallbackId::Enumeration callback)
{
    NotifyCallback(callback);

    for (Actions::iterator it = m_callbackActions[callback].begin();
         it != m_callbackActions[callback].end(); ++it)
    {
        ExecuteAction(*it);
    }
}


void Executor::NotifyCallback(const Safir::Dob::CallbackId::Enumeration callback)
{
    ++m_callbackCounts.at(callback);

    if (m_isWaitingForCallback && callback == m_waitingForCallback)
    {
        EndWaitForCallback(L"callback arrived");
    }
}


void Executor::BeginWaitForCallback(const DoseTest::ActionPtr& action)
{
    if (!m_isActive)
    {
        //An inactive partner has no consumers to deliver anything to, so waiting
        //could only ever time out.
        std::wcout << "WaitForCallback: partner is not active, not waiting" << std::endl;
        m_actionReceiver.SendAck();
        return;
    }

    if (action->WaitForCallbackId().IsNull())
    {
        std::wcout << "WaitForCallback action without a WaitForCallbackId!" << std::endl;
        m_actionReceiver.SendAck();
        return;
    }

    const Safir::Dob::CallbackId::Enumeration callback = action->WaitForCallbackId().GetVal();

    //If it has already happened we are done, and the testcase does not pay for the
    //wait at all. Without this the wait would be a race: the callback that the
    //sequencer is asking us to wait for has usually arrived before the sequencer
    //gets round to sending the wait, and we would then sit here until the timeout
    //waiting for a second one that is never coming.
    if (m_callbackCounts.at(callback) > 0)
    {
        std::wcout << "WaitForCallback: " << Safir::Dob::CallbackId::ToString(callback)
             << " has already happened " << m_callbackCounts.at(callback)
             << " time(s) in this testcase, not waiting" << std::endl;
        m_actionReceiver.SendAck();
        return;
    }

    //Default to a minute if the testcase did not say. Long, because the point of
    //the timeout is only to turn a hang into an ordinary test failure - if we are
    //waiting this long the testcase has already failed, we are just deciding how
    //long to take about admitting it.
    const double timeout = action->WaitForCallbackTimeout().IsNull()
        ? 60.0
        : action->WaitForCallbackTimeout().GetVal();

    std::wcout << "WaitForCallback: waiting up to " << timeout << " seconds for "
         << Safir::Dob::CallbackId::ToString(callback) << std::endl;

    m_isWaitingForCallback = true;
    m_waitingForCallback = callback;

    m_waitForCallbackTimer.expires_after
        (std::chrono::milliseconds(static_cast<std::int64_t>(timeout * 1000)));
    m_waitForCallbackTimer.async_wait([this](const boost::system::error_code& error)
    {
        if (!error && m_isWaitingForCallback)
        {
            EndWaitForCallback(L"TIMED OUT");
        }
    });
}


void Executor::EndWaitForCallback(const std::wstring& reason)
{
    std::wcout << "WaitForCallback: done waiting for "
         << Safir::Dob::CallbackId::ToString(m_waitingForCallback)
         << " (" << reason << ")" << std::endl;

    m_isWaitingForCallback = false;
    m_waitForCallbackTimer.cancel();

    //Acknowledge either way. A wait that gives up must not hold up the sequencer:
    //the missing callback will show up as missing output in the testcase diff,
    //which is a test failure rather than a hung run.
    m_actionReceiver.SendAck();
}


void Executor::ResetCallbackCounts()
{
    if (m_isWaitingForCallback)
    {
        //Cannot normally happen: the sequencer is blocked on the acknowledgement we
        //are withholding, so it cannot have sent us a Reset.
        std::wcout << "WaitForCallback: Reset while still waiting!" << std::endl;
        EndWaitForCallback(L"Reset");
    }

    std::fill(m_callbackCounts.begin(), m_callbackCounts.end(), 0);
}


void Executor::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     /*typeId*/,
                                     const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
{
    if (m_isActive)
    {
        lout << "Deactivating" << std::endl;
        m_testConnection.Close();
        m_controlConnection.UnregisterHandler(m_partnerEntityId.GetTypeId(), Safir::Dob::Typesystem::HandlerId(m_instance));
        m_controlConnection.UnregisterHandler(DoseTest::Dump::ClassTypeId,Safir::Dob::Typesystem::HandlerId(m_instance));
        m_isActive = false;
    }
}

void Executor::DispatchControlConnection()
{
    try
    {
        m_controlConnection.Dispatch();
    }
    catch (const Safir::Dob::Typesystem::Exception & exc)
    {
        lout << "Caught Exception when Dispatching controlConnection: " << exc.GetName() << std::endl;
        std::wcout << "Exception info: " << exc.GetExceptionInfo() << std::endl;
    }
    catch (const Safir::Dob::Typesystem::FundamentalException & exc)
    {
        lout << "Caught FundamentalException when Dispatching controlConnection: " << exc.GetName() << std::endl;
        std::wcout << "Exception info: " << exc.GetExceptionInfo() << std::endl;
    }
}

void Executor::DispatchTestConnection()
{
    if (m_isActive && m_dispatchTestConnection)
    {
        try
        {
            ExecuteCallbackActions(Safir::Dob::CallbackId::OnDoDispatch);
            std::for_each(m_consumers.begin(),m_consumers.end(),
                          [](const auto& consumer)
                          {consumer->ExecuteCallbackActions(Safir::Dob::CallbackId::OnDoDispatch);});

            m_testConnection.Dispatch();
        }
        catch (const Safir::Dob::Typesystem::Exception & exc)
        {
            lout << "Caught Exception when Dispatching testConnection: " << exc.GetName() << std::endl;
            std::wcout << "Exception info: " << exc.GetExceptionInfo() << std::endl;
        }
        catch (const Safir::Dob::Typesystem::FundamentalException & exc)
        {
            lout << "Caught FundamentalException when Dispatching testConnection: " << exc.GetName() << std::endl;
            std::wcout << "Exception info: " << exc.GetExceptionInfo() << std::endl;
        }
    }

}

void
Executor::Run()
{
    std::wcout << m_identifier << ":" <<  m_instance << " Started" <<std::endl;

    auto keepRunning = boost::asio::make_work_guard(m_ioContext);
    m_ioContext.run();

    m_testConnection.Close();
    m_controlConnection.Close();
}

void
Executor::OnServiceRequest(const Safir::Dob::ServiceRequestProxy /*serviceRequestProxy*/,
                           Safir::Dob::ResponseSenderPtr         responseSender)
{
    DoseTest::DumpResultPtr result = DoseTest::DumpResult::Create();
    result->Result().SetVal(lout.Dump());
    responseSender->Send(result);
}

void Executor::HandleSequencerState(const DoseTest::SequencerPtr& sequencer)
{
    const bool activate = sequencer != NULL && sequencer->Partners()[m_instance].GetVal() == m_identifier;

    if (activate == m_isActive)
    {
        //already active or not active
        return;
    }

    if (activate)
    {
        m_defaultContext = sequencer->Context();
        std::wcout << "Activating (default context is " << m_defaultContext << ")" << std::endl;
        m_controlConnection.RegisterEntityHandler(m_partnerEntityId.GetTypeId(),
                                                  Safir::Dob::Typesystem::HandlerId(m_instance),
                                                  Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                                  this);
        m_controlConnection.RegisterServiceHandler(DoseTest::Dump::ClassTypeId,
                                                   Safir::Dob::Typesystem::HandlerId(m_instance),this);

        m_actionReceiver.Open();

        DoseTest::PartnerPtr partner = DoseTest::Partner::Create();
        partner->Identifier() = m_identifier;
        partner->Port() = m_actionReceiver.Port();

        {
            using namespace Safir::Dob;
            using namespace Safir::Dob::Typesystem;
            const auto nodeId = ConnectionAspectMisc(m_controlConnection).GetNodeId();

            // Wait for NodeInfo to be available
            for(;;)
            {
                try
                {
                    partner->Address() = std::static_pointer_cast<NodeInfo>
                                         (m_controlConnection.Read(EntityId(NodeInfo::ClassTypeId,
                                                                            InstanceId(nodeId))).
                                          GetEntity())->IpAddress();
                    break;
                }
                catch (const Safir::Dob::NotFoundException&)
                {
                    boost::this_thread::sleep_for(boost::chrono::milliseconds(100));
                }
            }
        }

        m_controlConnection.SetAll(partner, m_partnerEntityId.GetInstanceId(),
                                   Safir::Dob::Typesystem::HandlerId(m_instance));
        m_isActive = true;
    }
    else
    {
        std::wcout << "Deactivating" << std::endl;
        m_actionReceiver.Close();

        m_testConnection.Close();

        m_controlConnection.Delete(m_partnerEntityId, Safir::Dob::Typesystem::HandlerId(m_instance));
        m_controlConnection.UnregisterHandler(m_partnerEntityId.GetTypeId(),Safir::Dob::Typesystem::HandlerId(m_instance));

        m_controlConnection.UnregisterHandler(DoseTest::Dump::ClassTypeId,Safir::Dob::Typesystem::HandlerId(m_instance));
        m_isActive = false;
        lout.Clear();
    }
}
