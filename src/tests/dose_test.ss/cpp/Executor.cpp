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

#include "Executor.h"
#include <iostream>

#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <DoseTest/Partner.h>
#include <boost/bind.hpp>
#include <boost/thread.hpp>
#include "Logger.h"
#include <DoseTest/Dump.h>
#include <DoseTest/DumpResult.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/NodeInfo.h>
#include <Safir/Dob/ThisNodeParameters.h>
#if 0 //stewart
#include "dosecom_stuff.h"
#endif


#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4702)
#endif

#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif

//dosecom_stuff.h includes stuff that includes windows.h crap header file!
//undef the stupid defines from there.
#ifdef GetMessage
#undef GetMessage
#endif

#ifdef _MSC_VER
#  pragma warning(push)
#  pragma warning(disable: 4355)
#endif

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
    m_testDispatcher(boost::bind(&Executor::DispatchTestConnection,this), m_ioService),
    m_controlDispatcher(boost::bind(&Executor::DispatchControlConnection,this), m_ioService),
    m_actionReceiver(m_ioService, boost::bind(&Executor::HandleAction,this,_1),m_instance),
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
    m_ioService.stop();
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
                    boost::static_pointer_cast<DoseTest::Partner>
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
                std::vector<boost::shared_ptr<Consumer> > newConsumers;
                for (int i = 0; i < 3; ++i)
                {
                    newConsumers.push_back(ConsumerPtr(new Consumer(i,m_testConnectionName,m_instanceString)));
                }
                std::wcout << "Swapping consumers" << std::endl;
                m_consumers.swap(newConsumers);

                std::wcout << "Deleting old consumers" << std::endl;
                newConsumers.clear();

                std::wcout << "Clearing callback actions" << std::endl;
                std::for_each(m_callbackActions.begin(),m_callbackActions.end(),boost::bind(&Actions::clear,_1));
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

    case DoseTest::ActionEnum::InhibitOutgoingTraffic:
        {
            if (m_isActive)
            {
#if 0 //stewart
                DOSE_SHARED_DATA_S * pShm = (DOSE_SHARED_DATA_S *) Get_NodeSharedData_Pointer();

                pShm->InhibitOutgoingTraffic = action->Inhibit().GetVal();
                lout << "InhibitOutgoingTraffic set to " << pShm->InhibitOutgoingTraffic << std::endl;
#endif
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
            std::for_each(m_callbackActions.begin(),m_callbackActions.end(),boost::bind(&Actions::clear,_1));
        }
        break;

    case DoseTest::ActionEnum::Sleep:
        {
            if (m_isActive)
            {
                std::wcout << "Sleeping " << action->SleepDuration() << " seconds"<<std::endl;
                boost::this_thread::sleep_for(boost::chrono::microseconds
                                          (static_cast<boost::int64_t>(action->SleepDuration() * 1e6)));
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
    for (Actions::iterator it = m_callbackActions[callback].begin();
         it != m_callbackActions[callback].end(); ++it)
    {
        ExecuteAction(*it);
    }
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
            std::for_each(m_consumers.begin(),m_consumers.end(), boost::bind(&Consumer::ExecuteCallbackActions,_1,Safir::Dob::CallbackId::OnDoDispatch));

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

    boost::asio::io_service::work keepRunning(m_ioService);
    m_ioService.run();

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
            partner->Address() = boost::static_pointer_cast<NodeInfo>
                (m_controlConnection.Read(EntityId(NodeInfo::ClassTypeId,
                                                   InstanceId(nodeId))).
                 GetEntity())->IpAddress();
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
