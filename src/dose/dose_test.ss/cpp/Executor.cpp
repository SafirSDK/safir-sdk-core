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

#include "Executor.h"
#include <iostream>

#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <DoseTest/Partner.h>
#include <DoseTest/Parameters.h>
#include <boost/bind.hpp>
#include "Logger.h"
#include <DoseTest/Dump.h>
#include <DoseTest/DumpResult.h>
#include <Safir/Dob/OverflowException.h>
#include <ace/OS_NS_unistd.h>
#include <ace/INET_Addr.h>
#include <ace/OS_NS_sys_socket.h>

#include "dosecom_stuff.h"

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4702)
#endif

#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif


#ifdef GetMessage
#undef GetMessage
#endif

ActionReader::ActionReader(const boost::function<void (DoseTest::ActionPtr)> & handleActionCallback,
                           const std::string& multicastNic):

    ACE_Event_Handler(ACE_Reactor::instance()),
    m_handleActionCallback(handleActionCallback),
    m_sock(ACE_SOCK_Dgram_Mcast::options(ACE_SOCK_Dgram_Mcast::DEFOPT_BINDADDR |
                                         ACE_SOCK_Dgram_Mcast::OPT_NULLIFACE_ONE))
{
    // Port and address must correspond to what is used by the test sequencer
    const unsigned int cPort = 31789;
    const std::wstring cMulticastAddr = DoseTest::Parameters::TestMulticastAddress();

    ACE_INET_Addr addr;
    addr.set(cPort,
             cMulticastAddr.c_str());

    ACE_OS::socket_init();

    std::wcout << "Joined socket to group for multicast reception. Multicast address " << cMulticastAddr << ", port " << cPort << "." << std::endl;

    int res;
    if (multicastNic.empty())
    {
        std::wcout << "NIC is not set ... will listen on default interface." << std::endl;
        res = m_sock.join(addr);
    }
    else
    {
        std::wcout << "Used NIC: " << Safir::Dob::Typesystem::Utilities::ToWstring(multicastNic) << std::endl;
        res = m_sock.join(addr, 1, multicastNic.c_str());
    }
    if (res == -1)
    {
        std::wcout << "Error joining multicast group!" << std::endl;
    }

    m_sock.enable(ACE_NONBLOCK);

    reactor ()->register_handler (this, ACE_Event_Handler::READ_MASK); 
}

int ActionReader::handle_input(ACE_HANDLE)
{
    ACE_INET_Addr from;
    const unsigned int cBufLen = 65000; 
    char buf[cBufLen];

    while (m_sock.recv(buf, cBufLen, from) != -1)
    {
        DoseTest::ActionPtr action =
            boost::static_pointer_cast<DoseTest::Action>(Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(buf));

        m_handleActionCallback(action);
    }
    return 0;
}

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4355)
#endif
Executor::Executor(const std::vector<std::string> & commandLine):
    m_identifier(L"cpp"),
    m_instance(boost::lexical_cast<int>(commandLine.at(1))),
    m_instanceString(boost::lexical_cast<std::wstring>(m_instance)),
    m_controlConnectionName(m_identifier + L"_control"),
    m_testConnectionName(L"partner_test_connection"),
    m_partnerEntityId(DoseTest::Partner::ClassTypeId,Safir::Dob::Typesystem::InstanceId(m_instance)),
    m_isActive(false),
    m_dispatchTestConnection(true),
    m_testDispatcher(boost::bind(&Executor::DispatchTestConnection,this)),
    m_controlDispatcher(boost::bind(&Executor::DispatchControlConnection,this)),
    m_actionReader(boost::bind(&Executor::HandleAction,this,_1), commandLine.size() > 2 ? commandLine.at(2): ""),
    m_callbackActions(Safir::Dob::CallbackId::Size()),
    m_defaultContext(0),
    m_lastRecSeqNbr(0)
{
    m_controlConnection.Open(m_controlConnectionName, m_instanceString, 0, this, &m_controlDispatcher);

    //subscribe to messages going to everyone and to me.
    m_controlConnection.SubscribeMessage(DoseTest::Action::ClassTypeId, Safir::Dob::Typesystem::ChannelId(m_instance),this);
    m_controlConnection.SubscribeMessage(DoseTest::Action::ClassTypeId, Safir::Dob::Typesystem::ChannelId(),this);

}
#ifdef _MSC_VER
  #pragma warning(pop)
#endif

void Executor::OnStopOrder()
{
    lout << "Got stop order" << std::endl;
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnStopOrder);
    ACE_Reactor::instance()->end_reactor_event_loop();
}

void Executor::OnMessage(const Safir::Dob::MessageProxy messageProxy)
{
    ExecuteCallbackActions(Safir::Dob::CallbackId::OnMessage);

    DoseTest::ActionPtr action = boost::static_pointer_cast<DoseTest::Action>(messageProxy.GetMessage());

    HandleAction(action);
}

void Executor::HandleAction(DoseTest::ActionPtr action)

{
    if (!action->SeqNbr().IsNull())
    {
        if (action->SeqNbr().GetVal() != m_lastRecSeqNbr + 1)
        {
            std::wcout << "Seems an action from the sequencer is lost!!" << std::endl; 
        }
        m_lastRecSeqNbr = action->SeqNbr().GetVal();
    }

    if (!action->Partner().IsNull() && action->Partner().GetVal() != Safir::Dob::Typesystem::ChannelId(m_instance))
    {
        // Not meant for this partner
        return;
    }

    if (action->Consumer().IsNull())
    {//No consumer set, meant for the executor.
        if (action->ActionCallback().IsNull()) //it is a normal action
        {
            ExecuteAction(action);
        }
        else if (m_isActive)
        {
            AddCallbackAction(action);
        }
    }
    else if (m_isActive)
    {
        Consumer & theConsumer = *m_consumers.at(action->Consumer().GetVal());

        if (action->ActionCallback().IsNull()) //it is a normal action
        {
            theConsumer.ExecuteAction(action);
        }
        else
        {
            theConsumer.AddCallbackAction(action);
        }
    }

}

void
Executor::ExecuteAction(DoseTest::ActionPtr action)
{
    switch (action->ActionKind().GetVal())
    {
    case DoseTest::ActionEnum::Activate:
        {
            if (action->Identifier() == m_identifier)
            {
                m_defaultContext = action->Context().GetVal();
                std::wcout << "Activating (default context is " << m_defaultContext << ")" << std::endl;
                if (!m_isActive)
                {
                    m_controlConnection.RegisterEntityHandler(m_partnerEntityId.GetTypeId(),
                                                              Safir::Dob::Typesystem::HandlerId(m_instance),
                                                              Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                                              this);
                    m_controlConnection.RegisterServiceHandler(DoseTest::Dump::ClassTypeId,
                        Safir::Dob::Typesystem::HandlerId(m_instance),this);
                }
                DoseTest::PartnerPtr partner = DoseTest::Partner::Create();
                partner->Incarnation() = 0;
                partner->Identifier() = m_identifier;
                m_controlConnection.SetAll(partner, m_partnerEntityId.GetInstanceId(),
                                           Safir::Dob::Typesystem::HandlerId(m_instance));
                m_isActive = true;
            }
        }
        break;

    case DoseTest::ActionEnum::Deactivate:
        {
            if (action->Identifier() == m_identifier)
            {
                std::wcout << "Deactivating" << std::endl;
                m_testConnection.Close();

                m_controlConnection.Delete(m_partnerEntityId, Safir::Dob::Typesystem::HandlerId(m_instance));
                m_controlConnection.UnregisterHandler(m_partnerEntityId.GetTypeId(),Safir::Dob::Typesystem::HandlerId(m_instance));

                m_controlConnection.UnregisterHandler(DoseTest::Dump::ClassTypeId,Safir::Dob::Typesystem::HandlerId(m_instance));
                m_isActive = false;
            }
        }
        break;

    case DoseTest::ActionEnum::Reset:
        {
            if (m_isActive)
            {
                m_testConnection.Close();
                m_testConnection.Open(m_testConnectionName,m_instanceString,m_defaultContext,NULL,&m_testDispatcher);

                DoseTest::PartnerPtr partner =
                    boost::static_pointer_cast<DoseTest::Partner>
                    (m_controlConnection.Read(m_partnerEntityId).GetEntity());
                ++partner->Incarnation();
                m_controlConnection.SetChanges(partner,m_partnerEntityId.GetInstanceId(),
                    Safir::Dob::Typesystem::HandlerId(m_instance));

                std::vector<boost::shared_ptr<Consumer> > newConsumers;
                for (int i = 0; i < 3; ++i)
                {
                    newConsumers.push_back(ConsumerPtr(new Consumer(i,m_testConnectionName,m_instanceString)));
                }

                m_consumers.swap(newConsumers);

                std::for_each(m_callbackActions.begin(),m_callbackActions.end(),boost::bind(&Actions::clear,_1));
            }
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
                DOSE_SHARED_DATA_S * pShm = (DOSE_SHARED_DATA_S *) Get_NodeSharedData_Pointer();
                
                pShm->InhibitOutgoingTraffic = action->Inhibit().GetVal();
                lout << "InhibitOutgoingTraffic set to " << pShm->InhibitOutgoingTraffic << std::endl;
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
                std::wcout << "Sleeping " << action->SleepDuration().GetVal() << " seconds"<<std::endl;
                const double decimals = action->SleepDuration() - static_cast<unsigned int>(action->SleepDuration());

                const ACE_Time_Value time(static_cast<unsigned int>(action->SleepDuration()),
                                          static_cast<unsigned int>(decimals * 1000000));
                ACE_OS::sleep(time);
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

    ACE_Reactor::instance()->run_reactor_event_loop();

    ACE_Reactor::instance()->close();
}

void
Executor::OnServiceRequest(const Safir::Dob::ServiceRequestProxy /*serviceRequestProxy*/,
                           Safir::Dob::ResponseSenderPtr         responseSender)
{
    DoseTest::DumpResultPtr result = DoseTest::DumpResult::Create();
    result->Result().SetVal(lout.Dump());
    responseSender->Send(result);
}
