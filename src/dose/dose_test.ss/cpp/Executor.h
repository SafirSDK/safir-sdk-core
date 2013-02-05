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

#ifndef __DOSE_TEST_CPP_NODE_H__
#define __DOSE_TEST_CPP_NODE_H__

#include <boost/noncopyable.hpp>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Internal/Atomic.h>
#include <DoseTest/Action.h>
#include <DoseTest/Sequencer.h>
#include <Safir/Dob/ErrorResponse.h>
#include "Consumer.h"
#include <boost/function.hpp>
#include <boost/asio.hpp>
#include <boost/bind.hpp>
#include "ActionReceiver.h"

class Dispatcher:
    public Safir::Dob::Dispatcher,
    private boost::noncopyable
{
public:
    Dispatcher(const boost::function<void(void)> & dispatchCallback,
               boost::asio::io_service & ioService)
        : m_dispatchCallback(dispatchCallback)
        , m_isNotified(0)
        , m_ioService(ioService)
    {}

private:
    virtual void OnDoDispatch()
    {
        if (m_isNotified == 0)
        {
            m_isNotified = 1;
            m_ioService.post(boost::bind(&Dispatcher::Dispatch,this));
        }
    }
    virtual void Dispatch()
    {
        m_isNotified = 0;
        m_dispatchCallback();
    }

    const boost::function<void(void)> m_dispatchCallback;
    Safir::Dob::Internal::AtomicUint32 m_isNotified;
    boost::asio::io_service & m_ioService;
};


class Executor:
    public Safir::Dob::StopHandler,
    public Safir::Dob::EntityHandler,
    public Safir::Dob::EntitySubscriber,
    public Safir::Dob::ServiceHandler,
    private boost::noncopyable
{
public:
    Executor(const std::vector<std::string> & commandLine);


    void Run();

private:

    virtual void OnStopOrder();

    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                       const Safir::Dob::Typesystem::HandlerId& handlerId);

    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender)
    {responseSender->Send(Safir::Dob::ErrorResponse::Create());}

    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender)
    {responseSender->Send(Safir::Dob::ErrorResponse::Create());}

    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender)
    {responseSender->Send(Safir::Dob::ErrorResponse::Create());}

    virtual void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                  Safir::Dob::ResponseSenderPtr         responseSender);
    
    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
    {HandleSequencerState(boost::static_pointer_cast<DoseTest::Sequencer>(entityProxy.GetEntity()));}
 
    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
    {HandleSequencerState(boost::static_pointer_cast<DoseTest::Sequencer>(entityProxy.GetEntity()));}
    
    //ignore deletes since they may be due to an inhibitoutgoingtraffic on the other side
    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy,
                                 const bool) {}
    

    void HandleSequencerState(const DoseTest::SequencerPtr& sequencer);

    void HandleAction(DoseTest::ActionPtr action);

    void ExecuteAction(DoseTest::ActionPtr action);
    void AddCallbackAction(DoseTest::ActionPtr action);
    void ExecuteCallbackActions(const Safir::Dob::CallbackId::Enumeration callback);

    void DispatchControlConnection();
    void DispatchTestConnection();

    const std::wstring m_identifier;
    const int m_instance;
    const std::wstring m_instanceString;
    const std::wstring m_controlConnectionName;
    const std::wstring m_testConnectionName;

    boost::asio::io_service m_ioService;

    const Safir::Dob::Typesystem::EntityId m_partnerEntityId;

    bool m_isDone;
    bool m_isActive;

    std::vector<boost::shared_ptr<Consumer> > m_consumers;

    Safir::Dob::Connection m_controlConnection;
    Safir::Dob::Connection m_testConnection;
    bool m_dispatchTestConnection;

    Dispatcher m_testDispatcher;
    Dispatcher m_controlDispatcher;

    ActionReceiver m_actionReceiver;

    typedef std::vector<DoseTest::ActionPtr> Actions;
    typedef std::vector<Actions> CallbackActions;
    std::vector<std::vector<DoseTest::ActionPtr> > m_callbackActions;

    int m_defaultContext;
};

typedef boost::shared_ptr<Executor> ExecutorPtr;

#endif
