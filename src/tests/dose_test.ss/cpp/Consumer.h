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

#ifndef __DOSE_TEST_CPP_LOGGER_H__
#define __DOSE_TEST_CPP_LOGGER_H__

#include <Safir/Dob/Connection.h>
#include <Safir/Application/Backdoor.h>
#include <Safir/Application/BackdoorKeeper.h>
#include <DoseTest/Action.h>
#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>
#include <map>

class Consumer:
    public Safir::Dob::MessageSubscriber,
    public Safir::Dob::MessageSender,
    public Safir::Dob::RegistrationSubscriber,
    public Safir::Dob::EntitySubscriber,
    public Safir::Dob::EntityHandler,
    public Safir::Dob::EntityHandlerInjection,
    public Safir::Dob::EntityHandlerPending,
    public Safir::Dob::ServiceHandler,
    public Safir::Dob::ServiceHandlerPending,
    public Safir::Dob::Requestor,
    public Safir::Application::Backdoor,
    private boost::noncopyable
{
public:
    Consumer(const int consumerNumber,
             const std::wstring & connectionName,
             const std::wstring & instance);

    void ExecuteAction(DoseTest::ActionPtr action);
    void AddCallbackAction(DoseTest::ActionPtr action);
    void ExecuteCallbackActions(const Safir::Dob::CallbackId::Enumeration callback);

private:

    Safir::Dob::Typesystem::Int64 GetTimestamp(const DoseTest::ActionPtr& action);

    //Consumer functions
    virtual void OnMessage(const Safir::Dob::MessageProxy messageProxy);
    virtual void OnNotMessageOverflow();

    // Registration subscriber functions
    virtual void OnRegistered(const Safir::Dob::Typesystem::TypeId      typeId,
                              const Safir::Dob::Typesystem::HandlerId&  handlerId);
    virtual void OnUnregistered(const Safir::Dob::Typesystem::TypeId      typeId,
                                const Safir::Dob::Typesystem::HandlerId&  handlerId);

    // Service handler functions
    virtual void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                  Safir::Dob::ResponseSenderPtr   responseSender);

    // Service/entity handler functions
    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                       const Safir::Dob::Typesystem::HandlerId& handlerId);

    // Service/entity handler pending additional functions
    virtual void OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                         const Safir::Dob::Typesystem::HandlerId& handlerId);

    // Entity subscriber functions
    void OnNewEntity(const Safir::Dob::EntityProxy entityProxy);
    void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy);
    void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool deleted);

    // Entity handler functions
    void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityProxy, Safir::Dob::ResponseSenderPtr rs);
    void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityProxy, Safir::Dob::ResponseSenderPtr rs);
    void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityProxy, Safir::Dob::ResponseSenderPtr rs);

    // Entity handler injected additional functions
    void OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy entityProxy);
    void OnInjectedUpdatedEntity(const Safir::Dob::InjectedEntityProxy entityProxy);
    void OnInjectedDeletedEntity(const Safir::Dob::InjectedEntityProxy entityProxy);
    void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId       typeId,
                                 const Safir::Dob::Typesystem::HandlerId&   handlerId);

    void OnResponse(const Safir::Dob::ResponseProxy responseProxy);
    void OnNotRequestOverflow();

    // Backdoor functions
    virtual void HandleCommand(const std::vector<std::wstring>& cmdTokens);
    virtual std::wstring GetHelpText();

    const std::wstring CallbackId() const;

    Safir::Dob::SecondaryConnection     m_connection;
    Safir::Application::BackdoorKeeper m_backdoorKeeper;

    const int                           m_consumerNumber;
    const std::wstring                  m_connectionName;
    const std::wstring                  m_connectionInstance;

    typedef std::vector<DoseTest::ActionPtr> Actions;
    typedef std::vector<Actions> CallbackActions;
    std::vector<std::vector<DoseTest::ActionPtr> > m_callbackActions;

    Safir::Dob::ResponseSenderPtr       m_responseSender;
    bool                                m_responseSenderDiscarded;
    Safir::Dob::RequestId               m_latestRequestId;

    typedef boost::tuple<Safir::Dob::Typesystem::TypeId, Safir::Dob::Typesystem::HandlerId> PolicyKey;

    typedef std::pair<Safir::Dob::InstanceIdPolicy::Enumeration, Safir::Dob::Typesystem::Int64> PolicyValue;
    typedef std::map<PolicyKey, PolicyValue> InstanceIdPolicyMap;

    InstanceIdPolicyMap m_instanceIdPolicyMap;

    class TimestampRequestor:
    public Safir::Dob::Requestor
    {
        void OnResponse(const Safir::Dob::ResponseProxy responseProxy) {}
        void OnNotRequestOverflow() {}
    };

    TimestampRequestor m_timestampRequestor;
};

typedef boost::shared_ptr<Consumer> ConsumerPtr;

#endif

