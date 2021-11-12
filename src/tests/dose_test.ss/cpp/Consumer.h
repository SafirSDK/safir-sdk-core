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
    public Safir::Application::Backdoor
{
public:
    Consumer(const int consumerNumber,
             const std::wstring & connectionName,
             const std::wstring & instance);

    Consumer(const Consumer&) = delete;
    Consumer& operator=(const Consumer&) = delete;

    void ExecuteAction(DoseTest::ActionPtr action);
    void AddCallbackAction(DoseTest::ActionPtr action);
    void ExecuteCallbackActions(const Safir::Dob::CallbackId::Enumeration callback);

private:

    Safir::Dob::Typesystem::Int64 GetTimestamp(const DoseTest::ActionPtr& action);

    //Consumer functions
    void OnMessage(const Safir::Dob::MessageProxy messageProxy) override;
    void OnNotMessageOverflow() override;

    // Registration subscriber functions
    void OnRegistered(const Safir::Dob::Typesystem::TypeId      typeId,
                              const Safir::Dob::Typesystem::HandlerId&  handlerId) override;
    void OnUnregistered(const Safir::Dob::Typesystem::TypeId      typeId,
                                const Safir::Dob::Typesystem::HandlerId&  handlerId) override;

    // Service handler functions
    void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                  Safir::Dob::ResponseSenderPtr   responseSender) override;

    // Service/entity handler functions
    void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                       const Safir::Dob::Typesystem::HandlerId& handlerId) override;

    // Service/entity handler pending additional functions
    void OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                         const Safir::Dob::Typesystem::HandlerId& handlerId) override;

    // Entity subscriber functions
    void OnNewEntity(const Safir::Dob::EntityProxy entityProxy) override;
    void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy) override;
    void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool deleted) override;

    // Entity handler functions
    void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityProxy, Safir::Dob::ResponseSenderPtr rs) override;
    void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityProxy, Safir::Dob::ResponseSenderPtr rs) override;
    void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityProxy, Safir::Dob::ResponseSenderPtr rs) override;

    // Entity handler injected additional functions
    void OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy entityProxy) override;
    void OnInjectedUpdatedEntity(const Safir::Dob::InjectedEntityProxy entityProxy) override;
    void OnInjectedDeletedEntity(const Safir::Dob::InjectedEntityProxy entityProxy) override;
    void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId       typeId,
                                 const Safir::Dob::Typesystem::HandlerId&   handlerId) override;

    void OnResponse(const Safir::Dob::ResponseProxy responseProxy) override;
    void OnNotRequestOverflow() override;

    // Backdoor functions
    void HandleCommand(const std::vector<std::wstring>& cmdTokens) override;
    std::wstring GetHelpText() override;

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
        void OnResponse(const Safir::Dob::ResponseProxy /*responseProxy*/) override {}
        void OnNotRequestOverflow() override {}
    };

    TimestampRequestor m_timestampRequestor;
};

typedef boost::shared_ptr<Consumer> ConsumerPtr;

#endif

