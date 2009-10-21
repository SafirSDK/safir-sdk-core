/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / stjoot
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

#include <Safir/Dob/Internal/Connection.h>

#include "Signals.h"
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/Subscription.h>
#include <Safir/Dob/Internal/MessageTypes.h>
#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/QueueParameters.h>
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/bind.hpp>
#include <boost/lambda/algorithm.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>
#include <boost/static_assert.hpp>

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4702)
#endif

#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    
    

    Identifier Connection::CalculateIdentifier(const std::string & name)
    {
        return DotsId_Generate64(name.c_str());
    }

    Connection::Connection(const std::string & name,
                           const Typesystem::Int32 counter,
                           const NodeNumber node,
                           const int /*context*/,
                           const int pid):
        m_nameWithoutCounter(name.begin(),name.end()),
        m_counter(counter),
        m_id(),
        m_pid(pid),
        m_subscribedTypes(NumberOfSubscriptionTypes),
        m_messageInQueues(GetSharedMemory().construct<MessageQueueContainer>(boost::interprocess::anonymous_instance)()),
        m_messageOutQueue(Dob::QueueParameters::DefaultMessageOutQueueLength()),
        m_requestInQueues(GetSharedMemory().construct<RequestInQueueContainer>(boost::interprocess::anonymous_instance)()),
        m_requestOutQueue(Dob::QueueParameters::DefaultRequestOutQueueLength()),
        m_stopOrderPending(0),
        m_died(0),
        m_nodeDown(0),
        m_isLocal(node == Dob::ThisNodeParameters::NodeNumber())
    {
        const std::wstring wNameWithCounter = Typesystem::Utilities::ToWstring(name) + L"#" + boost::lexical_cast<std::wstring>(counter);
        const std::string nameWithCounter = Typesystem::Utilities::ToUtf8(wNameWithCounter);
        m_nameWithCounter = ShmString(nameWithCounter.begin(),nameWithCounter.end());
        m_id = ConnectionId(node,CalculateIdentifier(nameWithCounter));
        lllout << "In Connection constructor: Constructing '" << name.c_str() << "' id = " << m_id <<" pid = " << m_pid << std::endl;
    }

    Connection::~Connection()
    {
        lllout << "In Connection destructor for '" << m_nameWithoutCounter.c_str() << "' id = " << m_id << std::endl;

        if (IsLocal())
        {
            Signals::Instance().Remove(Id());
        }
        // Unregister all registered handlers
        //(making copy of the map allows Unregister to call back into connection without upsetting the iteration)
        RegistrationsMap registrations;
        registrations.swap(m_registrations);
        std::for_each(registrations.begin(),
                      registrations.end(),
                      boost::bind(&Connection::Unregister, this, _1));

        // Remove all subscriptions
        for (TypesSetVector::iterator it = m_subscribedTypes.begin(); it < m_subscribedTypes.end(); ++it)
        {
            TypesSet subscribedTypes;

            // Making copy of the set allows Unsubscribe to call back into connection without upsetting the iteration.
            subscribedTypes.swap(*it);
            std::for_each(subscribedTypes.begin(),
                          subscribedTypes.end(),
                          boost::bind(&Connection::Unsubscribe, this, _1));
        }

        //We do nothing about the message in queue, we don't care if the app hasn't emptied its in-queue before exiting.
        //We do nothing about the pending ownerships, since dose_main clears up the "master lists"
        //We do nothing about the revoked ownerships, since it doesnt matter if the app has not been notified of revoked ownerships
        //  when it is exiting.

        //ENSURE(m_messageOutQueue.empty(), << "MessageOutQ should have been cleaned up before call to Connection destructor of " << Name());
        //TODO: The above is only useful when we've implemented zombie connections.

        //TODO: check that all other vectors/queues are empty as they should be!
    }

    RequestInQueuePtr Connection::AddRequestInQueue(const ConsumerId& consumer)
    {
        if (!IsLocal())
        {
            return RequestInQueuePtr();  // null pointer
        }

        return m_requestInQueues->AddQueue(m_requestInQueues,consumer, Dob::QueueParameters::DefaultRequestInQueueLength());
    }

    void Connection::ForEachRequestInQueue(const RequestInQueueContainer::QueueFunc& queueFunc) const
    {
        m_requestInQueues->ForEach(queueFunc);
    }

    void Connection::ForSpecificRequestInQueue(const ConsumerId& consumer, const RequestInQueueContainer::QueueFunc& queueFunc) const
    {
        m_requestInQueues->ForSpecific(consumer, queueFunc);
    }


    bool Connection::IsLocal() const
    {
        return m_isLocal;
    }

    MessageQueuePtr Connection::AddMessageInQueue(const ConsumerId& consumer)
    {
        return m_messageInQueues->AddQueue(m_messageInQueues,consumer, Dob::QueueParameters::DefaultMessageInQueueLength());
    }

    void Connection::ForEachMessageInQueue(const MessageQueueContainer::QueueFunc& queueFunc) const
    {
        m_messageInQueues->ForEach(queueFunc);
    }

    void Connection::AddSubscription(const Typesystem::TypeId   typeId,
                                     SubscriptionType           subscriptionType)
    {
        if (!IsLocal()) return;

        m_subscribedTypes[subscriptionType].insert(typeId);
    }

    Connection::TypesSet
    Connection::GetSubscriptions(SubscriptionType subscriptionType) const
    {
        return m_subscribedTypes[subscriptionType];
    }

    void Connection::AddEmptyInitialInjection(const Typesystem::TypeId              typeId,
                                              const Dob::Typesystem::HandlerId&     handlerId)
    {
        TypeHandlerKey key = std::make_pair(typeId, handlerId);
        InitialInjectionValue instanceSet;
        m_initialInjectionInstances.insert(std::make_pair(key, instanceSet));
    }

    std::vector<TypeHandlerPair> Connection::GetAndClearEmptyInitialInjections()
    {
        std::vector<TypeHandlerPair> result;

        for (InitialInjectionInstances::iterator it = m_initialInjectionInstances.begin();
             it != m_initialInjectionInstances.end();) // Note the missing ++it.
        {
            // Since we are erasing what the iterator is refering to, the iterator incrementation is
            // handled here instead of in the for loop.
            if (it->second.empty())
            {
                result.push_back(std::make_pair(it->first.first, it->first.second.GetHandlerId()));
                it = m_initialInjectionInstances.erase(it);
            }
            else
            {
                ++it;
            }
        }
        return result;
    }

    void Connection::AddInitialInjectionInstance(const Typesystem::TypeId              typeId,
                                                 const Dob::Typesystem::HandlerId&     handlerId,
                                                 const Dob::Typesystem::InstanceId&    instanceId)
    {
        TypeHandlerKey key = std::make_pair(typeId, handlerId);

        InitialInjectionInstances::iterator it = m_initialInjectionInstances.find(key);

        if (it != m_initialInjectionInstances.end())
        {
            // The type/handler already exist, insert instance in the existing set
            it->second.insert(instanceId.GetRawValue());
        }
        else
        {
            // The type/handler doesn't exist.
            InitialInjectionValue instanceSet;
            instanceSet.insert(instanceId.GetRawValue());
            m_initialInjectionInstances.insert(std::make_pair(key, instanceSet));
        }
    }

    bool Connection::RemoveInitialInjectionInstance(const Typesystem::TypeId              typeId,
                                                    const Dob::Typesystem::HandlerId&     handlerId,
                                                    const Dob::Typesystem::InstanceId&    instanceId,
                                                    const bool                            allInstances)
    {
        if (m_initialInjectionInstances.empty())
        {
            return false;
        }

        TypeHandlerKey key = std::make_pair(typeId, handlerId);

        InitialInjectionInstances::iterator it = m_initialInjectionInstances.find(key);

        if (it != m_initialInjectionInstances.end())
        {
            // The type/handler already exist.

            if (allInstances)
            {
                it->second.clear();
            }
            else
            {
                it->second.erase(instanceId.GetRawValue());
            }

            if (it->second.empty())
            {
                // We just removed the last instance
                m_initialInjectionInstances.erase(it);
                return true;
            }
        }
        return false;
    }

    void Connection::AddInjectionHandler(const Typesystem::TypeId              typeId,
                                         const Dob::Typesystem::HandlerId&     handlerId,
                                         const ConsumerId&                     consumer)
    {
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        m_injectionHandlers.insert(std::make_pair(std::make_pair(typeId, ShmHandlerId(handlerId)), consumer));
    }

    void Connection::RemoveInjectionHandler(const Typesystem::TypeId              typeId,
                                            const Dob::Typesystem::HandlerId&     handlerId)
    {
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        m_injectionHandlers.erase(std::make_pair(typeId, ShmHandlerId(handlerId)));
    }

    const ConsumerId Connection::GetInjectionHandlerConsumer(const Typesystem::TypeId              typeId,
                                                             const Dob::Typesystem::HandlerId&     handlerId) const
    {
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        RegistrationsMap::const_iterator it = m_injectionHandlers.find(std::make_pair(typeId, ShmHandlerId(handlerId)));
        ENSURE(it != m_injectionHandlers.end(), << "Didn't find given type/handler");
        return it->second;
    }

    const RegistrationVector Connection::GetInjectionHandlers() const
    {
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);
        return GetRegistrations(m_injectionHandlers);
    }

    void Connection::AddRegistration(const Typesystem::TypeId              typeId,
                                     const Dob::Typesystem::HandlerId&     handlerId,
                                     const ConsumerId&                     consumer)
    {
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        m_registrations.insert(std::make_pair(std::make_pair(typeId, ShmHandlerId(handlerId)), consumer));
    }

    void Connection::RemoveRegistration(const Typesystem::TypeId              typeId,
                                        const Dob::Typesystem::HandlerId&     handlerId)
    {
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        m_registrations.erase(std::make_pair(typeId, ShmHandlerId(handlerId)));
    }

    void Connection::AddRevokedRegistration(const Typesystem::TypeId              typeId,
                                            const Dob::Typesystem::HandlerId&     handlerId,
                                            const ConsumerId&                     consumer)
    {
        if (!IsLocal()) return;

        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        m_revokedRegistrations.insert(std::make_pair(std::make_pair(typeId, ShmHandlerId(handlerId)), consumer));
    }

    void Connection::RemoveRevokedRegistration(const Typesystem::TypeId              typeId,
                                               const Dob::Typesystem::HandlerId&     handlerId)
    {
        if (!IsLocal()) return;

        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        m_revokedRegistrations.erase(std::make_pair(typeId, ShmHandlerId(handlerId)));
    }

    const RegistrationVector Connection::GetRevokedRegistrations() const
    {
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);
        return GetRegistrations(m_revokedRegistrations);
    }

    const RegistrationVector Connection::GetAndClearRevokedRegistrations()
    {
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        RegistrationVector tmp = GetRegistrations(m_revokedRegistrations);
        m_revokedRegistrations.clear();
        return tmp;
    }

    void Connection::SignalOut() const
    {
        *m_outSignal = 1;
        Signals::Instance().SignalConnectOrOut();
    }

    void Connection::SignalIn() const
    {
        if (!IsLocal()) return;

        Signals::Instance().SignalIn(Id());
    }

    void Connection::AddPendingRegistration(const PendingRegistration & reg)
    {
        // Acquire connection lock
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);
        lllout << "AddPendingRegistration for type " << Dob::Typesystem::Operations::GetName(reg.typeId)
            << " for connection " << NameWithCounter() << std::endl;

        m_pendingOwnerships.push_back(reg);
    }

    void Connection::Unsubscribe(const Typesystem::TypeId typeId)
    {
        if (Dob::Typesystem::Operations::IsOfType(typeId, Dob::Entity::ClassTypeId))
        {
            lllout << "Unsubscribing all entity handler registrations and entity subscriptions for type "<< typeId << std::endl;
            EntityTypes::Instance().UnsubscribeAll(this, typeId);
        }
        else if (Dob::Typesystem::Operations::IsOfType(typeId, Dob::Service::ClassTypeId))
        {
            lllout << "Unsubscribing all service handler registrations for type "<< typeId << std::endl;
            ServiceTypes::Instance().UnsubscribeRegistrationAll(this, typeId);
        }
        else if (Dob::Typesystem::Operations::IsOfType(typeId, Dob::Message::ClassTypeId))
        {
            lllout << "Unsubscribing all Messages of type "<< typeId << std::endl;
            MessageTypes::Instance().UnsubscribeAll(this, typeId);
        }
        else
        {
            ENSURE(false, << "Connection::Unsubscribe: Unexpected typeId (" << Dob::Typesystem::Operations::GetName(typeId) << ")!!!");
        }
    }

    void Connection::Unregister(const std::pair<TypeHandlerKey, ConsumerId>& reg)
    {
        const Typesystem::TypeId& typeId = reg.first.first;

        if (Dob::Typesystem::Operations::IsOfType(typeId, Dob::Entity::ClassTypeId))
        {
            EntityTypes::Instance().UnregisterAll(this, typeId);
        }
        else if (Dob::Typesystem::Operations::IsOfType(typeId, Dob::Service::ClassTypeId))
        {
            ServiceTypes::Instance().UnregisterAll(this, typeId);
        }
        else
        {
            ENSURE(false, << "Connection::Unsubscribe: Unexpected typeId (" << Dob::Typesystem::Operations::GetName(typeId) << ")!!!");
        }
    }

    void Connection::SetPendingRegistrationAccepted(const long requestId)
    {
        // Acquire connection lock
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        for (PendingOwnerships::iterator it = m_pendingOwnerships.begin();
             it != m_pendingOwnerships.end(); ++it)
        {
            PendingRegistration &theReg = *it;
            if (theReg.id == requestId)
            {
                if (theReg.remove)
                {
                    lllout << "Pending registration request " << requestId << " has been removed, so I'll not set it accepted." <<std::endl;
                    return;
                }
                lllout << "Pending registration request " << requestId << " has completed" <<std::endl;
                theReg.accepted = true;
                return;
            }
        }

        ENSURE(false, << "Connection::SetPendingRegistrationAccepted: Could not find pending registration in connection!");
    }

    bool Connection::IsPendingAccepted(const Dob::Typesystem::TypeId typeId,
                                       const Dob::Typesystem::HandlerId & handlerId) const
    {
        // Acquire connection lock
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        if (IsLocal())
        {
            for (PendingOwnerships::const_iterator it = m_pendingOwnerships.begin();
                 it != m_pendingOwnerships.end(); ++it)
            {
                const PendingRegistration & reg = *it;
                if (reg.accepted && reg.typeId == typeId && reg.handlerId == handlerId)
                {
                    return true;
                }
            }
        }
        return false;
    }


    const PendingRegistrationVector
    Connection::GetRemovedPendingRegistrations()
    {
        PendingRegistrationVector prv;

        // Acquire connection lock
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        PendingOwnerships::iterator firstRemoved =
            std::partition(m_pendingOwnerships.begin(),m_pendingOwnerships.end(),!boost::bind(&PendingRegistration::IsRemoved,_1));
        std::copy(firstRemoved,m_pendingOwnerships.end(),std::back_inserter(prv));
        m_pendingOwnerships.erase(firstRemoved,m_pendingOwnerships.end());
        return prv;
    }

    void Connection::RetryAcceptedPendingOwnership(const long requestId)
    {
        // Acquire connection lock
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        for (PendingOwnerships::iterator it = m_pendingOwnerships.begin();
             it != m_pendingOwnerships.end(); ++it)
        {
            PendingRegistration &theReg = *it;
            if (theReg.id == requestId)
            {
                ENSURE(theReg.id != 0 && theReg.accepted, << "Connection::RetryPendingOwnership: Incorrect state! " << theReg.id << " " << theReg.accepted);
                theReg.id = 0;
                theReg.accepted = false;
                return;
            }
        }

        ENSURE(false, << "Connection::RetryPendingOwnership: Could not find pending registration in connection!");
    }

    bool RequestIdMatches(const PendingRegistration & theReg, const int reqId)
    {return theReg.id == reqId;}

    void Connection::RemoveAcceptedPendingOwnership(const long requestId)
    {
        // Acquire connection lock
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        PendingOwnerships::iterator removeEnd = std::remove_if(m_pendingOwnerships.begin(),m_pendingOwnerships.end(),
                                                               boost::bind(RequestIdMatches,_1,requestId));

        if (removeEnd != m_pendingOwnerships.end())
        {
            m_pendingOwnerships.erase(removeEnd,m_pendingOwnerships.end());
        }
        else
        {
            //TODO: verify that this is the case! See ticket #13
            lllout << "Connection::RemoveAcceptedPendingOwnership: Could not find the accepted pending ownership in connection! "
                   << "Assuming that it belongs to a previous incarnation of this connection, so I'll ignore it" << std::endl;
            //        ENSURE(false, << "Connection::RemovePendingOwnership: Could not find pending registration in connection!");
        }
    }


    const PendingRegistrationVector Connection::GetPendingRegistrations()
    {
        // Acquire connection lock
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        PendingRegistrationVector prv;
        std::copy(m_pendingOwnerships.begin(),m_pendingOwnerships.end(),std::back_inserter(prv));
        return prv;
    }

    bool Connection::GetNextNewPendingRegistration(const long requestId, PendingRegistration & reg)
    {
        // Acquire connection lock
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        for (PendingOwnerships::iterator it = m_pendingOwnerships.begin();
             it != m_pendingOwnerships.end(); ++it)
        {
            PendingRegistration & aReg = *it;
            if (aReg.id == 0) //no request sent yet
            {
                aReg.id = requestId;
                reg = aReg;
                return true;
            }
        }
        return false;
    }

    bool Connection::RemovePendingRegistrations(const Dob::Typesystem::TypeId typeId,
                                                const Dob::Typesystem::HandlerId & handlerId)
    {
        bool somethingRemoved = false;
        // Acquire connection lock
        boost::interprocess::scoped_lock<boost::interprocess::interprocess_mutex> lck(m_lock);

        for (PendingOwnerships::iterator it = m_pendingOwnerships.begin();
             it != m_pendingOwnerships.end(); ++it)
        {
            if (it->typeId == typeId && it->handlerId == handlerId)
            {
                it->remove = true;
                somethingRemoved = true;
            }
        }
        return somethingRemoved;
    }

    const RegistrationVector Connection::GetRegistrations(const RegistrationsMap& registrations) const
    {
        RegistrationVector regVec;

        for (RegistrationsMap::const_iterator it = registrations.begin(); it != registrations.end(); ++it)
        {
            RegistrationInfo regInfo;
            regInfo.typeId = it->first.first;
            regInfo.handlerId = it->first.second.GetHandlerId();
            regInfo.consumer = it->second;

            regVec.push_back(regInfo);
        }

        return regVec;
    }
}
}
}
