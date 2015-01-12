/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

#ifndef _dose_dipacher_h
#define _dose_dipacher_h

#include <Safir/Dob/Internal/Interface.h>
// Sdk
#include <Safir/Dob/CallbackId.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/ConsumerId.h>
#include <Safir/Dob/Internal/Subscription.h>
#include <Safir/Dob/Typesystem/Object.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <boost/noncopyable.hpp>

#include <vector>
#include <stack>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    struct CallbackData
    {
        explicit CallbackData(const CallbackId::Enumeration callbackId);
        CallbackId::Enumeration m_callbackId;
    };

    typedef std::stack<CallbackData> CallbackStack;

    //The dispatcher class
    class Dispatcher
    {
    public:
        Dispatcher();
        ~Dispatcher();

        void SetCallbacks(long lang, //see Interface.h
                          OnNewEntityCb* onNewEntityCb,
                          OnUpdatedEntityCb* onUpdatedEntityCb,
                          OnDeletedEntityCb* onDeletedEntityCb,
                          OnCreateRequestCb* onCreateRequestCb,
                          OnUpdateRequestCb* onUpdateRequestCb,
                          OnDeleteRequestCb* onDeleteRequestCb,
                          OnServiceRequestCb* onServiceRequestCb,
                          OnResponseCb* onResponseCb,
                          OnMessageCb* onMessageCb,
                          OnRegisteredCb* onRegisteredCb,
                          OnUnregisteredCb* onUnregisteredCb,
                          OnRevokedRegistrationCb* onRevokedRegistrationCb,
                          OnCompletedRegistrationCb* onCompletedRegistrationCb,
                          OnInjectedNewEntityCb* onInjectedNewEntityCb,
                          OnInjectedUpdatedEntityCb* onInjectedUpdatedEntityCb,
                          OnInjectedDeletedEntityCb* onInjectedDeletedEntityCb,
                          OnInitialInjectionsDoneCb* onInitialInjectionsDoneCb,
                          OnNotRequestOverflowCb* onNotRequestOverflowCb,
                          OnNotMessageOverflowCb* onNotMessageOverflowCb,
                          OnDropReferenceCb* onDropReferenceCb);

        void SetConnectionOwner(const ConsumerId & connectionOwner,
                                OnStopOrderCb* onStopOrderCb);

        /** Add a consumer to be notified when we no longer have a message overflow. */
        void AddOverflowedMessageConsumer(const ConsumerId & overflowedConsumer)
        { m_overflowedMessageConsumers.insert(overflowedConsumer); }

        /** Add a consumer to be notified when we no longer have a request out overflow. */
        void AddOverflowedRequestConsumer(const ConsumerId & overflowedConsumer)
        { m_overflowedRequestConsumers.insert(overflowedConsumer); }

        /** Add consumer that awaits a response for the request with the given request id */
        void AddResponseConsumer(const RequestId requestId,
                            const ConsumerId & overflowedConsumer)
        { m_responseConsumers.insert(std::make_pair(requestId,overflowedConsumer)); }

        /** Remove consumer corresponding to the given request id */
        void RemoveResponseConsumer(const RequestId requestId)
        { m_responseConsumers.erase(requestId); }

        /** Dispatch a response to the requestor that issued the request (based on requestId). */
        void InvokeOnResponseCb(const DistributionData & response, const DistributionData & request);

        void InvokeOnMessageCb(const ConsumerId& consumer,
                               const DistributionData & message);

        void InvokeOnRegisteredCb(const ConsumerId&                           consumer,
                                  const Safir::Dob::Typesystem::TypeId&       typeId,
                                  const Safir::Dob::Typesystem::HandlerId&    handlerId);

        void InvokeOnUnregisteredCb(const ConsumerId&                           consumer,
                                    const Safir::Dob::Typesystem::TypeId&       typeId,
                                    const Safir::Dob::Typesystem::HandlerId&    handlerId);

        void InvokeOnRevokedRegistrationCb(const ConsumerId&                           consumer,
                                           const Safir::Dob::Typesystem::TypeId&       typeId,
                                           const Safir::Dob::Typesystem::HandlerId&    handlerId);

        void InvokeOnCompletedRegistrationCb(const ConsumerId&                           consumer,
                                             const Safir::Dob::Typesystem::TypeId&       typeId,
                                             const Safir::Dob::Typesystem::HandlerId&    handlerId);

        void InvokeOnServiceRequestCb(const ConsumerId& consumer,
                                      const DistributionData& request,
                                      const long ctrl);


        void InvokeOnCreateRequestCb(const ConsumerId& consumer,
                                     const DistributionData& request,
                                     const long ctrl);

        void InvokeOnUpdateRequestCb(const ConsumerId& consumer,
                                     const DistributionData& request,
                                     const long ctrl);

        void InvokeOnDeleteRequestCb(const ConsumerId& consumer,
                                     const DistributionData& request,
                                     const long ctrl);

        void InvokeOnNewEntityCb(const ConsumerId& consumer,
                                 const DistributionData& currentState,
                                 const bool timestampChangeInfo);

        void InvokeOnUpdatedEntityCb(const ConsumerId& consumer,
                                     const DistributionData& currentState,
                                     const DistributionData& lastState,
                                     const bool timestampChangeInfo);

        void InvokeOnDeletedEntityCb(const ConsumerId& consumer,
                                     const DistributionData& currentState,
                                     const DistributionData& lastState,
                                     const bool explicitlyDeleted,
                                     const bool timestampChangeInfo);

        void InvokeOnInjectedNewEntityCb(const ConsumerId& consumer,
                                         const DistributionData& injectionState);

        void InvokeOnInjectedUpdatedEntityCb(const ConsumerId& consumer,
                                             const DistributionData& injectionState,
                                             const DistributionData& currentState);

        void InvokeOnInjectedDeletedEntityCb(const ConsumerId& consumer,
                                             const DistributionData& injectionState,
                                             const DistributionData& currentState);

        void InvokeOnInitialInjectionsDoneCb(const ConsumerId&                          consumer,
                                             const Safir::Dob::Typesystem::TypeId&      typeId,
                                             const Safir::Dob::Typesystem::HandlerId&   handlerId);

        void InvokeDropReferenceCb(const ConsumerId&    consumer,
                                   const long           refCounter);

        bool InCallback() const {return !m_callbackStack.empty();}
        bool InCallback(const CallbackId::Enumeration callbackId) const {return InCallback() && m_callbackStack.top().m_callbackId == callbackId;}
        const CallbackStack & GetCallbackStack() const {return m_callbackStack;}

        void DispatchNotRequestOverflows();
        void DispatchNotMessageOverflows();

        //Stop order
        void DispatchStopOrder();

        void Clear();

    private:
        //----------------------------
        // Internal types
        //----------------------------

        typedef std::set<ConsumerId> ConsumerIdSet;

        ConsumerIdSet m_overflowedMessageConsumers;
        ConsumerIdSet m_overflowedRequestConsumers;

        typedef std::map<RequestId, ConsumerId> ResponseConsumerTable;
        ResponseConsumerTable m_responseConsumers;

        //------------------------------
        //Callback functions
        //------------------------------
        class Callbacks
        {
        public:
            Callbacks() :
              m_initiated(false),
              m_onNewEntityCb(NULL),
              m_onUpdatedEntityCb(NULL),
              m_onDeletedEntityCb(NULL),
              m_onCreateRequestCb(NULL),
              m_onUpdateRequestCb (NULL),
              m_onDeleteRequestCb(NULL),
              m_onServiceRequestCb(NULL),
              m_onResponseCb(NULL),
              m_onMessageCb(NULL),
              m_onRegisteredCb(NULL),
              m_onUnregisteredCb(NULL),
              m_onRevokedRegistrationCb(NULL),
              m_onCompletedRegistrationCb(NULL),
              m_onInjectedNewEntityCb(NULL),
              m_onInjectedUpdatedEntityCb(NULL),
              m_onInjectedDeletedEntityCb(NULL),
              m_onInitialInjectionsDoneCb(NULL),
              m_onNotRequestOverflowCb(NULL),
              m_onNotMessageOverflowCb(NULL),
              m_onDropReferenceCb(NULL) {}

            bool m_initiated;
            OnNewEntityCb*                  m_onNewEntityCb;
            OnUpdatedEntityCb*              m_onUpdatedEntityCb;
            OnDeletedEntityCb*              m_onDeletedEntityCb;
            OnCreateRequestCb*              m_onCreateRequestCb;
            OnUpdateRequestCb*              m_onUpdateRequestCb;
            OnDeleteRequestCb*              m_onDeleteRequestCb;
            OnServiceRequestCb*             m_onServiceRequestCb;
            OnResponseCb*                   m_onResponseCb;
            OnMessageCb*                    m_onMessageCb;
            OnRegisteredCb*                 m_onRegisteredCb;
            OnUnregisteredCb*               m_onUnregisteredCb;
            OnRevokedRegistrationCb*        m_onRevokedRegistrationCb;
            OnCompletedRegistrationCb*      m_onCompletedRegistrationCb;
            OnInjectedNewEntityCb*          m_onInjectedNewEntityCb;
            OnInjectedUpdatedEntityCb*      m_onInjectedUpdatedEntityCb;
            OnInjectedDeletedEntityCb*      m_onInjectedDeletedEntityCb;
            OnInitialInjectionsDoneCb*      m_onInitialInjectionsDoneCb;
            OnNotRequestOverflowCb*         m_onNotRequestOverflowCb;
            OnNotMessageOverflowCb*         m_onNotMessageOverflowCb;
            OnDropReferenceCb*              m_onDropReferenceCb;
        };

        Callbacks m_callbacks[4]; //see Interface.h for explaination of indices.

        void NotRequestOverflowCb(const ConsumerId& consumer);

        void NotMessageOverflowCb(const ConsumerId& consumer);

        //------------------------------
        // Stop order variables
        //  There is only one stop order callback since there can only be
        //  one connection owner!
        //------------------------------

        OnStopOrderCb *  m_onStopOrderCb;
        ConsumerId       m_connectionOwner;

        CallbackStack m_callbackStack;

        /**
         * Check that no locks are taken when calling into user code.
         * Only done in debug builds
         */
#ifndef NDEBUG
        void CheckLocks() const;
#else
        inline void CheckLocks() const {}
#endif

    };

    class DispatcherIsInCallback:
        private boost::noncopyable
    {
    public:
        DispatcherIsInCallback(CallbackStack & callbackStack,
                               const CallbackId::Enumeration callback);

        ~DispatcherIsInCallback();
    private:
        CallbackStack & m_callbackStack;
        const CallbackId::Enumeration m_callback;
    };
}
}
}

#endif //_dose_dipacher_h
