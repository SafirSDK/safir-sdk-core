/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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

#include "dose_dispatcher.h"
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/Interface.h>
#include <Safir/Dob/Internal/Subscription.h>
#include <Safir/Dob/Internal/LeveledLockHelper.h>
#include <Safir/Dob/Typesystem/BlobOperations.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <iostream>
#include <boost/shared_ptr.hpp>


namespace Safir
{
namespace Dob
{
namespace Internal
{
    Dispatcher::Dispatcher():
        m_onStopOrderCb(NULL),
        m_connectionOwner(NULL, -1L)
    {

    }

    Dispatcher::~Dispatcher()
    {

    }

    void Dispatcher::SetCallbacks(long lang,
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
                                  OnDropReferenceCb* onDropReferenceCb)
    {
        if (!m_callbacks[lang].m_initiated)
        {
            m_callbacks[lang].m_onNewEntityCb=onNewEntityCb;
            m_callbacks[lang].m_onUpdatedEntityCb=onUpdatedEntityCb;
            m_callbacks[lang].m_onDeletedEntityCb=onDeletedEntityCb;
            m_callbacks[lang].m_onCreateRequestCb=onCreateRequestCb;
            m_callbacks[lang].m_onUpdateRequestCb=onUpdateRequestCb;
            m_callbacks[lang].m_onDeleteRequestCb=onDeleteRequestCb;
            m_callbacks[lang].m_onServiceRequestCb=onServiceRequestCb;
            m_callbacks[lang].m_onResponseCb=onResponseCb;
            m_callbacks[lang].m_onMessageCb=onMessageCb;
            m_callbacks[lang].m_onRegisteredCb=onRegisteredCb;
            m_callbacks[lang].m_onUnregisteredCb=onUnregisteredCb;
            m_callbacks[lang].m_onRevokedRegistrationCb=onRevokedRegistrationCb;
            m_callbacks[lang].m_onCompletedRegistrationCb=onCompletedRegistrationCb;
            m_callbacks[lang].m_onInjectedNewEntityCb=onInjectedNewEntityCb;
            m_callbacks[lang].m_onInjectedUpdatedEntityCb=onInjectedUpdatedEntityCb;
            m_callbacks[lang].m_onInjectedDeletedEntityCb=onInjectedDeletedEntityCb;
            m_callbacks[lang].m_onInitialInjectionsDoneCb=onInitialInjectionsDoneCb;
            m_callbacks[lang].m_onNotRequestOverflowCb=onNotRequestOverflowCb;
            m_callbacks[lang].m_onNotMessageOverflowCb=onNotMessageOverflowCb;
            m_callbacks[lang].m_onDropReferenceCb=onDropReferenceCb;
        }
    }

    void Dispatcher::SetConnectionOwner(const ConsumerId & connectionOwner,
                                        OnStopOrderCb* onStopOrderCb)
    {
        m_connectionOwner = connectionOwner;
        m_onStopOrderCb = onStopOrderCb;
    }

    void Dispatcher::InvokeOnResponseCb(const DistributionData& response,
                                          const DistributionData& request)
    {
        //find the response consumer
        const ResponseConsumerTable::iterator findIt
            = m_responseConsumers.find(response.GetRequestId().GetCounter());

        ENSURE(findIt!=m_responseConsumers.end(),
            << "Trying to dispatch a response whose request id does not appear in the response consumer list! type = "
            << response.GetTypeId() << " requestId = " << response.GetRequestId());

        const ConsumerId consumer = findIt->second;

        //dispatch the response
        DispatcherIsInCallback push(m_callbackStack,CallbackId::OnResponse);
        bool success;


        boost::shared_ptr<const char> responseRefHolder (response.GetReference(), &DistributionData::DropReference);
        boost::shared_ptr<const char> requestRefHolder (request.GetReference(), &DistributionData::DropReference);

        const bool requestHasBlob = request.GetType() != DistributionData::Request_EntityDelete;

        CheckLocks();

        m_callbacks[consumer.lang].m_onResponseCb(response.GetRequestId().GetCounter(),
                                                  response.GetBlob(),
                                                  responseRefHolder.get(),
                                                  requestHasBlob?request.GetBlob():NULL,
                                                  requestRefHolder.get(),
                                                  consumer.consumer,
                                                  success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                     << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                     << ". The response being processed was:" << std::endl
                     << Typesystem::Serialization::ToXml(response.GetBlob()) << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }

        //finally, remove the response consumer
        m_responseConsumers.erase(findIt);
    }

    void Dispatcher::DispatchNotRequestOverflows()
    {
        //Get a copy of the overflowed consumers and clear the original (through a swap)
        ConsumerIdSet overflowedConsumers;
        overflowedConsumers.swap(m_overflowedRequestConsumers);
        std::for_each(overflowedConsumers.begin(),overflowedConsumers.end(),
            boost::bind(&Dispatcher::NotRequestOverflowCb,this,_1));
    }

    void Dispatcher::DispatchNotMessageOverflows()
    {
        //Get a copy of the overflowed consumers and clear the original (through a swap)
        ConsumerIdSet overflowedConsumers;
        overflowedConsumers.swap(m_overflowedMessageConsumers);
        std::for_each(overflowedConsumers.begin(),overflowedConsumers.end(),
            boost::bind(&Dispatcher::NotMessageOverflowCb,this,_1));
    }

    //Stop order
    void Dispatcher::DispatchStopOrder()
    {
        ENSURE (m_onStopOrderCb != NULL, << "Failed to dispatch stop order since there was no registered connection owner!");

        if (m_connectionOwner.consumer != NULL)
        {
            bool success;
            CheckLocks();
            lllog(3) << "Calling stop order callback" << std::endl;
            m_onStopOrderCb(m_connectionOwner.consumer,
                            success);
            if (!success)
            {
                Typesystem::LibraryExceptions::Instance().Throw();
            }
        }
    }

    void Dispatcher::InvokeOnMessageCb(const ConsumerId& consumer, const DistributionData& message)
    {
        DispatcherIsInCallback push(m_callbackStack,CallbackId::OnMessage);
        bool success;

        boost::shared_ptr<const char> refHolder (message.GetReference(), &DistributionData::DropReference);

        CheckLocks();

        m_callbacks[consumer.lang].m_onMessageCb(message.GetBlob(),
                                                 refHolder.get(),
                                                 consumer.consumer,
                                                 success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                << ". The object being processed was:" << std::endl
                << Typesystem::Serialization::ToXml(message.GetBlob()) << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::InvokeOnRegisteredCb(const ConsumerId&                           consumer,
                                            const Safir::Dob::Typesystem::TypeId&       typeId,
                                            const Safir::Dob::Typesystem::HandlerId&    handlerId)
    {
        DispatcherIsInCallback push(m_callbackStack,CallbackId::OnRegistered);
        bool success;

        CheckLocks();

        m_callbacks[consumer.lang].m_onRegisteredCb(typeId,
                                                    handlerId.GetRawValue(),
                                                    handlerId.Utf8String().c_str(),
                                                    consumer.consumer,
                                                    success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                << ". typeId:" << Typesystem::Operations::GetName(typeId) << " handlerId:" << handlerId << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::InvokeOnUnregisteredCb(const ConsumerId&                           consumer,
                                              const Safir::Dob::Typesystem::TypeId&       typeId,
                                              const Safir::Dob::Typesystem::HandlerId&    handlerId)
    {
        DispatcherIsInCallback push(m_callbackStack,CallbackId::OnUnregistered);
        bool success;

        CheckLocks();

        m_callbacks[consumer.lang].m_onUnregisteredCb(typeId,
                                                      handlerId.GetRawValue(),
                                                      handlerId.Utf8String().c_str(),
                                                      consumer.consumer,
                                                      success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                << ". typeId:" << Typesystem::Operations::GetName(typeId) << " handlerId:" << handlerId << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::InvokeOnRevokedRegistrationCb(const ConsumerId&                           consumer,
                                                     const Safir::Dob::Typesystem::TypeId&       typeId,
                                                     const Safir::Dob::Typesystem::HandlerId&    handlerId)
    {
        DispatcherIsInCallback push(m_callbackStack, CallbackId::OnRevokedRegistration);
        bool success;

        CheckLocks();

        m_callbacks[consumer.lang].m_onRevokedRegistrationCb(typeId,
                                                             handlerId.GetRawValue(),
                                                             handlerId.Utf8String().c_str(),
                                                             consumer.consumer,
                                                             success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                << ". typeId:" << Typesystem::Operations::GetName(typeId) << " handlerId:" << handlerId << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    void Dispatcher::InvokeOnCompletedRegistrationCb(const ConsumerId&                           consumer,
                                                     const Safir::Dob::Typesystem::TypeId&       typeId,
                                                     const Safir::Dob::Typesystem::HandlerId&    handlerId)
    {
        DispatcherIsInCallback push(m_callbackStack, CallbackId::OnCompletedRegistration);
        bool success;

        CheckLocks();

        m_callbacks[consumer.lang].m_onCompletedRegistrationCb(typeId,
                                                              handlerId.GetRawValue(),
                                                              handlerId.Utf8String().c_str(),
                                                              consumer.consumer,
                                                              success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                << ". typeId:" << Typesystem::Operations::GetName(typeId) << " handlerId:" << handlerId << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    //-------------------
    //Requests
    //-------------------
    void Dispatcher::InvokeOnServiceRequestCb(const ConsumerId& consumer,
                                              const DistributionData& request,
                                              const long ctrl)
    {
        DispatcherIsInCallback push(m_callbackStack, CallbackId::OnServiceRequest);

        bool success;
        boost::shared_ptr<const char> refHolder (request.GetReference(), &DistributionData::DropReference);

        CheckLocks();

        m_callbacks[consumer.lang].m_onServiceRequestCb(request.GetBlob(),
                                                        refHolder.get(),
                                                        ctrl,
                                                        request.GetResponseId().GetCounter(),
                                                        consumer.consumer,
                                                        success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                     << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                     << ". The request being processed was:" << std::endl
                     << Typesystem::Serialization::ToXml(request.GetBlob()) << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::InvokeOnCreateRequestCb(const ConsumerId& consumer,
                                             const DistributionData& request,
                                             const long ctrl)
    {
        DispatcherIsInCallback push(m_callbackStack, CallbackId::OnCreateRequest);

        bool success;
        boost::shared_ptr<const char> refHolder (request.GetReference(), &DistributionData::DropReference);

        CheckLocks();

        m_callbacks[consumer.lang].m_onCreateRequestCb(request.GetBlob(),
                                                       refHolder.get(),
                                                       ctrl,
                                                       request.GetResponseId().GetCounter(),
                                                       consumer.consumer,
                                                       success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                     << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                     << ". The request being processed was:" << std::endl
                     << Typesystem::Serialization::ToXml(request.GetBlob()) << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::InvokeOnUpdateRequestCb(const ConsumerId& consumer,
                                             const DistributionData& request,
                                             const long ctrl)
    {
        DispatcherIsInCallback push(m_callbackStack, CallbackId::OnUpdateRequest);

        bool success;
        boost::shared_ptr<const char> refHolder (request.GetReference(), &DistributionData::DropReference);

        CheckLocks();

        m_callbacks[consumer.lang].m_onUpdateRequestCb(request.GetBlob(),
                                                       refHolder.get(),
                                                       ctrl,
                                                       request.GetResponseId().GetCounter(),
                                                       consumer.consumer,
                                                       success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                     << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                     << ". The request being processed was:" << std::endl
                     << Typesystem::Serialization::ToXml(request.GetBlob()) << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::InvokeOnDeleteRequestCb(const ConsumerId& consumer,
                                             const DistributionData& request,
                                             const long ctrl)
    {
        DispatcherIsInCallback push(m_callbackStack, CallbackId::OnDeleteRequest);

        bool success;
        boost::shared_ptr<const char> refHolder (request.GetReference(), &DistributionData::DropReference);

        CheckLocks();

        m_callbacks[consumer.lang].m_onDeleteRequestCb(refHolder.get(),
                                                       ctrl,
                                                       request.GetResponseId().GetCounter(),
                                                       consumer.consumer,
                                                       success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                     << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                     << ".The entity being processed was:" << std::endl
                     << request.GetEntityId() << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    void Dispatcher::InvokeOnNewEntityCb(const ConsumerId& consumer,
                                         const DistributionData& currentState,
                                         const bool timestampChangeInfo)
    {
        DispatcherIsInCallback push(m_callbackStack, CallbackId::OnNewEntity);

        bool success;
        boost::shared_ptr<const char> refHolder (currentState.GetReference(), &DistributionData::DropReference);

        CheckLocks();

        m_callbacks[consumer.lang].m_onNewEntityCb(currentState.GetBlob(),
                                                   refHolder.get(),
                                                   consumer.consumer,
                                                   timestampChangeInfo,
                                                   success);

        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                     << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                     << ". The entity being processed was:" << std::endl
                     << Typesystem::Serialization::ToXml(currentState.GetBlob()) << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    void Dispatcher::InvokeOnUpdatedEntityCb(const ConsumerId& consumer,
                                             const DistributionData& currentState,
                                             const DistributionData& lastState,
                                             const bool timestampChangeInfo)
    {
        DispatcherIsInCallback push(m_callbackStack, CallbackId::OnUpdatedEntity);

        bool success;
        boost::shared_ptr<const char> currentHolder (currentState.GetReference(), &DistributionData::DropReference);
        boost::shared_ptr<const char> lastHolder (lastState.GetReference(), &DistributionData::DropReference);

        CheckLocks();

        m_callbacks[consumer.lang].m_onUpdatedEntityCb(currentState.GetBlob(),
                                                       currentHolder.get(),
                                                       lastState.GetBlob(),
                                                       lastHolder.get(),
                                                       consumer.consumer,
                                                       timestampChangeInfo,
                                                       success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                     << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                     << ". The entity being processed was:" << std::endl
                     << Typesystem::Serialization::ToXml(currentState.GetBlob()) << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::InvokeOnDeletedEntityCb(const ConsumerId& consumer,
                                             const DistributionData& currentState,
                                             const DistributionData& lastState,
                                             const bool explicitlyDeleted,
                                             const bool timestampChangeInfo)
    {
        DispatcherIsInCallback push(m_callbackStack, CallbackId::OnDeletedEntity);

        bool success;
        boost::shared_ptr<const char> currentHolder (currentState.GetReference(), &DistributionData::DropReference);
        boost::shared_ptr<const char> lastHolder (lastState.GetReference(), &DistributionData::DropReference);

        CheckLocks();

        m_callbacks[consumer.lang].m_onDeletedEntityCb(currentHolder.get(),
                                                       lastState.GetBlob(),
                                                       lastHolder.get(),
                                                       explicitlyDeleted,
                                                       consumer.consumer,
                                                       timestampChangeInfo,
                                                       success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                     << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                     << ". The entity being processed was:" << std::endl
                     << currentState.GetEntityId() << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::InvokeOnInjectedNewEntityCb(const ConsumerId& consumer,
                                                 const DistributionData& injectionState)
    {
        DispatcherIsInCallback push(m_callbackStack, CallbackId::OnInjectedNewEntity);

        bool success;
        boost::shared_ptr<const char> refHolder (injectionState.GetReference(), &DistributionData::DropReference);
        boost::shared_ptr<char> blobHolder(injectionState.GetBlobCopy(), Typesystem::Internal::Delete);
        Typesystem::Internal::SetChanged(blobHolder.get(), true);

        CheckLocks();

        m_callbacks[consumer.lang].m_onInjectedNewEntityCb(blobHolder.get(),
                                                           refHolder.get(),
                                                           consumer.consumer,
                                                           success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                     << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                     << ". The entity being processed was:" << std::endl
                     << Typesystem::Serialization::ToXml(injectionState.GetBlob()) << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::InvokeOnInjectedUpdatedEntityCb(const ConsumerId& consumer,
                                                     const DistributionData& injectionState,
                                                     const DistributionData& currentState)
    {
        DispatcherIsInCallback push(m_callbackStack, CallbackId::OnInjectedUpdatedEntity);

        bool success;
        boost::shared_ptr<const char> injectionHolder (injectionState.GetReference(), &DistributionData::DropReference);
        boost::shared_ptr<const char> currentHolder (currentState.GetReference(), &DistributionData::DropReference);

        CheckLocks();

        m_callbacks[consumer.lang].m_onInjectedUpdatedEntityCb(injectionState.GetBlob(),
                                                               injectionHolder.get(),
                                                               currentState.GetBlob(),
                                                               currentHolder.get(),
                                                               consumer.consumer,
                                                               success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                     << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                     << ". The entity being processed was:" << std::endl
                     << Typesystem::Serialization::ToXml(injectionState.GetBlob()) << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::InvokeOnInjectedDeletedEntityCb(const ConsumerId& consumer,
                                                     const DistributionData& injectionState,
                                                     const DistributionData& currentState)
    {
        DispatcherIsInCallback push(m_callbackStack, CallbackId::OnInjectedDeletedEntity);

        bool success;
        boost::shared_ptr<const char> injectionHolder (injectionState.GetReference(), &DistributionData::DropReference);
        boost::shared_ptr<const char> currentHolder (currentState.GetReference(), &DistributionData::DropReference);

        CheckLocks();

        m_callbacks[consumer.lang].m_onInjectedDeletedEntityCb(injectionHolder.get(),
                                                               currentState.GetBlob(),
                                                               currentHolder.get(),
                                                               consumer.consumer,
                                                               success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                     << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                     << ". The entity being processed was:" << std::endl
                     << injectionState.GetEntityId() << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::InvokeOnInitialInjectionsDoneCb(const ConsumerId&                          consumer,
                                                     const Safir::Dob::Typesystem::TypeId&      typeId,
                                                     const Safir::Dob::Typesystem::HandlerId&   handlerId)
    {
        DispatcherIsInCallback push(m_callbackStack, CallbackId::OnInitialInjectionsDone);

        bool success;

        CheckLocks();

        m_callbacks[consumer.lang].m_onInitialInjectionsDoneCb(typeId,
                                                               handlerId.GetRawValue(),
                                                               handlerId.Utf8String().c_str(),
                                                               consumer.consumer,
                                                               success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback "
                << CallbackId::ToString(m_callbackStack.top().m_callbackId)
                << ". typeId:" << Typesystem::Operations::GetName(typeId) << " handlerId:" << handlerId << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::InvokeDropReferenceCb(const ConsumerId&    consumer,
                                           const long           refCounter)
    {
        ENSURE(g_garbageCollected[consumer.lang], << "Dispatcher::InvokeDropReferenceCb called for a non GC language");

        bool success;

        CheckLocks();

        m_callbacks[consumer.lang].m_onDropReferenceCb(consumer.consumer,
                                                       refCounter,
                                                       success);
        if (!success)
        {
            std::wostringstream moreDesc;
            moreDesc << "Exception while in Dob callback OnDropReference. Consumer:" << consumer.consumer
                << " Lang:" << consumer.lang << std::endl;
            Typesystem::LibraryExceptions::Instance().AppendDescription(moreDesc.str());
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::Clear()
    {
        m_overflowedMessageConsumers.clear();
        m_overflowedRequestConsumers.clear();
    }

    void Dispatcher::NotRequestOverflowCb(const ConsumerId & consumer)
    {
        DispatcherIsInCallback push(m_callbackStack,CallbackId::OnNotRequestOverflow);
        bool success;

        CheckLocks();

        m_callbacks[consumer.lang].m_onNotRequestOverflowCb(consumer.consumer, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Dispatcher::NotMessageOverflowCb(const ConsumerId & consumer)
    {
        DispatcherIsInCallback push(m_callbackStack,CallbackId::OnNotMessageOverflow);
        bool success;

        CheckLocks();

        m_callbacks[consumer.lang].m_onNotMessageOverflowCb(consumer.consumer, success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

#ifndef NDEBUG
    void Dispatcher::CheckLocks() const
    {
        ENSURE(LeveledLockHelper::Instance().GetNumberOfHeldLocks() == 0,
               << "A lock is held when making a callback into user code!");
    }
#endif

    CallbackData::CallbackData(const CallbackId::Enumeration callbackId)
        : m_callbackId(callbackId)
    {
    }

    DispatcherIsInCallback::DispatcherIsInCallback(CallbackStack & callbackStack,
                                                   const CallbackId::Enumeration callback):
        m_callbackStack(callbackStack),
        m_callback(callback)
    {
        m_callbackStack.push(CallbackData(callback));
    }

    DispatcherIsInCallback::~DispatcherIsInCallback()
    {
        (void)m_callback; //unused variable in release

        //Can't use ENSURE in destructor, since throwing exceptions inside a destructor is no good idea.
        assert(m_callbackStack.top().m_callbackId == m_callback);
        m_callbackStack.pop();
    }
}
}
}
