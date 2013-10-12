/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#include "Callbacks.h"
#include "MessageProxyImpl.h"
#include "ResponseProxyImpl.h"
#include "ResponseSenderImpl.h"
#include "ServiceRequestProxyImpl.h"
#include "EntityRequestProxyImpl.h"
#include "EntityProxyImpl.h"
#include "InjectedEntityProxyImpl.h"
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>


namespace Safir
{
namespace Dob
{
namespace Internal
{
    void Callbacks::OnDispatch(void* const consumer,
                               bool & success)
    {
        success = false;
        try
        {
            ConsumerBase::ToDispatcher(consumer)->OnDoDispatch();
            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnStopOrder(void* const consumer,
                                bool & success)
    {
        success = false;
        try
        {
            ConsumerBase::ToStopHandler(consumer)->OnStopOrder();
            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnNewEntity(const char* const currentBlob,
                                const char* const currentState,
                                void* const consumer,
                                const bool timestampDiff,
                                bool& success)
    {
        success = false;
        try
        {
            ENSURE(currentBlob != NULL, << "Got NULL currentBlob in OnNewEntity from dose!!!");

            EntityProxy entityProxy(new EntityProxyImpl(currentBlob,currentState,NULL,NULL,true,timestampDiff));

            ConsumerBase::ToEntitySubscriber(consumer)->OnNewEntity(entityProxy);

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnUpdatedEntity(const char* const currentBlob,
                                    const char* const currentState,
                                    const char* const previousBlob,
                                    const char* const previousState,
                                    void* const consumer,
                                    const bool timestampDiff,
                                    bool& success)
    {
        success = false;
        try
        {
            ENSURE(currentBlob != NULL, << "Got NULL currentBlob in OnUpdatedEntity from dose!!!");

            EntityProxy entityProxy(new EntityProxyImpl(currentBlob,currentState,previousBlob,previousState,true, timestampDiff));

            ConsumerBase::ToEntitySubscriber(consumer)->OnUpdatedEntity(entityProxy);

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnDeletedEntity(const char* const currentState,
                                    const char* const previousBlob,
                                    const char* const previousState,
                                    const bool explicitlyDeleted,
                                    void* const consumer,
                                    const bool timestampDiff,
                                    bool& success)
    {
        success = false;
        try
        {
            ENSURE(previousBlob != NULL, << "Got NULL previousBlob in OnDeletedEntity from dose!!!");

            EntityProxy entityProxy(new EntityProxyImpl(NULL,currentState,previousBlob,previousState,true, timestampDiff));

            ConsumerBase::ToEntitySubscriber(consumer)->OnDeletedEntity(entityProxy, explicitlyDeleted);

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnCreateRequest(const char* const requestBlob,
                                    const char* const state,
                                    const long ctrl,
                                    const Safir::Dob::Typesystem::Int32 responseId,
                                    void* const consumer,
                                    bool& success)
    {
        success = false;
        try
        {
            ENSURE(requestBlob != NULL, << "Got NULL request in OnCreateRequest from dose!!!");

            EntityRequestProxy entityRequestProxy(new EntityRequestProxyImpl(requestBlob,state));
            ResponseSenderPtr responseSender(new ResponseSenderImpl(ctrl,consumer,responseId));

            ConsumerBase::ToEntityRequestBase(consumer)->OnCreateRequest(entityRequestProxy,
                                                                         responseSender);

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnUpdateRequest(const char* const requestBlob,
                                    const char* const state,
                                    const long ctrl,
                                    const Safir::Dob::Typesystem::Int32 responseId,
                                    void* const consumer,
                                    bool& success)
    {
        success = false;
        try
        {
            ENSURE(requestBlob != NULL, << "Got NULL request in OnUpdateRequest from dose!!!");

            EntityRequestProxy entityRequestProxy(new EntityRequestProxyImpl(requestBlob,state));
            ResponseSenderPtr responseSender(new ResponseSenderImpl(ctrl,consumer,responseId));

            ConsumerBase::ToEntityRequestBase(consumer)->OnUpdateRequest(entityRequestProxy,
                                                                         responseSender);

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnDeleteRequest(const char* const state,
                                    const long ctrl,
                                    const Safir::Dob::Typesystem::Int32 responseId,
                                    void* const consumer,
                                    bool& success)
    {
        success = false;
        try
        {
            EntityRequestProxy entityRequestProxy(new EntityRequestProxyImpl(NULL,state));
            ResponseSenderPtr responseSender(new ResponseSenderImpl(ctrl,consumer,responseId));

            ConsumerBase::ToEntityRequestBase(consumer)->OnDeleteRequest(entityRequestProxy,
                                                                         responseSender);

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnServiceRequest(const char* const requestBlob,
                                     const char* const state,
                                     const long ctrl,
                                     const Safir::Dob::Typesystem::Int32 responseId,
                                     void* const consumer,
                                     bool& success)
    {
        success = false;
        try
        {
            ENSURE(requestBlob != NULL, << "Got NULL request in OnServiceRequest from dose!!!");

            ServiceRequestProxy serviceRequestProxy(new ServiceRequestProxyImpl(requestBlob,state));
            ResponseSenderPtr responseSender(new ResponseSenderImpl(ctrl,consumer,responseId));

            ConsumerBase::ToServiceRequestBase(consumer)->OnServiceRequest(serviceRequestProxy,
                                                                       responseSender);

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnResponse(const Safir::Dob::RequestId requestId,
                               const char* const responseBlob,
                               const char* const responseState,
                               const char* const requestBlob,
                               const char* const requestState,
                               void* const consumer,
                               bool& success)
    {
        success = false;
        try
        {
            ENSURE(responseBlob != NULL, << "Got NULL response in OnResponse from dose!!!");

            ResponseProxy responseProxy(new ResponseProxyImpl(requestId,
                                                              responseBlob,
                                                              responseState,
                                                              requestBlob,
                                                              requestState));

            ConsumerBase::ToRequestor(consumer)->OnResponse(responseProxy);

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnMessage(const char * const message,
                              const char * const state,
                              void* const consumer,
                              bool & success)
    {
        success = false;
        try
        {
            ENSURE(message != NULL, << "Got NULL message in OnMessage from dose!!!");

            MessageProxy messageProxy(new MessageProxyImpl(message, state));

            ConsumerBase::ToMessageSubscriber(consumer)->OnMessage(messageProxy);

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnRegistered(const Safir::Dob::Typesystem::TypeId typeId,
                                 const Safir::Dob::Typesystem::Int64 handlerId,
                                 const char* const handlerIdStr,
                                 void* const consumer,
                                 bool& success)
    {
        success = false;
        try
        {
            ConsumerBase::ToRegistrationSubscriber(consumer)->OnRegistered
                (typeId,
                 Typesystem::HandlerId(handlerId,
                                       Typesystem::Utilities::ToWstring(handlerIdStr)));

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;

    }

    void Callbacks::OnUnregistered(const Safir::Dob::Typesystem::TypeId typeId,
                                  const Safir::Dob::Typesystem::Int64 handlerId,
                                  const char* const handlerIdStr,
                                  void* const consumer,
                                  bool& success)
    {
        success = false;
        try
        {
            ConsumerBase::ToRegistrationSubscriber(consumer)->OnUnregistered
                (typeId,
                 Typesystem::HandlerId(handlerId,
                                       Typesystem::Utilities::ToWstring(handlerIdStr)));

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;

    }


    void Callbacks::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId,
                                          const Safir::Dob::Typesystem::Int64 handlerId,
                                          const char* const handlerIdStr,
                                          void* const consumer,
                                          bool& success)
    {
        success = false;
        try
        {
            ConsumerBase::ToRevokedRegistrationBase(consumer)->OnRevokedRegistration
                (typeId,
                 Typesystem::HandlerId(handlerId,
                                       Typesystem::Utilities::ToWstring(handlerIdStr)));

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId typeId,
                                            const Safir::Dob::Typesystem::Int64 handlerId,
                                            const char* const handlerIdStr,
                                            void* const consumer,
                                            bool& success)
    {
        success = false;
        try
        {
            ConsumerBase::ToCompletedRegistrationBase(consumer)->
                OnCompletedRegistration(typeId,
                                        Typesystem::HandlerId(handlerId,
                                                              Typesystem::Utilities::ToWstring(handlerIdStr)));

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnInjectedNewEntity(const char* const injectionBlob,
                                        const char* const injectionState,
                                        void* const consumer,
                                        bool& success)
    {
        success = false;
        try
        {
            ENSURE(injectionBlob != NULL, << "Got NULL blob in OnInjectedNewEntity from dose!!!");

            InjectedEntityProxy injectedEntityProxy(new InjectedEntityProxyImpl(injectionBlob,injectionState,NULL,NULL));

            ConsumerBase::ToEntityInjectionBase(consumer)->OnInjectedNewEntity(injectedEntityProxy);

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnInjectedUpdatedEntity(const char* const injectionBlob,
                                            const char* const injectionState,
                                            const char* const currentBlob,
                                            const char* const currentState,
                                            void* const consumer,
                                            bool& success)
    {
        success = false;
        try
        {
            ENSURE(injectionBlob != NULL, << "Got NULL blob in OnInjectedUpdatedEntity from dose!!!");

            InjectedEntityProxy injectedEntityProxy(new InjectedEntityProxyImpl
                (injectionBlob,injectionState, currentBlob, currentState));

            ConsumerBase::ToEntityInjectionBase(consumer)->OnInjectedUpdatedEntity(injectedEntityProxy);

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnInjectedDeletedEntity(const char* const injectionState,
                                            const char* const currentBlob,
                                            const char* const currentState,
                                            void* const consumer,
                                            bool& success)
    {
        success = false;
        try
        {
            InjectedEntityProxy injectedEntityProxy(new InjectedEntityProxyImpl(NULL,injectionState,currentBlob, currentState));

            ConsumerBase::ToEntityInjectionBase(consumer)->OnInjectedDeletedEntity(injectedEntityProxy);

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId,
                                            const Safir::Dob::Typesystem::Int64 handlerId,
                                            const char* const handlerIdStr,
                                            void* const consumer,
                                            bool& success)
    {
        success = false;
        try
        {
            ConsumerBase::ToEntityInjectionBase(consumer)->
                OnInitialInjectionsDone(typeId,
                                        Typesystem::HandlerId(handlerId,
                                                              Typesystem::Utilities::ToWstring(handlerIdStr)));

            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnNotRequestOverflow(void* const consumer,
                                         bool & success)
    {
        success = false;
        try
        {
            ConsumerBase::ToRequestor(consumer)->OnNotRequestOverflow();
            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }

    void Callbacks::OnNotMessageOverflow(void* const consumer,
                                         bool & success)
    {
        success = false;
        try
        {
            ConsumerBase::ToMessageSender(consumer)->OnNotMessageOverflow();
            success = true;
        }
        CATCH_LIBRARY_EXCEPTIONS;
    }
}
}
}
