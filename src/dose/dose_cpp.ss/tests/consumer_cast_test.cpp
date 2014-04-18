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

//NOTE: TO BE ABLE TO RUN THIS YOU NEED TO MAKE ALL PARTS OF CONSUMERBASE PUBLIC!

#include <Safir/Dob/Consumer.h>
#include <iostream>

using namespace Safir::Dob;
using namespace Safir::Dob::Internal;

//#pragma warning (disable: 4100)
class Consumer :
    public StopHandler,
    public Dispatcher,
    public EntityHandler,
    public EntityHandlerInjection,
    public EntityHandlerPending,
    public ServiceHandler,
    public ServiceHandlerPending,
    public Requestor,
    public MessageSender,
    public RegistrationSubscriber,
    public MessageSubscriber,
    public EntitySubscriber
    //#endif
{
public:
    virtual void OnStopOrder() {}
    virtual void OnDoDispatch() {}
    virtual void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     /*typeId*/,
                                       const Safir::Dob::Typesystem::HandlerId& /*handlerId*/){}
    virtual void OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     /*typeId*/,
                                         const Safir::Dob::Typesystem::HandlerId& /*handlerId*/){}

    virtual void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender){}

    virtual void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender){}

    virtual void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                 Safir::Dob::ResponseSenderPtr        responseSender){}
    virtual void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                  Safir::Dob::ResponseSenderPtr         responseSender){}
    virtual void OnResponse(const Safir::Dob::ResponseProxy responseProxy) {}

    virtual void OnNotRequestOverflow() {}

    virtual void OnNotMessageOverflow() {}

    virtual void OnRegistered(const Safir::Dob::Typesystem::TypeId      /*typeId*/,
                              const Safir::Dob::Typesystem::HandlerId&  /*handlerId*/) {}

    virtual void OnUnregistered(const Safir::Dob::Typesystem::TypeId      /*typeId*/,
                                const Safir::Dob::Typesystem::HandlerId&  /*handlerId*/) {}

    virtual void OnMessage(const Safir::Dob::MessageProxy messageProxy) {}

    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy) {}

    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy) {}

    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy,
                                 const bool                    /*deletedByOwner*/) {}


};


namespace Safir 
{
namespace Dob
{
namespace Internal
{
    class Callbacks 
    {
    public:
        Callbacks(): m_success(true) {}

        int Result()
        {
            if (m_success)
            {
                return 0;
            }
            else
            {
                return 1;
            }
        }

        void Test()
        {
            Consumer cons;
            Consumer * c = &cons;
            void * v = static_cast<Internal::ConsumerBase*>(c);
            
            CheckAddr(L"StopHandler",static_cast<StopHandler*>(c),ConsumerBase::ToStopHandler(v));
            CheckAddr(L"Dispatcher",static_cast<Dispatcher*>(c),ConsumerBase::ToDispatcher(v));
            CheckAddr(L"EntityHandler",static_cast<EntityHandler*>(c),ConsumerBase::ToEntityHandler(v));
            CheckAddr(L"EntityHandlerInjection",static_cast<EntityHandlerInjection*>(c),ConsumerBase::ToEntityHandlerInjection(v));
            CheckAddr(L"EntityHandlerPending",static_cast<EntityHandlerPending*>(c),ConsumerBase::ToEntityHandlerPending(v));
            CheckAddr(L"ServiceHandler",static_cast<ServiceHandler*>(c),ConsumerBase::ToServiceHandler(v));
            CheckAddr(L"ServiceHandlerPending",static_cast<ServiceHandlerPending*>(c),ConsumerBase::ToServiceHandlerPending(v));
            CheckAddr(L"Requestor",static_cast<Requestor*>(c),ConsumerBase::ToRequestor(v));
            CheckAddr(L"MessageSender",static_cast<MessageSender*>(c),ConsumerBase::ToMessageSender(v));
            CheckAddr(L"RegistrationSubscriber",static_cast<RegistrationSubscriber*>(c),ConsumerBase::ToRegistrationSubscriber(v));
            CheckAddr(L"MessageSubscriber",static_cast<MessageSubscriber*>(c),ConsumerBase::ToMessageSubscriber(v));
            CheckAddr(L"EntitySubscriber",static_cast<EntitySubscriber*>(c),ConsumerBase::ToEntitySubscriber(v));
            
            CheckAddr(L"RevokedRegistrationBase",static_cast<RevokedRegistrationBase*>(c),ConsumerBase::ToRevokedRegistrationBase(v));
            CheckAddr(L"CompletedRegistrationBase",static_cast<CompletedRegistrationBase*>(c),ConsumerBase::ToCompletedRegistrationBase(v));
            CheckAddr(L"EntityRequestBase",static_cast<EntityRequestBase*>(c),ConsumerBase::ToEntityRequestBase(v));
            CheckAddr(L"EntityInjectionBase",static_cast<EntityInjectionBase*>(c),ConsumerBase::ToEntityInjectionBase(v));
            CheckAddr(L"ServiceRequestBase",static_cast<ServiceRequestBase*>(c),ConsumerBase::ToServiceRequestBase(v));
        }

    private:
        bool m_success;

        void CheckAddr(const std::wstring & desc, void * first, void * second)
        {
            std::wstring cmp = L" == ";
            if (first != second)
            {
                m_success = false;
                std::wcout << "FAILED : ";
                cmp = L" != ";
            }
            else
            {
                std::wcout << "SUCCESS: ";
            }
            std::wcout << desc << std::hex << " (0x" << first << cmp << "0x" << second << ")" << std::endl;
        }

    };
}
}
}   


int main(int, char**)
{
    Safir::Dob::Internal::Callbacks tester;
    tester.Test();
    
    return tester.Result();
}
