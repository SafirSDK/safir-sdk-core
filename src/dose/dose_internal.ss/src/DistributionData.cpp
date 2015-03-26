/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrhafsetchangeflags
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

#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/StateDeleter.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/BlobOperations.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/BlobOperations.h>
#include <Safir/Dob/Typesystem/Members.h>
#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <boost/static_assert.hpp>

#ifdef REGISTER_TIMES
  #include <boost/random/ranlux.hpp>
  #include <boost/thread/mutex.hpp>
  #include <ctime>
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    //This method is meant to contain BOOST_STATIC_ASSERTs, and is not meant to be called
    //BOOST_STATIC_ASSERTs are evaluated at compile-time, so this is ok!
    void DistributionData::CheckSizes()
    {
        BOOST_STATIC_ASSERT(sizeof(bool) == 1);
        BOOST_STATIC_ASSERT(sizeof(VersionNumber) == sizeof(boost::uint16_t));
        BOOST_STATIC_ASSERT(sizeof(ResponseId) == 4);
        BOOST_STATIC_ASSERT(sizeof(LamportTimestamp) == 16);
        BOOST_STATIC_ASSERT(sizeof(InternalRequestId) == 4);
        BOOST_STATIC_ASSERT(sizeof(Identifier) == 8);
        BOOST_STATIC_ASSERT(sizeof(NodeNumber) == 8);
        BOOST_STATIC_ASSERT(sizeof(ContextId) == 4);
        BOOST_STATIC_ASSERT(sizeof(ConnectionId) == 24);
        BOOST_STATIC_ASSERT(sizeof(Header) == 4 + sizeof(ConnectionId));

        BOOST_STATIC_ASSERT(sizeof(ConnectHeader) == sizeof(Header) + 4);

        BOOST_STATIC_ASSERT(sizeof(MessageHeader) == sizeof(Header)
                            + 8); //m_channelId

        BOOST_STATIC_ASSERT(sizeof(StateHeader) == sizeof(Header)
                            + 8   //m_typeId
                            + 8   //handlerid
                            + 16); //m_regTime

        BOOST_STATIC_ASSERT(sizeof(InstanceIdPolicy::Enumeration) == 4);

        BOOST_STATIC_ASSERT(sizeof(RegistrationStateKind) == 4);


        BOOST_STATIC_ASSERT(sizeof(RegistrationStateHeader) == sizeof(StateHeader)
                            + 4  //handlerStrSize
                            + 4  //m_instanceIdPolicy
                            + 4  //m_kind
                            );

        BOOST_STATIC_ASSERT(sizeof(EntityStateKind) == 4);

        BOOST_STATIC_ASSERT(sizeof(EntityStateHeader) == sizeof(StateHeader)
                            + 8  //m_instanceId
                            + 16  //m_creationTime
                            + 2  //m_version
                            + 2  //padding
                            + 4  //m_kind
                            + 1  //m_explicitlyDeleted
                            + 1  //m_sourceIsPermanentStore
                            + 1  //m_hasBlob
                            + 1  //m_versionIsDecremented
                            + 4  //m_numTimestamps
                            );

        BOOST_STATIC_ASSERT(sizeof (PendingRegistrationMsg) == sizeof(Header)
                            + sizeof(Typesystem::TypeId)
                            + sizeof(Typesystem::Int64)
                            + sizeof(RegisterTime)
                            + 2 * 4
                            + sizeof(ConnectionId));

        BOOST_STATIC_ASSERT(sizeof (RequestPDHeader) == sizeof(Header)
                            + sizeof(ConnectionId));

        BOOST_STATIC_ASSERT(sizeof (HavePersistenceDataResponseMsg) == sizeof(Header)
                            + 4); //bool plus padding

        BOOST_STATIC_ASSERT(sizeof(RequestHeader) == sizeof(Header)
                            + sizeof(Typesystem::Int64)
                            + sizeof(InternalRequestId)
                            + sizeof(ResponseId));

        BOOST_STATIC_ASSERT(sizeof(EntityCreateRequestHeader) == sizeof(RequestHeader)
                            + sizeof(Typesystem::Int64)
                            + sizeof(bool)*4);

        BOOST_STATIC_ASSERT(sizeof(EntityUpdateRequestHeader) == sizeof(RequestHeader)
                            + sizeof(Typesystem::Int64));

        BOOST_STATIC_ASSERT(sizeof(EntityDeleteRequestHeader) == sizeof(RequestHeader)
                            + sizeof(Typesystem::Int64)*2);

        BOOST_STATIC_ASSERT(sizeof(ResponseHeader) == sizeof(Header)
                            + sizeof(ConnectionId)
                            + sizeof(InternalRequestId)
                            + 4); //padding
        BOOST_STATIC_ASSERT(sizeof(ResponseHeader) == 60);


        BOOST_STATIC_ASSERT(sizeof (unsigned int) == sizeof(boost::uint32_t));

        BOOST_STATIC_ASSERT(sizeof(Safir::Utilities::Internal::AtomicUint32) == 4);
    }
    

#ifdef REGISTER_TIMES
    Typesystem::Int32 GenerateId()
    {
        static boost::ranlux64_3 * random_generator = NULL;
        if (random_generator == NULL)
        {
            == Replace this instantiation with boost::once stuff!
            static boost::mutex instantiation_lock;
            boost::lock_guard<boost::mutex> lck(instantiation_lock);
            if (random_generator == NULL)
            {
                random_generator = new boost::ranlux64_3();
                random_generator->seed(static_cast<boost::uint32_t>(time(NULL)));
            }
        }
        static boost::mutex use_lock;
        boost::lock_guard<boost::mutex> lck(use_lock);

        return static_cast<Typesystem::Int32>(0x00000000ffffffffLL & (*random_generator)());
    }


    Typesystem::Int32 DistributionData::GetId() const
    {
        return GetHeader().m_id;
    }
#endif


    template <class T>
    static inline T * AnyPtrCast(char * const data)
    {
        return static_cast<T*>(static_cast<void*>(data));
    }

    template <class T>
    static inline const T * AnyPtrCast(const char * const data)
    {
        return AnyPtrCast<T>(const_cast<char*>(data));
    }

    DistributionData::Header &
    DistributionData::GetHeader()
    {
        ENSURE(m_data, << "Trying to GetHeader from a NULL DistributionData!");
        return *AnyPtrCast<Header>(GetData());
    }

    DistributionData::RequestPDHeader &
    DistributionData::GetRequestPDHeader()
    {
        ENSURE(m_data, << "Trying to GetHeader from a NULL DistributionData!");
        ENSURE(GetType() == Action_RequestPoolDistribution,
               << "GetRequestPDHeader called on DistributionData that was not a Action_RequestPoolDistribution (type = " << GetType() << ")");
        return *AnyPtrCast<RequestPDHeader>(GetData());
    }

    DistributionData::ConnectHeader &
    DistributionData::GetConnectHeader()
    {
        ENSURE(GetType() == Action_Connect, << "GetConnectHeader called on DistributionData that was not a Action_Connect (type = " << GetType() << ")");

        return *AnyPtrCast<ConnectHeader>(GetData());
    }

    DistributionData::MessageHeader &
    DistributionData::GetMessageHeader()
    {
        ENSURE(GetType() == Message, << "GetMessageHeader called on DistributionData that was not a Message (type = " << GetType() << ")");

        return *AnyPtrCast<MessageHeader>(GetData());
    }

    DistributionData::StateHeader&
    DistributionData::GetStateHeader()
    {
        ENSURE(GetType() == RegistrationState ||
               GetType() == EntityState, << "GetStateHeader called on DistributionData that was not a Registration state or Entity state(type = " << GetType() << ")");
        ENSURE(m_data, << "Cannot get state header from a NoState object");

        return *AnyPtrCast<StateHeader>(GetData());
    }

    DistributionData::RegistrationStateHeader&
    DistributionData::GetRegistrationStateHeader()
    {
        ENSURE(GetType() == RegistrationState, << "GetRegistrationStateHeader called on DistributionData that was not a Registration state (type = " << GetType() << ")");
        ENSURE(m_data, << "Cannot get state header from a NoState object");

        return *AnyPtrCast<RegistrationStateHeader>(GetData());
    }

    DistributionData::EntityStateHeader&
    DistributionData::GetEntityStateHeader()
    {
        ENSURE(GetType() == EntityState, << "GetEntityStateHeader called on DistributionData that was not an Entity state (type = " << GetType() << ")");
        ENSURE(m_data, << "Cannot get state header from a NoState object");

        return *AnyPtrCast<EntityStateHeader>(GetData());
    }

    DistributionData::PendingRegistrationMsg &
    DistributionData::GetPendingRegistrationMsg()
    {
        ENSURE(m_data, << "Trying to GetPendingRegistrationMsg from a NULL DistributionData!");
        ENSURE(GetType() == Action_PendingRegistrationRequest ||
               GetType() == Action_PendingRegistrationResponse,
               << "GetPendingRegistrationMsg called on DistributionData that was not a pending req or resp (type = " << GetType() << ")");
        return *AnyPtrCast<PendingRegistrationMsg>(GetData());
    }

    DistributionData::HavePersistenceDataResponseMsg &
    DistributionData::GetHavePersistenceDataResponseMsg()
    {
        ENSURE(m_data, << "Trying to GetHavePersistenceDataResponseMsg from a NULL DistributionData!");
        ENSURE(GetType() == Action_HavePersistenceDataResponse,
               << "GetHavePersistenceDataResponseMsg called on DistributionData that was not a have persistence response (type = " << GetType() << ")");
        return *AnyPtrCast<HavePersistenceDataResponseMsg>(GetData());
    }


    DistributionData::RequestHeader &
    DistributionData::GetRequestHeader()
    {
        ENSURE(m_data, << "Trying to GetRequestHeader from a NULL DistributionData!");
        ENSURE(GetType() == Request_Service ||
               GetType() == Request_EntityCreate ||
               GetType() == Request_EntityUpdate ||
               GetType() == Request_EntityDelete,
               << "GetRequestHeader called on DistributionData that was not a request (type = " << GetType() << ")");
        return *AnyPtrCast<RequestHeader>(GetData());
    }

    DistributionData::EntityCreateRequestHeader &
    DistributionData::GetEntityCreateRequestHeader()
    {
        ENSURE(m_data, << "Trying to GetEntityCreateRequestHeader from a NULL DistributionData!");
        ENSURE(GetType() == Request_EntityCreate,
               << "GetEntityCreateRequestHeader called on DistributionData that was not a create request (type = " << GetType() << ")");
        return *AnyPtrCast<EntityCreateRequestHeader>(GetData());
    }

    DistributionData::EntityUpdateRequestHeader &
    DistributionData::GetEntityUpdateRequestHeader()
    {
        ENSURE(m_data, << "Trying to GetEntityUpdateRequestHeader from a NULL DistributionData!");
        ENSURE(GetType() == Request_EntityUpdate,
               << "GetEntityUpdateRequestHeader called on DistributionData that was not a request (type = " << GetType() << ")");
        return *AnyPtrCast<EntityUpdateRequestHeader>(GetData());
    }

    DistributionData::EntityDeleteRequestHeader &
    DistributionData::GetEntityDeleteRequestHeader()
    {
        ENSURE(m_data, << "Trying to EntityDeleteRequestHeader from a NULL DistributionData!");
        ENSURE(GetType() == Request_EntityDelete,
            << "GetEntityDeleteRequestHeader called on DistributionData that was not a delete request (type = " << GetType() << ")");
        return *AnyPtrCast<EntityDeleteRequestHeader>(GetData());
    }

    DistributionData::ResponseHeader &
    DistributionData::GetResponseHeader()
    {
        ENSURE(m_data, << "Trying to GetResponseHeader from a NULL DistributionData!");
        ENSURE(GetType() == Response,
               << "GetResponseHeader called on DistributionData that was not a response (type = " << GetType() << ")");
        return *AnyPtrCast<ResponseHeader>(GetData());
    }

    void DistributionData::Allocate(const size_t size)
    {
        m_data = IntrusiveOperations::Allocate(size);

#ifdef REGISTER_TIMES
        GetHeader().m_id = GenerateId();
#endif
    }


    DistributionData::DistributionData(tabula_rasa_tag_t, const size_t size)
    {
        Allocate(size);
        Header & header = GetHeader();
        header.m_type = (Type)-1;
        header.m_sender = ConnectionId();
    }



    DistributionData::DistributionData(connect_message_tag_t,
                                       const ConnectionId & sender,
                                       const std::string & connectionNameWithoutCounter,
                                       const Typesystem::Int32 counter)
    {
        Allocate(sizeof(ConnectHeader) + MAX_CONNECTION_NAME_LENGTH + 1); //null termination

        Header & header = GetHeader();
        //Header
        header.m_type=Action_Connect;
        header.m_sender=sender;

        //ConnectHeader
        GetConnectHeader().m_counter = counter;

        //AppName
        strncpy(GetData() + sizeof(ConnectHeader), connectionNameWithoutCounter.c_str(), MAX_CONNECTION_NAME_LENGTH + 1);
    }


    DistributionData::DistributionData(disconnect_message_tag_t, const ConnectionId & sender)
    {
        Allocate(sizeof(Header));
        Header & header = GetHeader();
        header.m_type = Action_Disconnect;
        header.m_sender = sender;

    }

    DistributionData::DistributionData(no_state_tag_t)
    {
        //m_data will be NULL.
    }

    DistributionData::DistributionData(registration_state_tag_t,
                                       const ConnectionId& sender,
                                       const Typesystem::TypeId typeId,
                                       const Typesystem::HandlerId& handlerId,
                                       const InstanceIdPolicy::Enumeration instanceIdPolicy,
                                       const RegistrationStateKind kind,
                                       const LamportTimestamp& regTime)
    {
        const Typesystem::Int32 handlerStrSize = handlerId.Utf8StringLength();

        Allocate(sizeof(RegistrationStateHeader) + handlerStrSize);

        Header& header = GetHeader();
        header.m_type=RegistrationState;
        header.m_sender=sender;

        StateHeader& stateHeader = GetStateHeader();
        stateHeader.m_typeId = typeId;
        stateHeader.m_handlerId = handlerId.GetRawValue();
        stateHeader.m_regTime = regTime;

        RegistrationStateHeader& registrationStateHeader = GetRegistrationStateHeader();

        registrationStateHeader.m_kind = kind;
        registrationStateHeader.m_instanceIdPolicy = instanceIdPolicy;

        registrationStateHeader.m_handlerStrSize = handlerStrSize;
        strncpy(GetData() + sizeof(RegistrationStateHeader), handlerId.Utf8String().c_str(), handlerStrSize);
    }

    DistributionData::DistributionData(message_tag_t,
                                       const ConnectionId & sender,
                                       const Typesystem::ChannelId & channel,
                                       const char * const blob)
    {
        const size_t blobSize=Dob::Typesystem::BlobOperations::GetSize(blob);

        Allocate(sizeof(MessageHeader) + blobSize);
        Header & header = GetHeader();

        header.m_type = Message;
        header.m_sender = sender;
        MessageHeader & messageHeader = GetMessageHeader();
        messageHeader.m_channelId = channel.GetRawValue();

        memcpy(GetData() + sizeof(MessageHeader), blob, blobSize);
    }

    DistributionData::DistributionData(entity_state_tag_t,
                                       const ConnectionId& sender,
                                       const Typesystem::TypeId typeId,
                                       const Typesystem::HandlerId& handlerId,
                                       const LamportTimestamp& regTime,
                                       const Typesystem::InstanceId& instanceId,
                                       const LamportTimestamp& creationTime,
                                       const EntityStateKind kind,
                                       const bool explicitlyDeleted,
                                       const bool sourceIsPermanentStore,
                                       const char* const blob)
    {
        // Common Header
        size_t blobSize = 0;
        if (blob != NULL)
        {
            blobSize = Dob::Typesystem::BlobOperations::GetSize(blob);
        }

        int numTimestamps = 0;
        if (InjectionKindTable::Instance().IsInjectable(typeId))
        {
            //The top-level timestamp
            numTimestamps = 1;

            //the per-member timestamps
            if (blob != NULL)
            {
                numTimestamps += Safir::Dob::Typesystem::Members::GetNumberOfMembers(typeId);
            }
        }

        Allocate(sizeof(EntityStateHeader) + blobSize + numTimestamps * sizeof(Typesystem::Int64));

        Header& header = GetHeader();
        header.m_type=EntityState;
        header.m_sender=sender;

        StateHeader& stateHeader = GetStateHeader();
        stateHeader.m_typeId = typeId;
        stateHeader.m_handlerId = handlerId.GetRawValue();
        stateHeader.m_regTime = regTime;

        EntityStateHeader& entityStateHeader = GetEntityStateHeader();
        entityStateHeader.m_instanceId = instanceId.GetRawValue();
        entityStateHeader.m_creationTime = creationTime;
        entityStateHeader.m_version = VersionNumber(); //version number 0
        entityStateHeader.m_kind = kind;
        entityStateHeader.m_explicitlyDeleted = explicitlyDeleted;
        entityStateHeader.m_sourceIsPermanentStore = sourceIsPermanentStore;
        entityStateHeader.m_hasBlob = blob != NULL;
        entityStateHeader.m_versionIsDecremented = false;
        entityStateHeader.m_numTimestamps = numTimestamps;

        if (numTimestamps > 0)
        {
            Typesystem::Int64 * timestamps = AnyPtrCast<Typesystem::Int64>(GetData() + sizeof(EntityStateHeader));
            for (int i = 0; i < numTimestamps;++i)
            {
                timestamps[i] = 0;

//There is some odd compiler bug on 64 bit gcc (at least version 4.3.2-1ubuntu11) when
//release compiling this code, so we add some junk code so that the failing
//optimization doesnt kick in.
#if defined (__GNUC__) && defined (__amd64) && defined(__OPTIMIZE__)
                volatile Typesystem::Int64 j = timestamps[i];
                j=j;
#endif
            }
        }

        if (blob != NULL)
        {
            memcpy(GetData() + sizeof(EntityStateHeader) + numTimestamps * sizeof(Typesystem::Int64), blob, blobSize);
        }
    }

    DistributionData::DistributionData(pending_registration_request_tag_t,
                                       const ConnectionId & sender,
                                       const Typesystem::TypeId typeId,
                                       const Typesystem::HandlerId & handlerId,
                                       const LamportTimestamp & timestamp,
                                       const long requestId)
    {
        Allocate(sizeof(PendingRegistrationMsg));

        Header & header = GetHeader();
        header.m_type=Action_PendingRegistrationRequest;
        header.m_sender=sender;

        PendingRegistrationMsg & prMsg = GetPendingRegistrationMsg();
        prMsg.m_typeId = typeId;
        prMsg.m_handlerId = handlerId.GetRawValue();
        prMsg.m_timestamp = timestamp;
        prMsg.m_requestId = requestId;
        prMsg.m_response = true;
        prMsg.m_originator = sender;
    }


    DistributionData::DistributionData(pending_registration_response_tag_t,
                                       const DistributionData & request,
                                       const ConnectionId & sender,
                                       const bool response)
    {
        Allocate(sizeof(PendingRegistrationMsg));

        Header & header = GetHeader();
        header.m_type=Action_PendingRegistrationResponse;
        header.m_sender=sender;

        PendingRegistrationMsg & prMsg = GetPendingRegistrationMsg();

        const PendingRegistrationMsg & reqMsg= request.GetPendingRegistrationMsg();

        prMsg.m_typeId = reqMsg.m_typeId;
        prMsg.m_handlerId = reqMsg.m_handlerId;
        prMsg.m_timestamp = reqMsg.m_timestamp;
        prMsg.m_requestId = reqMsg.m_requestId;
        prMsg.m_response = response;
        prMsg.m_originator = reqMsg.m_originator;
    }

    DistributionData::DistributionData(have_persistence_data_request_tag_t,
                                       const ConnectionId& sender)
    {
        Allocate(sizeof(Header));

        Header & header = GetHeader();
        header.m_type=Action_HavePersistenceDataRequest;
        header.m_sender=sender;
    }

    DistributionData::DistributionData(have_persistence_data_response_tag_t,
                                       const ConnectionId& sender,
                                       const bool iHavePersistenceData)
    {
        Allocate(sizeof(HavePersistenceDataResponseMsg));

        Header & header = GetHeader();
        header.m_type=Action_HavePersistenceDataResponse;
        header.m_sender=sender;

        HavePersistenceDataResponseMsg& response = GetHavePersistenceDataResponseMsg();
        response.m_iHavePersistenceData = iHavePersistenceData;
    }


    DistributionData::DistributionData(request_pool_distribution_request_tag_t,
                                       const ConnectionId& sender,
                                       const ConnectionId& receiver)
    {
        Allocate(sizeof(RequestPDHeader));

        Header & header = GetHeader();
        header.m_type=Action_RequestPoolDistribution;
        header.m_sender=sender;

        RequestPDHeader & pdRequest = GetRequestPDHeader();
        pdRequest.m_receiver = receiver;
    }


    DistributionData::DistributionData(service_request_tag_t,
                                       const ConnectionId & sender,
                                       const Typesystem::HandlerId & handler,
                                       const InternalRequestId requestId,
                                       const char * const blob)
    {
        const size_t blobSize = Typesystem::BlobOperations::GetSize(blob);
        Allocate(sizeof(RequestHeader) + blobSize);

        Header & header = GetHeader();
        header.m_type = Request_Service;
        header.m_sender = sender;

        RequestHeader & reqHeader = GetRequestHeader();
        reqHeader.m_handlerId = handler.GetRawValue();
        reqHeader.m_requestId = requestId;
        reqHeader.m_responseId = ResponseId();
        memcpy(GetData() + sizeof(RequestHeader), blob, blobSize);
    }


    DistributionData::DistributionData(entity_create_request_tag_t,
                                       const ConnectionId & sender,
                                       const Typesystem::HandlerId & handler,
                                       const InternalRequestId requestId,
                                       const bool hasInstanceId,
                                       const Typesystem::InstanceId & instance,
                                       const char * const blob)
    {
        const size_t blobSize = Typesystem::BlobOperations::GetSize(blob);
        Allocate(sizeof(EntityCreateRequestHeader) + blobSize);

        Header & header = GetHeader();
        header.m_type = Request_EntityCreate;
        header.m_sender = sender;

        EntityCreateRequestHeader & reqHeader = GetEntityCreateRequestHeader();
        reqHeader.m_requestHeader.m_handlerId = handler.GetRawValue();
        reqHeader.m_requestHeader.m_requestId = requestId;
        reqHeader.m_requestHeader.m_responseId = ResponseId();
        reqHeader.m_hasInstanceId = hasInstanceId;
        if (hasInstanceId)
        {
            reqHeader.m_instanceId = instance.GetRawValue();
        }
        memcpy(GetData() + sizeof(EntityCreateRequestHeader), blob, blobSize);
    }

    DistributionData::DistributionData(entity_update_request_tag_t,
                                       const ConnectionId & sender,
                                       const InternalRequestId requestId,
                                       const Typesystem::InstanceId & instance,
                                       const char * const blob)
    {
        const size_t blobSize = Typesystem::BlobOperations::GetSize(blob);
        Allocate(sizeof(EntityUpdateRequestHeader) + blobSize);

        Header & header = GetHeader();
        header.m_type = Request_EntityUpdate;
        header.m_sender = sender;

        EntityUpdateRequestHeader & reqHeader = GetEntityUpdateRequestHeader();
        reqHeader.m_requestHeader.m_handlerId = -1;
        reqHeader.m_requestHeader.m_requestId = requestId;
        reqHeader.m_requestHeader.m_responseId = ResponseId();
        reqHeader.m_instanceId = instance.GetRawValue();
        memcpy(GetData() + sizeof(EntityUpdateRequestHeader), blob, blobSize);
    }

    DistributionData::DistributionData(entity_delete_request_tag_t,
                                       const ConnectionId & sender,
                                       const InternalRequestId requestId,
                                       const Typesystem::EntityId& entityId)
    {
        Allocate(sizeof(EntityDeleteRequestHeader));

        Header & header = GetHeader();
        header.m_type = Request_EntityDelete;
        header.m_sender = sender;

        EntityDeleteRequestHeader & reqHeader = GetEntityDeleteRequestHeader();

        reqHeader.m_requestHeader.m_handlerId = -1;
        reqHeader.m_requestHeader.m_requestId = requestId;
        reqHeader.m_requestHeader.m_responseId = ResponseId();
        reqHeader.m_instanceId = entityId.GetInstanceId().GetRawValue();
        reqHeader.m_typeId = entityId.GetTypeId();
    }

    DistributionData::DistributionData(response_tag_t,
                                       const ConnectionId & sender,
                                       const ConnectionId & receiver,
                                       const InternalRequestId req,
                                       const char * const blob)
    {
        const size_t blobSize=Dob::Typesystem::BlobOperations::GetSize(blob);
        Allocate(sizeof(ResponseHeader) + blobSize);

        Header & header = GetHeader();
        header.m_type=Response;
        header.m_sender=sender;

        ResponseHeader & respHeader = GetResponseHeader();
        respHeader.m_requestId = req;
        respHeader.m_receiver = receiver;
        memcpy(GetData() + sizeof(ResponseHeader), blob, blobSize);
    }

    DistributionData::~DistributionData()
    {

    }


    //Get size of message
    size_t DistributionData::Size() const
    {
        ENSURE(m_data, << "Cannot get size for a NULL DistributionData!");

        switch (GetType())
        {
        case Action_Connect:
            return sizeof(ConnectHeader) + MAX_CONNECTION_NAME_LENGTH;

        case Action_Disconnect:
            return sizeof(Header);

        case Message:
            return sizeof(MessageHeader) + Dob::Typesystem::BlobOperations::GetSize(GetBlob());

        case Request_Service:
            return sizeof (RequestHeader) + Dob::Typesystem::BlobOperations::GetSize(GetBlob());

        case Request_EntityCreate:
            return sizeof(EntityCreateRequestHeader) + Dob::Typesystem::BlobOperations::GetSize(GetBlob());

        case Request_EntityUpdate:
            return sizeof(EntityUpdateRequestHeader) + Dob::Typesystem::BlobOperations::GetSize(GetBlob());

        case Request_EntityDelete:
            return sizeof(EntityDeleteRequestHeader);

        case Response:
            return sizeof(ResponseHeader) + Dob::Typesystem::BlobOperations::GetSize(GetBlob());

        case RegistrationState:
            return sizeof(RegistrationStateHeader) + GetRegistrationStateHeader().m_handlerStrSize;

        case EntityState:
            {
                const size_t headersAndTimestamps = sizeof(EntityStateHeader) + GetEntityStateHeader().m_numTimestamps * sizeof (Typesystem::Int64);
                if (HasBlob())
                {
                    return headersAndTimestamps + Dob::Typesystem::BlobOperations::GetSize(GetBlob());
                }
                else
                {
                    return headersAndTimestamps;
                }
            }

        case Action_PendingRegistrationRequest:
        case Action_PendingRegistrationResponse:
            return sizeof(PendingRegistrationMsg);

        case Action_HavePersistenceDataRequest:
            return sizeof (Header);
        case Action_HavePersistenceDataResponse:
            return sizeof (HavePersistenceDataResponseMsg);

        case Action_RequestPoolDistribution:
            return sizeof (RequestPDHeader);
        default:
            ENSURE(false, << "DistributionData::Size: Invalid message kind: " << GetType());
            return 0;
        }
    }

    DistributionData::Type
    DistributionData::GetType() const
    {
        ENSURE(m_data, << "DistributionData::GetType: NULL DistributionData! ");

        return GetHeader().m_type;
    }

    Typesystem::TypeId
    DistributionData::GetTypeId() const
    {
        switch (GetType())
        {
        case Request_EntityDelete:
            return GetEntityDeleteRequestHeader().m_typeId;

        //Read type id from blob
        case Message:
        case Request_Service:
        case Request_EntityCreate:
        case Request_EntityUpdate:
        case Response:
            return Typesystem::BlobOperations::GetTypeId(GetBlob());

        case Action_PendingRegistrationRequest:
        case Action_PendingRegistrationResponse:
            return GetPendingRegistrationMsg().m_typeId;

        case RegistrationState:
        case EntityState:
            return GetStateHeader().m_typeId;

        case Action_Connect:
        case Action_Disconnect:
            {
                ENSURE(false, << "Calling DistributionData::GetTypeId for msg Action_Connect or Action_Disconnect");
            }

        default:
            {
                ENSURE(false, << "Calling DistributionData::GetTypeId for msg unknown type " << GetType());
            }
        }
        //keep compiler happy. We never get here!
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Internal error, got to end of DistributionData::GetTypeId()", __WFILE__,__LINE__);
    }

    const Typesystem::ChannelId DistributionData::GetChannelId() const
    {
        return Typesystem::ChannelId(GetMessageHeader().m_channelId);
    }

    const Typesystem::HandlerId DistributionData::GetHandlerId() const
    {
        switch (GetType())
        {
        case Action_PendingRegistrationRequest:
        case Action_PendingRegistrationResponse:
            return Typesystem::HandlerId(GetPendingRegistrationMsg().m_handlerId);

        case Request_Service:
        case Request_EntityCreate:
        case Request_EntityUpdate:
        case Request_EntityDelete:
            return Typesystem::HandlerId(GetRequestHeader().m_handlerId);

        case RegistrationState:
        {
            if (GetRegistrationStateHeader().m_handlerStrSize > 0)
            {
                return Typesystem::HandlerId(GetStateHeader().m_handlerId,
                                             Typesystem::Utilities::ToWstring(GetData() + sizeof(RegistrationStateHeader)));
            }
            else
            {
                return Typesystem::HandlerId(GetStateHeader().m_handlerId);
            }
        }

        case EntityState:
            return Typesystem::HandlerId(GetStateHeader().m_handlerId);

        default:
            ENSURE(false, << "Calling DistributionData::GetHandlerId for type that doesn't have a handler id.");
        }
        //keep compiler happy. We never get here!
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Internal error, got to end of DistributionData::GetHandlerId()", __WFILE__,__LINE__);
    }

    void DistributionData::SetHandlerId(const Typesystem::HandlerId& handlerId)
    {
        switch (GetType())
        {
        case EntityState:
            GetStateHeader().m_handlerId = handlerId.GetRawValue();
            break;

        case Request_EntityUpdate:
        case Request_EntityDelete:
            GetRequestHeader().m_handlerId = handlerId.GetRawValue();
            break;

        default:
            ENSURE(false, << "Calling DistributionData::SetHandlerId for type doesnt allow it. type" << GetType());
        }
    }

    const Typesystem::InstanceId DistributionData::GetInstanceId() const
    {
        switch (GetType())
        {
        case Request_EntityCreate:
            {
                const EntityCreateRequestHeader & reqHeader = GetEntityCreateRequestHeader();
                if (!reqHeader.m_hasInstanceId)
                {
                    std::wostringstream ostr;
                    ostr << "This CreateRequest does not have an InstanceId!. (type = "
                        << Typesystem::Operations::GetName(GetTypeId())
                        << ", handler = " << GetHandlerId() << ")";
                    throw Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
                }
                return Typesystem::InstanceId(reqHeader.m_instanceId);
            }

        case Request_EntityUpdate:
            return Typesystem::InstanceId(GetEntityUpdateRequestHeader().m_instanceId);

        case Request_EntityDelete:
            return Typesystem::InstanceId(GetEntityDeleteRequestHeader().m_instanceId);

        case EntityState:
            return Typesystem::InstanceId(GetEntityStateHeader().m_instanceId);

        default:
            ENSURE(false, << "Calling DistributionData::GetInstanceId for type that doesn't have an instance id.");
        }
        //keep compiler happy. We never get here!
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Internal error, got to end of DistributionData::GetInstanceId()", __WFILE__,__LINE__);
    }

    bool DistributionData::HasInstanceId() const
    {
        switch (GetType())
        {
        case Request_EntityCreate:
            {
                return GetEntityCreateRequestHeader().m_hasInstanceId;
            }

        default:
            ENSURE(false, << "Calling DistributionData::HasInstanceId for type that is not Request_EntityCreate.");
        }
        //keep compiler happy. We never get here!
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Internal error, got to end of DistributionData::HasInstanceId()", __WFILE__,__LINE__);
    }

    const char * const DistributionData::GetConnectionName() const
    {
        return GetData() + sizeof(ConnectHeader);
    }

    const InternalRequestId
    DistributionData::GetRequestId() const
    {
        if (GetType() == Response)
        {
            return GetResponseHeader().m_requestId;
        }
        else
        {
            return GetRequestHeader().m_requestId;
        }
    }

    ResponseId
    DistributionData::GetResponseId() const
    {
        return GetRequestHeader().m_responseId;
    }


    const char * const
    DistributionData::GetBlob() const
    {
        switch (GetType())
        {
        case EntityState:
            {
                ENSURE(HasBlob(), << "Cannot do GetBlob on a state that doesnt have a blob!");
                return GetData() + sizeof(EntityStateHeader) + sizeof(Typesystem::Int64) * GetEntityStateHeader().m_numTimestamps;
            }
            break;

        case Request_Service:
            return GetData() + sizeof (RequestHeader);

        case Request_EntityCreate:
            return GetData() + sizeof (EntityCreateRequestHeader);
        case Request_EntityUpdate:
            return GetData() + sizeof (EntityUpdateRequestHeader);
        case Response:
            return GetData() + sizeof (ResponseHeader);
        case Message:
            return GetData() + sizeof(MessageHeader);
        default:
            ENSURE(false, << "GetBlob is not possible to call for DistributionDatas of type " << GetType());
        }
        return NULL; //keep compiler happy
    }

    Typesystem::Int64 DistributionData::GetTopTimestamp() const
    {
        ENSURE(!IsNoState(), << "Cannot do GetTopTimestamp on NoState");
        ENSURE(GetHeader().m_type == EntityState, << "Cannot do GetTopTimestamp on distributiondata of type " << GetHeader().m_type);
        ENSURE(InjectionKindTable::Instance().IsInjectable(GetTypeId()), << "Cannot do GetTopTimestamp on noninjectable type " << GetTypeId());
        return *AnyPtrCast<Typesystem::Int64>(GetData() + sizeof(EntityStateHeader));
    }
    void DistributionData::SetTopTimestamp(const Typesystem::Int64 timestamp)
    {
        ENSURE(!IsNoState(), << "Cannot do SetTopTimestamp on NoState");
        ENSURE(GetHeader().m_type == EntityState, << "Cannot do SetTopTimestamp on distributiondata of type " << GetHeader().m_type);
        ENSURE(InjectionKindTable::Instance().IsInjectable(GetTypeId()), << "Cannot do SetTopTimestamp on noninjectable type "
            << Safir::Dob::Typesystem::Operations::GetName(GetTypeId()));
        *AnyPtrCast<Typesystem::Int64>(GetData() + sizeof(EntityStateHeader)) = timestamp;
    }


    Typesystem::Int64 * DistributionData::GetMemberTimestamps()
    {
        ENSURE(!IsNoState(), << "Cannot do GetMemberTimestamps on NoState");
        ENSURE(GetHeader().m_type == EntityState, << "Cannot do GetMemberTimestamps on distributiondata of type " << GetHeader().m_type);
        const Typesystem::TypeId typeId = GetTypeId();
        ENSURE(InjectionKindTable::Instance().IsInjectable(typeId), << "Cannot do GetMemberTimestamps on noninjectable type " << GetTypeId());

        if (!HasBlob())
        {
            return NULL;
        }

        return AnyPtrCast<Typesystem::Int64>(GetData() + sizeof(EntityStateHeader) + sizeof(Typesystem::Int64));
    }

    const DistributionData
    DistributionData::GetRegistrationStateCopy() const
    {
        if (IsNoState())
        {
            return DistributionData(no_state_tag);
        }

        const size_t size = sizeof(RegistrationStateHeader) + GetRegistrationStateHeader().m_handlerStrSize;
        DistributionData result(tabula_rasa_tag, size);
        memcpy(result.GetData(), GetData(), size);

        return result;
    }

    const DistributionData
    DistributionData::GetEntityStateCopy(const bool includeBlob) const
    {
        if (IsNoState())
        {
            return DistributionData(no_state_tag);
        }

        size_t blobSize = 0;

        if (includeBlob && HasBlob())
        {
            const char * const blob = GetBlob();

            if (blob != NULL)
            {
                blobSize = Dob::Typesystem::BlobOperations::GetSize(blob);
            }
        }

        int numTimestamps = 0;
        if (InjectionKindTable::Instance().IsInjectable(GetTypeId()))
        {
            //The top-level timestamp
            numTimestamps = 1;

            //the per-member timestamps
            if (includeBlob && HasBlob())
            {
                numTimestamps += Safir::Dob::Typesystem::Members::GetNumberOfMembers(GetTypeId());
            }
        }

        const size_t size = sizeof(EntityStateHeader) + numTimestamps * sizeof(Typesystem::Int64) + blobSize;
        DistributionData result(tabula_rasa_tag, size);
        memcpy(result.GetData(), GetData(), size);

        result.GetEntityStateHeader().m_numTimestamps = numTimestamps;

        if (!includeBlob)
        {
            result.GetEntityStateHeader().m_hasBlob = false;
        }

        return result;
    }

    const DistributionData DistributionData::GetEntityStateCopy(const char * const blob) const
    {
        ENSURE(blob!=NULL, << "GetStateCopy: Blob is NULL! (when copying object " << GetTypeId() << ")");

        const Typesystem::TypeId typeId = Dob::Typesystem::BlobOperations::GetTypeId(blob);
        ENSURE (typeId == GetTypeId(), << "DistributionData::GetEntityStateCopy: TypeId mismatch!");

        const size_t blobSize = Dob::Typesystem::BlobOperations::GetSize(blob);

        int numTimestamps = 0;
        if (InjectionKindTable::Instance().IsInjectable(GetTypeId()))
        {
            //The top-level timestamp
            numTimestamps = 1 + Safir::Dob::Typesystem::Members::GetNumberOfMembers(GetTypeId());
        }

        const size_t timestampsSize = numTimestamps * sizeof(Typesystem::Int64);

        const size_t size = sizeof(EntityStateHeader) + timestampsSize + blobSize;

        DistributionData result(tabula_rasa_tag, size);

        // Copy header + any timestamps
        memcpy(result.GetData(), GetData(), sizeof(EntityStateHeader) + timestampsSize);

        result.GetEntityStateHeader().m_numTimestamps = numTimestamps;

        // Copy blob
        memcpy(result.GetData() + sizeof(EntityStateHeader) + timestampsSize, blob, blobSize);
        result.GetEntityStateHeader().m_hasBlob = true;

        return result;
    }

    void DistributionData::SetChangeFlags(const bool changed)
    {
        if (IsNoState() || !HasBlob())
        {
            return;
        }

        char* blob = const_cast<char*>(GetBlob());

        Typesystem::Internal::SetChanged(blob, changed);
    }

    void DistributionData::DecrementVersion()
    {
        ENSURE(!GetEntityStateHeader().m_versionIsDecremented, << "Can't decrement an already decremented version!");
        --GetEntityStateHeader().m_version;
        GetEntityStateHeader().m_versionIsDecremented = true;
    }

    const VersionNumber DistributionData::GetUndecrementedVersion() const
    {
        VersionNumber ver = GetEntityStateHeader().m_version;
        if (GetEntityStateHeader().m_versionIsDecremented)
        {
            ++ver;
        }
        return ver;
    }

    char * DistributionData::GetBlobCopy() const
    {
        return Dob::Typesystem::Internal::CreateCopy(GetBlob());
    }


    bool DistributionData::IsNoState() const
    {
        if (!m_data)
        {
            return true;
        }
        else
        {
            ENSURE(GetHeader().m_type == EntityState || GetHeader().m_type == RegistrationState,
                   << "IsNoState: DistributionData is not an entity or registration state (type = " << GetType() << ")");
            return false;
        }
    }


    bool DistributionData::IsCreated() const
    {
        return !IsNoState() &&
               GetEntityStateKind() == Real &&
               HasBlob();
    }

    const std::wstring DistributionData::RequestHeaderImage() const
    {
        std::wostringstream oStr;
        const RequestHeader & header=GetRequestHeader();

        oStr << "\tHandlerId: " << GetHandlerId() << std::endl
             << "\tRequestId: " << header.m_requestId << std::endl
             << "\tResponseId: " << header.m_responseId;

        return oStr.str();
    }


    const std::wstring DistributionData::HeaderImage() const
    {
        std::wostringstream oStr;
        const Header & header=GetHeader();

        oStr << "\tuse_count: " << IntrusiveOperations::GetCount(GetData()).value() << std::endl
             << "\tSender node: " << header.m_sender.m_node << std::endl
             << "\tSender id: " << header.m_sender.m_id;

        return oStr.str();
    }

    const std::wstring DistributionData::Image() const
    {
        std::wostringstream oStr;

        if (!m_data) //if NULL
        {
            oStr << "No_State" << std::endl;
            return oStr.str();
        }

        switch (GetType())
        {
        case Action_Connect:
            {
                oStr << "Action_Connect - " << GetConnectionName() << std::endl
                     << HeaderImage() << std::endl;
            }
            break;
        case Action_Disconnect:
            {
                oStr << "Action_Disconnect" << std::endl
                     << HeaderImage() << std::endl;
            }
            break;

        case RegistrationState:
            {
                oStr << "RegistrationState" << std::endl
                     << HeaderImage() << std::endl
                     << "\tType: " << Dob::Typesystem::Operations::GetName(GetTypeId()) << std::endl
                     << "\tHandlerId: " << GetHandlerId() << std::endl
                     << "\tInstanceIdPolicy: ";

                switch (GetInstanceIdPolicy())
                {
                case InstanceIdPolicy::HandlerDecidesInstanceId: oStr << "HandlerDecidesInstanceId"; break;
                case InstanceIdPolicy::RequestorDecidesInstanceId: oStr << "RequestorDecidesInstanceId"; break;
                }
                oStr << std::endl
                     << "\tRegistrationStateKind: ";
                switch (GetRegistrationStateKind())
                {
                case Registered: oStr << "Registered"; break;
                case Unregistered: oStr << "Unregistered"; break;
                case ImplicitUnregistered: oStr << "ImplicitUnregistered"; break;
                }
                oStr << std::endl
                     << "\tRegistrationTime: " << GetRegistrationTime() << std::endl;
            }
            break;

        case EntityState:
            {
                //const EntityStateHeader & header = GetEntityStateHeader();
                oStr << "EntityState" << std::endl
                     << HeaderImage() << std::endl
                     << "\tType: " << Dob::Typesystem::Operations::GetName(GetTypeId()) << std::endl
                     << "\tHandlerId: " << GetHandlerId() << std::endl
                     << "\tRegistrationTime: " << GetRegistrationTime() << std::endl
                     << "\tInstanceId: " << GetInstanceId() << std::endl
                     << "\tCreationTime: " << GetCreationTime() << std::endl
                     << "\tVersion: " << GetVersion() << std::endl
                     << "\tEntityStateKind: ";
                switch (GetEntityStateKind())
                {
                case Real: oStr << "Real"; break;
                case Ghost: oStr << "Ghost"; break;
                case Injection: oStr << "Injection"; break;
                }
                oStr << std::endl
                     << "\tExplicitlyDeleted: " << std::boolalpha << IsExplicitlyDeleted() << std::endl
                     << "\tSourceIsPermanentStore: " << SourceIsPermanentStore() << std::endl
                     << "\tHasBlob: " << HasBlob() << std::endl
                     << "\tVersionIsDecremented: " << GetEntityStateHeader().m_versionIsDecremented << std::endl
                     << "\tNumTimestamps: " << GetEntityStateHeader().m_numTimestamps <<std::endl;
                if (GetEntityStateHeader().m_numTimestamps != 0)
                {
                    oStr << "\tTimestamps: Top = " << GetTopTimestamp() << std::endl;
                    for (int i = 0; i < GetEntityStateHeader().m_numTimestamps - 1; ++i)
                    {
                        oStr << "\t            " << Safir::Dob::Typesystem::Members::GetName(GetTypeId(),i)
                            << " = \t" << GetMemberTimestamps()[i] << std::endl;
                    }
                }

                if (HasBlob())
                {
                    oStr << "\tObject = " << Safir::Dob::Typesystem::Serialization::ToXml(GetBlob()) <<std::endl;
                }
            }
            break;

        case Action_PendingRegistrationRequest:
            {
                ConnectionId originator = GetPendingOriginator();
                oStr << "Action_PendingRegistrationRequest" << std::endl
                     << HeaderImage() << std::endl
                     << "\tType: " << Dob::Typesystem::Operations::GetName(GetTypeId()) << std::endl
                     << "\tHandlerId: " << GetHandlerId() << std::endl
                     << "\tRequestTimestamp: " << GetPendingRequestTimestamp() << std::endl
                     << "\tRequestId: " << GetPendingRequestId() << std::endl
                     << "\tOriginator: (" << originator.m_id << ", " << originator.m_node << ")" << std::endl;
            }
            break;

        case Action_PendingRegistrationResponse:
            {
                ConnectionId originator = GetPendingOriginator();
                oStr << "Action_PendingRegistrationResponse" << std::endl
                     << HeaderImage() << std::endl
                     << "\tType: " << Dob::Typesystem::Operations::GetName(GetTypeId()) << std::endl
                     << "\tHandlerId: " << GetHandlerId() << std::endl
                     << "\tRequestTimestamp: " << GetPendingRequestTimestamp() << std::endl
                     << "\tRequestId: " << GetPendingRequestId() << std::endl
                     << "\tOriginator: (" << originator.m_id << ", " << originator.m_node << ")" << std::endl
                     << "\tResponse: " << std::boolalpha << GetPendingResponse() << std::endl;
            }
            break;

        case Action_HavePersistenceDataRequest:
            {
                oStr << "Action_HavePersistenceDataRequest" << std::endl
                     << HeaderImage() << std::endl;
            }
            break;

        case Action_HavePersistenceDataResponse:
            {
                oStr << "Action_HavePersistenceDataResponse" << std::endl
                     << HeaderImage() << std::endl
                     << "\tIHavePersistenceData: " << std::boolalpha << GetIHavePersistenceData() << std::endl;
            }
            break;

        case Action_RequestPoolDistribution:
            {
                oStr << "Action_RequestPoolDistribution" << std::endl
                     << HeaderImage() << std::endl
                     << "\tReceiverId: " << GetPDRequestReceiverId() << std::endl;
            }
            break;

        case Message:
            {
                oStr << "Message" << std::endl
                     << HeaderImage() << std::endl
                     << "\t" << GetChannelId() << std::endl;
            }
            break;

        case Request_Service:
            {
                oStr << "Request_Service" << std::endl
                     << HeaderImage() << std::endl
                     << RequestHeaderImage() << std::endl;
            }
            break;

        case Request_EntityCreate:
            {
                oStr << "Request_Service" << std::endl
                     << HeaderImage() << std::endl
                     << RequestHeaderImage() << std::endl
                     << "\tHasInstanceId: " << std::boolalpha << HasInstanceId() << std::endl;
                if (HasInstanceId())
                {
                    oStr << "\tInstanceId: " << GetInstanceId() << std::endl;
                }
            }
            break;

        case Request_EntityUpdate:
            {
                oStr << "Request_EntityUpdate" << std::endl
                     << HeaderImage() << std::endl
                     << RequestHeaderImage() << std::endl
                     << "\tInstanceId: " << GetInstanceId() << std::endl;
            }
            break;

        case Response:
            {
                oStr << "Response" << std::endl
                     << HeaderImage() << std::endl
                     << "\tReceiver: " << GetReceiverId() << std::endl
                     << "\tRequestId: " << GetRequestId() << std::endl;
            }
            break;

        case Request_EntityDelete:
            {
                oStr << "Request_EntityDelete" << std::endl
                     << HeaderImage() << std::endl
                     << RequestHeaderImage() << std::endl
                     << "\tInstanceId: " << GetInstanceId() << std::endl
                     << "\tType: " << Dob::Typesystem::Operations::GetName(GetTypeId()) << std::endl;
            }
            break;

        default:
            {
                oStr << "Unknown_Type!!!";
            }
            break;
        }

        return oStr.str();
    }


    const char * DistributionData::GetReference() const
    {
        intrusive_ptr_add_ref(GetData());
        return GetData();
    }

    void DistributionData::DropReference(const char* const data)
    {
        if (data != NULL)
        {
            intrusive_ptr_release(data);
        }
    }

    void DistributionData::AddReference(const char* const data)
    {
        if (data != NULL)
        {
            intrusive_ptr_add_ref(data);
        }
    }

    char * DistributionData::NewData(const size_t size)
    {
        char * mem = IntrusiveOperations::Allocate(size);

#ifdef REGISTER_TIMES
        AnyPtrCast<Header>(mem)->m_id = GenerateId();
#endif
        IntrusiveOperations::AddRef(mem);

        return mem;
    }

    DistributionData::DistributionData(new_data_tag_t, char * const data)
    {
        m_data = data;
    }

}
}
}
