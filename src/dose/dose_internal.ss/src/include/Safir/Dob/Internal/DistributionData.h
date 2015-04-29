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

#ifndef __DOSE_DISTRIBUTION_DATA_H__
#define __DOSE_DISTRIBUTION_DATA_H__
void intrusive_ptr_add_ref(const char * p);
void intrusive_ptr_release(const char * p);

#include <Safir/Dob/InstanceIdPolicy.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/ConnectionId.h>
#include <Safir/Utilities/Internal/Atomic.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Typesystem/Internal/BlobOperations.h>
#include <Safir/Dob/Internal/VersionNumber.h>

//#define REGISTER_TIMES

namespace Safir
{
namespace Dob
{
namespace Internal
{
    //
    // Tag types and constants for the kinds of DistributionData objects it is possible to create.
    //
    struct connect_message_tag_t {};
    const connect_message_tag_t connect_message_tag = connect_message_tag_t();

    struct disconnect_message_tag_t {};
    const disconnect_message_tag_t disconnect_message_tag = disconnect_message_tag_t();

    struct message_tag_t {};
    const message_tag_t message_tag = message_tag_t();

    struct no_state_tag_t {};
    const no_state_tag_t no_state_tag = no_state_tag_t();

    struct registration_state_tag_t {};
    const registration_state_tag_t registration_state_tag = registration_state_tag_t();

    struct entity_state_tag_t {};
    const entity_state_tag_t entity_state_tag = entity_state_tag_t();

    struct pending_registration_request_tag_t {};
    const pending_registration_request_tag_t pending_registration_request_tag = pending_registration_request_tag_t();

    struct pending_registration_response_tag_t {};
    const pending_registration_response_tag_t pending_registration_response_tag = pending_registration_response_tag_t();

    struct have_persistence_data_request_tag_t {};
    const have_persistence_data_request_tag_t have_persistence_data_request_tag = have_persistence_data_request_tag_t();

    struct have_persistence_data_response_tag_t {};
    const have_persistence_data_response_tag_t have_persistence_data_response_tag = have_persistence_data_response_tag_t();

    struct request_pool_distribution_request_tag_t {};
    const request_pool_distribution_request_tag_t request_pool_distribution_request_tag = request_pool_distribution_request_tag_t();

    struct service_request_tag_t {};
    const service_request_tag_t service_request_tag = service_request_tag_t();

    struct entity_create_request_tag_t {};
    const entity_create_request_tag_t entity_create_request_tag = entity_create_request_tag_t();

    struct entity_update_request_tag_t {};
    const entity_update_request_tag_t entity_update_request_tag = entity_update_request_tag_t();

    struct entity_delete_request_tag_t {};
    const entity_delete_request_tag_t entity_delete_request_tag = entity_delete_request_tag_t();

    struct response_tag_t {};
    const response_tag_t response_tag = response_tag_t();

    struct tabula_rasa_tag_t{};
    const tabula_rasa_tag_t tabula_rasa_tag = tabula_rasa_tag_t();

    struct new_data_tag_t{};
    const new_data_tag_t new_data_tag = new_data_tag_t();

    class DOSE_INTERNAL_API DistributionData:
        public SharedMemoryObject
    {
    public:
        enum Type
        {
            //Order types
            Action_Connect,
            Action_Disconnect,

            RegistrationState,
            EntityState,

            Action_PendingRegistrationRequest,
            Action_PendingRegistrationResponse,

            Action_HavePersistenceDataRequest,
            Action_HavePersistenceDataResponse,

            Action_RequestPoolDistribution,

            //Request types
            Request_Service,
            Request_EntityCreate,
            Request_EntityUpdate,
            Request_EntityDelete,

            //Response
            Response,

            //Message
            Message
        };

        enum EntityStateKind
        {
            Real = 0,
            Ghost,
            Injection
        };

        enum RegistrationStateKind
        {
            // The assigned enum values are important as they indicates the priority
            // between registration states (with the same timestamp).
            ImplicitUnregistered = 0,
            Registered = 1,
            Unregistered = 2
        };


        /** @name Memory allocation/deallocation routines for external communication.*/
        /** @{ */

        //####################################################################
        // Methods for allocating and deallocating memory that is meant
        // for the low level routines concerned with external communication
        // (i.e. only dose_main_communication should use these four routines.)
        //##

        /**
         * Increase the reference counter by 1 and return a raw pointer to the contained data.
         * This reference must be destroyed with DropReference below.
         */
        const char * GetReference() const;

        /**
         * Drop the reference created with GetReference above. This will decrease the refcount and maybe destroy the memory.
         */
        static void DropReference(const char* const data);

        static void AddReference(const char* const data);

        /**
         * Create a blank new data with the reference count initialized to 1.
         * This must be released with DropReference above, or inserted into a DistributionData.
         */
        static char * NewData(const size_t size);

        /**
         * Create a DistributionData object using memory allocated using NewData above.
         */
        DistributionData(new_data_tag_t, char * const data);

        static const DistributionData ConstConstructor(new_data_tag_t, const char * const data)
        {return DistributionData(new_data_tag_t(), const_cast<char*>(data));}
        //##
        //###################################################################

        /** @} */

        //Create an Action_Connect
        DistributionData(connect_message_tag_t,
                         const ConnectionId & sender,
                         const std::string & connectionNameWithoutCounter,
                         const Typesystem::Int32 counter);

        //Create an Action_Disconnect
        DistributionData(disconnect_message_tag_t,
                         const ConnectionId & sender);

        //Create a Message
        DistributionData(message_tag_t, const ConnectionId & sender, const Typesystem::ChannelId & channel, const char * const blob);

        //Create a NoState
        DistributionData(no_state_tag_t);

        // Create a RegistrationState
        DistributionData(registration_state_tag_t,
                         const ConnectionId& sender,
                         const Typesystem::TypeId typeId,
                         const Typesystem::HandlerId& handlerId,
                         const InstanceIdPolicy::Enumeration instanceIdPolicy,
                         const RegistrationStateKind kind,
                         const LamportTimestamp& regTime);

        //Create an EntityState
        //all timestamps are initialized to 0
        DistributionData(entity_state_tag_t,
                         const ConnectionId& sender,
                         const Typesystem::TypeId typeId,
                         const Typesystem::HandlerId& handlerId,
                         const LamportTimestamp& regTime,
                         const Typesystem::InstanceId& instanceId,
                         const LamportTimestamp& creationTime,
                         const EntityStateKind kind,
                         const bool explicitlyDeleted,
                         const bool sourceIsPermanentStore,
                         const char* const blob);

        DistributionData(entity_state_tag_t,
                         const ConnectionId& sender,
                         const Typesystem::TypeId typeId,
                         const Typesystem::HandlerId& handlerId,
                         const LamportTimestamp& regTime,
                         const Typesystem::InstanceId& instanceId,
                         const LamportTimestamp& creationTime,
                         const EntityStateKind kind,
                         const bool explicitlyDeleted,
                         const bool sourceIsPermanentStore,
                         Dob::Typesystem::Internal::BlobWriteHelper& blobWriter);

        //Create an Action_PendingRegistrationRequest
        DistributionData(pending_registration_request_tag_t,
                         const ConnectionId& sender,
                         const Typesystem::TypeId typeId,
                         const Typesystem::HandlerId& handlerId,
                         const LamportTimestamp& timestamp,
                         const long requestId);

        //Create an Action_PendingRegistrationResponse
        DistributionData(pending_registration_response_tag_t,
                         const DistributionData & request,
                         const ConnectionId & sender,
                         const bool response);

        //Create an Action_HavePersistenceDataRequest
        DistributionData(have_persistence_data_request_tag_t,
                         const ConnectionId& sender);


        //Create an Action_HavePersistenceDataResponse
        DistributionData(have_persistence_data_response_tag_t,
                         const ConnectionId& sender,
                         const bool iHavePersistence);

        //Create an Action_RequestPoolDistribution
        DistributionData(request_pool_distribution_request_tag_t,
                         const ConnectionId& sender,
                         const ConnectionId& receiver);

        //Create a Request_Service
        DistributionData(service_request_tag_t,
                         const ConnectionId & sender,
                         const Typesystem::HandlerId & handler,
                         const InternalRequestId requestId,
                         const char * const blob);

        //Create a Request_EntityCreate
        DistributionData(entity_create_request_tag_t,
                         const ConnectionId & sender,
                         const Typesystem::HandlerId & handler,
                         const InternalRequestId requestId,
                         const bool hasInstance,
                         const Typesystem::InstanceId & instance,
                         const char * const blob);

        //Create a Request_EntityUpdate
        DistributionData(entity_update_request_tag_t,
                         const ConnectionId & sender,
                         const InternalRequestId requestId,
                         const Typesystem::InstanceId & instance,
                         const char * const blob);

        //Create a Request_EntityDelete
        DistributionData(entity_delete_request_tag_t,
                         const ConnectionId & sender,
                         const InternalRequestId requestId,
                         const Typesystem::EntityId& entityId);

        //Create a Response,
        DistributionData(response_tag_t,
                         const ConnectionId & sender,
                         const ConnectionId & receiver,
                         const InternalRequestId requestId,
                         const char * const blob);

        ~DistributionData();

#ifdef REGISTER_TIMES
        Typesystem::Int32 GetId() const;
#endif

        /** Get Size of data */
        size_t Size() const;

        /** Get type of data, valid for all types...*/
        Type GetType() const;

        //Change data type, may destroy the data totally if missused.
        //Do we need this?!??!?!
        //if so, shouldnt it check for accepted transitions,
        //eg Action_PendingRegistrationRequest to Action_PendingRegistrationResponse
        //        void SetType(const Type type)

        //Reads type id from data, valid for all types except:
        // - Action_Connect
        // - Action_Disconnect
        Typesystem::TypeId GetTypeId() const;

        //Get the channel id from a Message (this call is not valid for any other types than Message)
        const Typesystem::ChannelId GetChannelId() const;

        //Get the handler id from the data. This call is valid for the following types:
        // - Action_PendingRegistrationRequest
        // - Action_PendingRegistrationResponse
        // - Request_Service
        // - Request_EntityCreate
        // - Request_EntityUpdate
        // - Request_EntityDelete
        // - RegistrationState
        // - EntityState
        const Typesystem::HandlerId GetHandlerId() const;

        //Set the handler id. This call is valid for the following types:
        // - RegistrationState
        // - EntityState
        // - Request_EntityUpdate
        // - Request_EntityDelete
        void SetHandlerId(const Typesystem::HandlerId& handlerId);

        //Reads and writes registration time. This call is valid for the following types:
        // - RegistrationState
        // - EntityState
        const LamportTimestamp GetRegistrationTime() const {return GetStateHeader().m_regTime;}
        void SetRegistrationTime(const LamportTimestamp regTime) {GetStateHeader().m_regTime = regTime;}

        //Reads the instance from a data, only valid id the data type is:
        // - EntityState
        // - Request_EntityCreate,
        // - Request_EntityUpdate,
        // - Request_EntityDelete,
        const Typesystem::InstanceId GetInstanceId() const;

        //Returns true if the request contains an instance id. This call is valid for the following types:
        // - Request_EntityCreate
        bool HasInstanceId() const;

        //Reads the entity id from a data, only valid id the data type is:
        // - EntityState
        // - Request_EntityCreate,
        // - Request_EntityUpdate,
        // - Request_EntityDelete,
        const Typesystem::EntityId GetEntityId() const
        {return Typesystem::EntityId(GetTypeId(), GetInstanceId());}
        //TODO: this can be more efficiently implemented by doing the work itself, rather
        //than calling the selectors. The ENSUREs are probably doubled as it is now.

        //Reads a blob from a data, only valid id the data type is:
        // - EntityState
        // - Message
        // - Request_Service,
        // - Request_EntityCreate,
        // - Request_EntityUpdate,
        // - Response,
        const char * const GetBlob() const;

        //Reads and writes sender id from data, valid for all types
        const ConnectionId & GetSenderId() const {return GetHeader().m_sender;}
        void SetSenderId(const ConnectionId & connId) {GetHeader().m_sender = connId;}
        void ResetSenderIdConnectionPart() {GetHeader().m_sender.m_id = -1;}

        /**
         * @name Operations on Action_Connect
         */

        /**
         * Reads unique application name from msg.
         * The returned pointer points inside the msg, and is null-terminated.
         * Its length is MAX_CONNECTION_NAME_LENGTH or shorter.
         */
        const char * const GetConnectionName() const;

        Typesystem::Int32 GetCounter() const {return GetConnectHeader().m_counter;}
        /** @} */

        /**
         * @name Operations on Requests and or Responses
         */
        /** @{ */

        /**
         * Reads RequestId from data.
         */
        const InternalRequestId GetRequestId() const;

        //Read/Write ResponseId from requst types (Not valid for Responses)
        ResponseId GetResponseId() const;
        void SetResponseId(const ResponseId responseId) //only to be used by dose_main
        {GetRequestHeader().m_responseId = responseId;}


        /**
         * Reads receiver app id from msg:
         * Not valid for requests.
         */
        const ConnectionId GetReceiverId() const {return GetResponseHeader().m_receiver;}
        /** @} */

       /**
         * Reads receiver app id from PDrequest:
         * 
         */
        const ConnectionId GetPDRequestReceiverId() const {return GetRequestPDHeader().m_receiver;}
        /** @} */

        /**
         * @name Operations common for both RegistrationStates and EntityStates
         */
        /** @{ */

        /**
         * Is the state a NoState?
         */
        bool IsNoState() const;


        /** @} */

        /**
         * @name Operations on RegistrationStates
         */
        /** @{ */

        const InstanceIdPolicy::Enumeration GetInstanceIdPolicy() const {return GetRegistrationStateHeader().m_instanceIdPolicy;}
        void SetInstanceIdPolicy(const InstanceIdPolicy::Enumeration instanceIdPolicy)
        {GetRegistrationStateHeader().m_instanceIdPolicy = instanceIdPolicy;}

        RegistrationStateKind GetRegistrationStateKind() const {return GetRegistrationStateHeader().m_kind;}
        void SetRegistrationStateKind(const RegistrationStateKind& kind) {GetRegistrationStateHeader().m_kind = kind;}
        bool IsRegistered() const {return GetRegistrationStateHeader().m_kind == Registered;}

        // Makes a copy of a RegistrationState msg
        const DistributionData GetRegistrationStateCopy() const;

        /** @} */

        /**
         * @name Operations on EntityStates
         */
        /** @{ */

        //Reads and writes creation time
        const LamportTimestamp GetCreationTime() const {return GetEntityStateHeader().m_creationTime;}
        void SetCreationTime(const LamportTimestamp& creationTime) {GetEntityStateHeader().m_creationTime = creationTime;}

        const VersionNumber GetVersion() const {return GetEntityStateHeader().m_version;}
        void IncrementVersion() {++GetEntityStateHeader().m_version;}
        void ResetVersion() {GetEntityStateHeader().m_version = VersionNumber();}

        EntityStateKind GetEntityStateKind() const {return GetEntityStateHeader().m_kind;}
        void SetEntityStateKind(const EntityStateKind& kind) {GetEntityStateHeader().m_kind = kind;}

        bool IsExplicitlyDeleted() const {return GetEntityStateHeader().m_explicitlyDeleted;}
        void SetExplicitlyDeleted(const bool explicitlyDeleted) {GetEntityStateHeader().m_explicitlyDeleted = explicitlyDeleted;}

        bool SourceIsPermanentStore() const {return GetEntityStateHeader().m_sourceIsPermanentStore;}
        void ResetSourceIsPermanentStore() {GetEntityStateHeader().m_sourceIsPermanentStore = false;}

        bool HasBlob() const {return GetEntityStateHeader().m_hasBlob;}

        Typesystem::Int64 GetTopTimestamp() const;
        void SetTopTimestamp(const Typesystem::Int64 timestamp);

        //returns NULL if state does not have a blob
        Typesystem::Int64 * GetMemberTimestamps();
        const Typesystem::Int64 * GetMemberTimestamps() const
        {return const_cast<DistributionData *>(this)->GetMemberTimestamps();}

        // Makes a copy of an EntityState msg.
        // includeBlob => true : Any existing blob is included in the copy.
        // includeBlob => false : Any existing blob is NOT included in the copy.
        //Timestamps are copied
        const DistributionData GetEntityStateCopy(const bool includeBlob) const;

        // Makes a copy of the given EntityState msg. Any blob in
        // the given msg is substituted with the given blob.
        //all timestamps are initialized to 0
        const DistributionData GetEntityStateCopy(const char * const blob, bool resetChangeFlags) const;

        //Make copy with identical header and timestamp but a new blob where all change flags
        //are updated to given value.
        const DistributionData SetChangeFlags(bool changed) const;

        char* GetBlobCopy() const;

        /** @} */

        /**
         * @name Operations on Action_PendingRegistrationRequest and Action_PendingRegistrationResponse
         */
        /** @{ */

        /**
         * Reads and request time from msg
         */
        LamportTimestamp GetPendingRequestTimestamp() const {return GetPendingRegistrationMsg().m_timestamp;}

        /**
         * Reads request id from msg
         */
        long GetPendingRequestId() const {return GetPendingRegistrationMsg().m_requestId;}

        //Reads write response from msg
        bool GetPendingResponse() const {return GetPendingRegistrationMsg().m_response;}
        void SetPendingResponse(const bool resp) {GetPendingRegistrationMsg().m_response = resp;}


        /**
         * Reads originator from msg
         */
        const ConnectionId & GetPendingOriginator() const {return GetPendingRegistrationMsg().m_originator;}

        bool operator ==(const DistributionData & other) const {return m_data == other.m_data;}
        bool operator !=(const DistributionData & other) const {return m_data != other.m_data;}

        /** @} */

        /**
         * @name Operations on Action_HavePersistence*
         */
        /** @{ */

        bool GetIHavePersistenceData() const {return GetHavePersistenceDataResponseMsg().m_iHavePersistenceData;}

        /** @} */

        /**
         * @name Complex operations.
         * These are more complex operations that check combinations of other things.
         */
        /** @{ */

        bool IsCreated() const;

        /** @} */

        //--------------------------------
        // DEBUG
        //--------------------------------
        const std::wstring Image() const;  // Returns a string representation of msg


    private:
        explicit DistributionData(tabula_rasa_tag_t, const size_t size);

#pragma pack (push)
#pragma pack (4)

        //This structure starts on 4 byte alignment, which is a bit odd, but the reason
        //is that the DistributionData intrusive allocation means that the data is already
        //4 byte aligned, so 4+4 is 8 and everything is good from after m_type in this 
        //structure.
        struct Header
        {
            Type m_type;
            ConnectionId m_sender;
        };

        struct ConnectHeader
        {
            Header m_commonHeader;
            Typesystem::Int32 m_counter;
            //followed by connection name, i.e. a string, so ok to end on 4 byte alignment
        };

        struct MessageHeader
        {
            Header              m_commonHeader;
            Typesystem::Int64   m_channelId;
        };

        struct StateHeader
        {
            Header              m_commonHeader;
            Typesystem::Int64   m_typeId;
            Typesystem::Int64   m_handlerId;
            LamportTimestamp    m_regTime;
        };

        struct RegistrationStateHeader
        {
            StateHeader                     m_stateHeader;
            Typesystem::Int32               m_handlerStrSize;
            InstanceIdPolicy::Enumeration   m_instanceIdPolicy;
            RegistrationStateKind           m_kind;
            //followed by handler id string, i.e. a string, so ok to end on 4 byte alignment
        };

        /* A few thoughts on the way to store timestamps in entity states:
         * Only states with types that are Injectable have the timestamps.
         * The timestamps are stored after the header, but before the blob.
         * A state with no blob only has one timestamp.
         * A state with a blob has one top-level timestamp plus one
         * timestamp per top-level member.
         */

        struct EntityStateHeader
        {
            StateHeader                 m_stateHeader;
            Typesystem::Int64           m_instanceId;
            LamportTimestamp            m_creationTime;
            VersionNumber               m_version;
            boost::uint16_t             m_padding1;
            EntityStateKind             m_kind;
            bool                        m_explicitlyDeleted;
            bool                        m_sourceIsPermanentStore;
            bool                        m_hasBlob;
            bool                        m_padding2;
            Typesystem::Int32           m_numTimestamps;
        };

        struct PendingRegistrationMsg
        {
            Header                      m_commonHeader;
            Typesystem::TypeId          m_typeId;
            Typesystem::Int64           m_handlerId;
            LamportTimestamp            m_timestamp;
            Typesystem::Int32           m_requestId;
            bool                        m_response;
            ConnectionId                m_originator;
        };

        struct HavePersistenceDataResponseMsg
        {
            Header   m_commonHeader;
            bool     m_iHavePersistenceData;
        };

        struct RequestHeader
        {
            Header m_commonHeader;
            Typesystem::Int64 m_handlerId;
            InternalRequestId m_requestId;
            ResponseId m_responseId;
        };

        struct EntityCreateRequestHeader
        {
            RequestHeader m_requestHeader;
            Typesystem::Int64 m_instanceId;
            bool m_hasInstanceId;
        };

        struct EntityUpdateRequestHeader
        {
            RequestHeader m_requestHeader;
            Typesystem::Int64 m_instanceId;
        };

        struct EntityDeleteRequestHeader
        {
            RequestHeader m_requestHeader;
            Typesystem::Int64 m_instanceId;
            Typesystem::TypeId m_typeId;
        };

        struct ResponseHeader
        {
            Header m_commonHeader;
            ConnectionId m_receiver;
            InternalRequestId m_requestId;
            Typesystem::Int32 m_padding;
        };

        struct RequestPDHeader
        {
            Header m_commonHeader;
            ConnectionId m_receiver;
        };

#pragma pack (pop)

        //This method is meant to contain BOOST_STATIC_ASSERTs, and is not meant to be called
        //BOOST_STATIC_ASSERTs are evaluated at compile-time, so this is ok!
        static void CheckSizes();

        Header & GetHeader();
        const Header & GetHeader() const  //just call non-const version
        {return const_cast<DistributionData *>(this)->GetHeader();}
        
        RequestPDHeader& GetRequestPDHeader();
        const RequestPDHeader & GetRequestPDHeader() const  //just call non-const version
        {return const_cast<DistributionData *>(this)->GetRequestPDHeader();}

        ConnectHeader& GetConnectHeader();
        const ConnectHeader & GetConnectHeader() const  //just call non-const version
        {return const_cast<DistributionData *>(this)->GetConnectHeader();}

        MessageHeader& GetMessageHeader();
        const MessageHeader & GetMessageHeader() const  //just call non-const version
        {return const_cast<DistributionData *>(this)->GetMessageHeader();}

        StateHeader& GetStateHeader();
        const StateHeader& GetStateHeader() const //just call non-const version
        {return const_cast<DistributionData *>(this)->GetStateHeader();}

        RegistrationStateHeader& GetRegistrationStateHeader();
        const RegistrationStateHeader& GetRegistrationStateHeader() const //just call non-const version
        {return const_cast<DistributionData *>(this)->GetRegistrationStateHeader();}

        EntityStateHeader & GetEntityStateHeader();
        const EntityStateHeader & GetEntityStateHeader() const //just call non-const version
        {return const_cast<DistributionData *>(this)->GetEntityStateHeader();}

        PendingRegistrationMsg & GetPendingRegistrationMsg();
        const PendingRegistrationMsg & GetPendingRegistrationMsg() const //just call non-const version
        {return const_cast<DistributionData *>(this)->GetPendingRegistrationMsg();}

        HavePersistenceDataResponseMsg & GetHavePersistenceDataResponseMsg();
        const HavePersistenceDataResponseMsg & GetHavePersistenceDataResponseMsg() const //just call non-const version
        {return const_cast<DistributionData *>(this)->GetHavePersistenceDataResponseMsg();}

        RequestHeader & GetRequestHeader();
        const RequestHeader & GetRequestHeader() const //just call non-const version
        {return const_cast<DistributionData *>(this)->GetRequestHeader();}

        EntityCreateRequestHeader & GetEntityCreateRequestHeader();
        const EntityCreateRequestHeader & GetEntityCreateRequestHeader() const //just call non-const version
        {return const_cast<DistributionData *>(this)->GetEntityCreateRequestHeader();}

        EntityUpdateRequestHeader & GetEntityUpdateRequestHeader();
        const EntityUpdateRequestHeader & GetEntityUpdateRequestHeader() const //just call non-const version
        {return const_cast<DistributionData *>(this)->GetEntityUpdateRequestHeader();}

        EntityDeleteRequestHeader & GetEntityDeleteRequestHeader();
        const EntityDeleteRequestHeader & GetEntityDeleteRequestHeader() const //just call non-const version
        {return const_cast<DistributionData *>(this)->GetEntityDeleteRequestHeader();}

        ResponseHeader & GetResponseHeader();
        const ResponseHeader & GetResponseHeader() const //just call non-const version
        {return const_cast<DistributionData *>(this)->GetResponseHeader();}

        //allocate new (unitialized) memory into m_data
        void Allocate(const size_t size);

        const std::wstring HeaderImage() const;
        const std::wstring RequestHeaderImage() const;

        /**
         * Operations for allocating and manipulating the use_count on the char
         * arrays that are contained by DistributionData.
         * The use_count is an atomic 32 bit integer, so be aware that the alignment
         * of the data is 4.
         */
        class IntrusiveOperations
        {
        public:
            static inline char * Allocate(const size_t size)
            {
                char * data = static_cast<char*>(GetSharedMemory().allocate(size + 
                                                                            sizeof(Safir::Utilities::Internal::AtomicUint32))) +
                    sizeof(Safir::Utilities::Internal::AtomicUint32);
                GetCount(data) = 0;
                return data;
            }

            static inline Safir::Utilities::Internal::AtomicUint32& GetCount(const char * p)
            {
                return *static_cast<Safir::Utilities::Internal::AtomicUint32*>
                    (static_cast<void *>
                     (const_cast<char*>(p - sizeof(Safir::Utilities::Internal::AtomicUint32))));
            }

            static inline void AddRef(const char * p)
            {
                GetCount(p)++;
            }
            static inline void ReleaseRef(const char * p)
            {
                //if the old value is 0 the new value is 0 and we can deallocate.
                if(1 == GetCount(p)--)
                {
                    GetSharedMemory().deallocate(const_cast<char*>(p) - sizeof(Safir::Utilities::Internal::AtomicUint32));
                }
            }


        private:
            IntrusiveOperations(); //defined but not declared so that it is not possible to instantiate
        };

        friend void ::intrusive_ptr_add_ref(const char * p);
        friend void ::intrusive_ptr_release(const char * p);

        char * GetData() {return m_data.get().get();}
        const char * GetData() const {return const_cast<DistributionData*>(this)->GetData();}

        SmartPointers<char>::intrusive_ptr m_data;
    };

}
}
}
//the ptrs are const so that we can hold references to const objects but still count up and down
//we cast away the constness inside the functions.

inline void intrusive_ptr_add_ref(const char * p)
{
    Safir::Dob::Internal::DistributionData::IntrusiveOperations::AddRef(p);
}

inline void intrusive_ptr_release(const char * p)
{
    Safir::Dob::Internal::DistributionData::IntrusiveOperations::ReleaseRef(p);
}



#endif

