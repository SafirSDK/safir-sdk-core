/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / stawi
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

#ifndef _dose_internal_message_types_h
#define _dose_internal_message_types_h

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Internal/MessageType.h>
#include <Safir/Dob/Internal/ConsumerId.h>
#include <Safir/Dob/Internal/Atomic.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API MessageTypes:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    private:
        //This is to make sure that only Instance can call the constructor even though the constructor
        //itself has to be public (limitation of boost::interprocess)
        struct private_constructor_t {};
    public:
        static void Initialize(const bool iAmDoseMain = false);
        static bool IsInitialized();

        static MessageTypes& Instance();

        void Subscribe(const ConnectionPtr&              connection,
                       Dob::Typesystem::TypeId           typeId,
                       const Dob::Typesystem::ChannelId& channelId,
                       const bool                        includeSubclasses,
                       const ConsumerId&                 consumer);

        void Unsubscribe(const ConnectionPtr&              connection,
                         Dob::Typesystem::TypeId           typeId,
                         const Dob::Typesystem::ChannelId& channelId,
                         const bool                        includeSubclasses,
                         const ConsumerId&                 consumer);

        // Removes all existing subscriptions (all consumers) for the given connection and type.
        void UnsubscribeAll(const ConnectionPtr&           connection,
                            const Dob::Typesystem::TypeId  typeId);

        // Distribute a message to the subscribers in-queues
        void DistributeMsg(const DistributionData& msg);

        // Returns true if the given connection/consumer has any message subscription for the given type.
        bool HasSubscription(const ConnectionPtr&           connection,
                             const ConsumerId&              consumer,
                             const Dob::Typesystem::TypeId  typeId);

        //Debug and statistics
        void DumpSubscriptions() const;

        //The constructor and destructor have to be public for the boost::interprocess internals to be able to call
        //them, but we can make the constructor "fake-private" by making it require a private type as argument.
        explicit MessageTypes(private_constructor_t);

    private:

        MessageType& GetType(const Typesystem::TypeId& typeId);

        typedef PairContainers<Typesystem::TypeId, MessageTypePtr>::map MessageTypeTable;
        MessageTypeTable m_messageTypes;

        bool m_iAmDoseMain;
        static MessageTypes* m_instance;
        static AtomicUint32  m_isInitialized;
    };
}
}
}
#endif

