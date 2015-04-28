/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#include <Safir/Dob/Internal/MessageTypes.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Message.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    MessageTypes* MessageTypes::m_instance = NULL;

    MessageTypes& MessageTypes::Instance()
    {
        ENSURE(m_instance != NULL, << "MessageTypes::Instance was called before Initialize!!!");
        return *m_instance;
    }

    MessageTypes::MessageTypes(private_constructor_t):
        m_iAmDoseMain(false)
    {

    }

    void MessageTypes::Initialize(const bool iAmDoseMain)
    {
        m_instance = GetSharedMemory().find_or_construct<MessageTypes>("MESSAGETYPES")(private_constructor_t());

        if (iAmDoseMain)
        {
            m_instance->m_iAmDoseMain = iAmDoseMain;
            ENSURE (m_instance->m_messageTypes.empty(),
                    << "Can't start dose_main. An application or another dose_main "
                    "instance is already started!");

            Dob::Typesystem::TypeIdVector tid = Dob::Typesystem::Operations::GetAllTypeIds();

            for (Dob::Typesystem::TypeIdVector::iterator it = tid.begin();
                 it != tid.end(); ++it)
            {
                if (Dob::Typesystem::Operations::IsClass(*it))
                {
                    if (Dob::Typesystem::Operations::IsOfType(*it, Safir::Dob::Message::ClassTypeId))
                    {
                        MessageTypePtr messageType = GetSharedMemory().construct<MessageType>(boost::interprocess::anonymous_instance)(*it);
                        m_instance->m_messageTypes.insert(std::make_pair(*it, messageType));
                    }
                }
            }
        }
    }

    void MessageTypes::Subscribe(const ConnectionPtr&              connection,
                                 Dob::Typesystem::TypeId           typeId,
                                 const Dob::Typesystem::ChannelId& channelId,
                                 const bool                        includeSubclasses,
                                 const ConsumerId&                 consumer)
    {
        Dob::Typesystem::TypeIdVector classTree;

        if (includeSubclasses)
        {
            classTree = Dob::Typesystem::Operations::GetClassTree(typeId);
        }
        else
        {
            classTree.push_back(typeId);
        }

        for (Dob::Typesystem::TypeIdVector::iterator it = classTree.begin();
             it != classTree.end();
             ++it)
        {
            GetType(*it).Subscribe(connection, channelId, consumer);
        }
    }

    void MessageTypes::Unsubscribe(const ConnectionPtr&              connection,
                                   Dob::Typesystem::TypeId           typeId,
                                   const Dob::Typesystem::ChannelId& channelId,
                                   const bool                        includeSubclasses,
                                   const ConsumerId&                 consumer)
    {
        Dob::Typesystem::TypeIdVector classTree;

        if (includeSubclasses)
        {
            classTree = Dob::Typesystem::Operations::GetClassTree(typeId);
        }
        else
        {
            classTree.push_back(typeId);
        }

        for (Dob::Typesystem::TypeIdVector::iterator it = classTree.begin();
             it != classTree.end();
             ++it)
        {
            GetType(*it).Unsubscribe(connection, channelId, consumer);
        }
    }


    void MessageTypes::UnsubscribeAll(const ConnectionPtr&           connection,
                                      const Dob::Typesystem::TypeId  typeId)
    {
            GetType(typeId).UnsubscribeAll(connection);
    }

    void MessageTypes::DistributeMsg(const DistributionData& msg)
    {
        GetType(msg.GetTypeId()).DistributeMsg(msg);
    }

    bool MessageTypes::HasSubscription(const ConnectionPtr&             connection,
                                       const ConsumerId&                consumer,
                                       const Dob::Typesystem::TypeId    typeId)
    {
        return GetType(typeId).HasSubscription(connection, consumer);
    }

    MessageType& MessageTypes::GetType(const Typesystem::TypeId& typeId)
    {
        MessageTypeTable::iterator findIt = m_messageTypes.find(typeId);
        ENSURE(findIt != m_messageTypes.end(), << "GetType failed to find the message type that was asked for!!! typeId = " <<
                                                  Dob::Typesystem::Operations::GetName(typeId));

        return *findIt->second;
    }





}
}
}

