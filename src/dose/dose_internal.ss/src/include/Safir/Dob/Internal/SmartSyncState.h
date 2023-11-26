/******************************************************************************
*
* Copyright Saab AB, 2023 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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

#pragma once

#include <iostream>
#include <list>
#include <algorithm>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/HandlerId.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    /**
     * @brief SmartSyncState is sent as part of a PoolDistributionRequest to other nodes.
     * This class contains information about all connections, registrations and entities that we already know about from
     * the other node. In that way the other node only have to send changes to us. SmartSync only have any effect when a
     * Normal node makes a pool distribution to a LightNode.
     * On the network the SmartSyncState is converted to the corresponding protbuf-type PoolSyncInfo.
     */
    struct SmartSyncState
    {
        struct Registration; // forward declaration
        struct Entity;

        struct Connection
        {
            int64_t connectionId;
            int32_t context;
            int32_t counter;
            std::list<SmartSyncState::Registration> registrations;
            std::string name;
        };

        struct Registration{
            int64_t typeId;
            Safir::Dob::Typesystem::HandlerId handlerId;
            uint32_t registrationTime;
            std::list<SmartSyncState::Entity> entities;
            SmartSyncState::Connection* connection;

            const SmartSyncState::Entity* GetEntity(int64_t instanceId) const
            {
                for (const auto& e : entities)
                {
                    if (e.instanceId == instanceId)
                    {
                        return &e;
                    }
                }
                return nullptr;
            }

            void DeleteEntity(int64_t instanceId)
            {
                auto it = std::find_if(std::begin(entities), std::end(entities), [instanceId](auto e)
                {
                    return e.instanceId == instanceId;
                });

                if (it != std::end(entities))
                {
                    entities.erase(it);
                }
            }
        };

        struct Entity
        {
            int64_t instanceId;
            uint32_t version;
            uint32_t creationTime;
            SmartSyncState::Registration* registration;
        };

        std::list<SmartSyncState::Connection> connections;

        const SmartSyncState::Registration* GetRegistration(int32_t context, int64_t handlerId, int64_t typeId) const
        {
            for (const auto& con : connections)
            {
                if (con.context == context)
                {
                    for (const auto& reg : con.registrations)
                    {
                        if (reg.typeId == typeId && reg.handlerId.GetRawValue() == handlerId)
                        {
                            return &reg;
                        }
                    }
                }
            }

            return nullptr;
        }

        const SmartSyncState::Entity* GetEntity(int32_t context, int64_t handlerId, int64_t typeId, int64_t instanceId) const
        {
            const SmartSyncState::Registration* reg = GetRegistration(context, handlerId, typeId);
            if (reg != nullptr)
            {
                return reg->GetEntity(instanceId);
            }
            return nullptr;
        }

        void ToString(std::wostream& os) const
        {
            for (const auto& con : connections)
            {
                os << L"connection: " << con.name.c_str() <<L" id=" << con.connectionId << L" context=" << con.context << L" counter=" << con.counter << std::endl;
                for (const auto& reg : con.registrations)
                {
                    os << L"  registration: typeId=" << Safir::Dob::Typesystem::Operations::GetName(reg.typeId) << L" handlerId=" << reg.handlerId << L" regTime=" << reg.registrationTime << std::endl;
                    for (const auto& entity : reg.entities)
                    {
                        os << L"    entity: instanceId=" << entity.instanceId << L" version=" << entity.version << L" creationTime=" << entity.creationTime << std::endl;
                    }
                }
            }
        }
    };
}
}
}
