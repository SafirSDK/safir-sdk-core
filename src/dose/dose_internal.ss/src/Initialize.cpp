/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://www.consoden.se)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <Safir/Dob/Internal/Initialize.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/ContextSharedTable.h>
#include <Safir/Dob/Internal/MessageTypes.h>
#include <Safir/Dob/Internal/EndStates.h>
#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Internal/NodeStatuses.h>
#include <Safir/Dob/Internal/EntityTypes.h>


namespace Safir
{
namespace Dob
{
namespace Internal
{

void InitializeDoseInternalFromDoseMain(const int64_t nodeId)
{
    Connections::Cleanup();
    ContextSharedTable::Initialize();
    MessageTypes::Initialize(true);
    EndStates::Initialize();
    ServiceTypes::Initialize(true,nodeId);
    InjectionKindTable::Initialize();
    NodeStatuses::Initialize();
    EntityTypes::Initialize(true,nodeId);

}

void InitializeDoseInternalFromApp()
{
    ContextSharedTable::Initialize();
    MessageTypes::Initialize(false);
    EndStates::Initialize();
    ServiceTypes::Initialize(false,0);
    InjectionKindTable::Initialize();
    NodeStatuses::Initialize();
    EntityTypes::Initialize(false,0);
}

}
}
}
