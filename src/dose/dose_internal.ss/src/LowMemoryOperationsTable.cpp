/******************************************************************************
*
* Copyright Saab AB, 2023 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#include <Safir/Dob/Internal/LowMemoryOperationsTable.h>
#include <Safir/Dob/LowMemoryOperationsAllowedProperty.h>
#include <Safir/Dob/LowMemoryOperationsAllowedOverrideProperty.h>
#include <Safir/Dob/NodeInfo.h>
#include <Safir/Dob/MirroredNodeInfo.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/Properties.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Parameters.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    LowMemoryOperationsTable* LowMemoryOperationsTable::m_instance = NULL;

    LowMemoryOperationsTable& LowMemoryOperationsTable::Instance()
    {
        ENSURE(m_instance != NULL, << "LowMemoryOperationsTable::Instance was called before Initialize!!!");
        return *m_instance;
    }

    void LowMemoryOperationsTable::Initialize()
    {
        m_instance = GetSharedMemory().find_or_construct<LowMemoryOperationsTable>("LOW_MEMORY_OPERATIONS_TABLE")(private_constructor_t());
    }

    static inline void CheckValue(const Safir::Dob::MemoryLevel::Enumeration memoryLevel,
                                  const Typesystem::TypeId typeId,
                                  const std::string& propertyName)
    {
        if (typeId == Safir::Dob::NodeInfo::ClassTypeId || typeId == Safir::Dob::MirroredNodeInfo::ClassTypeId)
        {
            return;
        }

        ENSURE(memoryLevel != Safir::Dob::MemoryLevel::Full,
               << "Full is not allowed in property mapping "
               << Typesystem::Operations::GetName(typeId) << "-Safir.Dob." << propertyName.c_str());

        ENSURE(memoryLevel != Safir::Dob::MemoryLevel::Normal,
               << "Normal has no effect in property mapping "
               << Typesystem::Operations::GetName(typeId) << "-Safir.Dob." << propertyName.c_str());

        ENSURE(memoryLevel != Safir::Dob::MemoryLevel::Warning,
               << "Warning has no effect in property mapping "
               << Typesystem::Operations::GetName(typeId) << "-Safir.Dob." << propertyName.c_str());

        ENSURE(memoryLevel != Safir::Dob::MemoryLevel::Low,
               << "Low has no effect in property mapping "
               << Typesystem::Operations::GetName(typeId) << "-Safir.Dob." << propertyName.c_str());
    }

    LowMemoryOperationsTable::LowMemoryOperationsTable(private_constructor_t)
    {
        for (auto typeId : Safir::Dob::Typesystem::Operations::GetAllTypeIds())
        {
            if (Dob::Typesystem::Operations::IsOfType(typeId,Safir::Dob::Entity::ClassTypeId))
            {
                bool isInherited, hasProperty;
                DotsC_TypeId paramTypeId;
                DotsC_ParameterIndex paramId;
                DotsC_Int32 paramIndex;

                Safir::Dob::Typesystem::Operations::HasProperty
                    (typeId,
                     Safir::Dob::LowMemoryOperationsAllowedOverrideProperty::ClassTypeId,
                     hasProperty,
                     isInherited);

                if (hasProperty && !isInherited)
                {
                    Safir::Dob::Typesystem::Properties::GetParameterReference(typeId, Safir::Dob::LowMemoryOperationsAllowedOverrideProperty::ClassTypeId, 0, 0, paramTypeId, paramId, paramIndex);
                    auto memoryLevel = static_cast<Safir::Dob::MemoryLevel::Enumeration>(Safir::Dob::Typesystem::Parameters::GetEnumeration(paramTypeId, paramId, paramIndex));

                    CheckValue(memoryLevel, typeId, "LowMemoryOperationsAllowedOverrideProperty");
                    m_memoryLevels.insert(std::make_pair(typeId, memoryLevel));
                }
                else if (Safir::Dob::Typesystem::Operations::HasProperty(typeId, Safir::Dob::LowMemoryOperationsAllowedProperty::ClassTypeId))
                {
                    Safir::Dob::Typesystem::Properties::GetParameterReference(typeId, Safir::Dob::LowMemoryOperationsAllowedProperty::ClassTypeId, 0, 0, paramTypeId, paramId, paramIndex);
                    auto memoryLevel = static_cast<Safir::Dob::MemoryLevel::Enumeration>(Safir::Dob::Typesystem::Parameters::GetEnumeration(paramTypeId, paramId, paramIndex));

                    CheckValue(memoryLevel, typeId, "LowMemoryOperationsAllowedProperty");

                    m_memoryLevels.insert(std::make_pair(typeId, memoryLevel));
                }
            }
        }
    }


    Safir::Dob::MemoryLevel::Enumeration
    LowMemoryOperationsTable::GetDisallowedLevel(const Typesystem::TypeId typeId,
                                                 const Safir::Dob::MemoryLevel::Enumeration defaultLevel) const
    {
        const auto findIt = m_memoryLevels.find(typeId);

        if (findIt == m_memoryLevels.end())
        {
            return defaultLevel;
        }

        return std::max(defaultLevel, findIt->second);
    }
}
}
}
