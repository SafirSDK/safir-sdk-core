/******************************************************************************
*
* Copyright Saab AB, 2022 (http://safirsdkcore.com)
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

#include <Safir/Dob/Typesystem/Properties.h>
#include <Safir/Dob/Typesystem/Parameters.h>
#include <Safir/Dob/DistributionScopeOverrideProperty.h>
#include <Safir/Dob/DistributionScopeProperty.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    class DistributionScopeReader
    {
    public:
        /**
         * @brief GetDistributionScope - Get the DistributionScope for a type.
         * @param typeId - The type.
         * @return DistributionScope for the type.
         */
        Safir::Dob::DistributionScope::Enumeration GetDistributionScope(Safir::Dob::Typesystem::TypeId typeId) const
        {
            if (IsLocal(typeId))
            {
                return Safir::Dob::DistributionScope::Local;
            }
            if (IsLimited(typeId))
            {
                return Safir::Dob::DistributionScope::Limited;
            }
            return Safir::Dob::DistributionScope::Global;
        }

        /**
         * @brief IsLocal - Specifically check if a type is local.
         * @param typeId - The type to check.
         * @return True if type is local.
         */
        bool IsLocal(Safir::Dob::Typesystem::TypeId typeId) const
        {
            return std::binary_search(m_local.cbegin(), m_local.cend(), typeId);
        }

        /**
         * @brief IsLimited - Specifically check if a type is limited.
         * @param typeId - The type to check.
         * @return True if type is limited.
         */
        bool IsLimited(Safir::Dob::Typesystem::TypeId typeId) const
        {
            return std::binary_search(m_limited.cbegin(), m_limited.cend(), typeId);
        }

        /**
         * @brief IsGlobal - Specifically check if a type is global.
         * @param typeId - The type to check.
         * @return True if type is global.
         */
        bool IsGlobal(Safir::Dob::Typesystem::TypeId typeId) const
        {
            return !IsLocal(typeId) && !IsLimited(typeId);
        }

        /**
         * @brief DistributionScopeReader - constructor
         */
        DistributionScopeReader()
        {
            // Read DistributionScope for all types, store local and limited in separate sorted vectors. Globals are not stored since that is the default behaviour.
            bool isInherited, hasProperty;
            DotsC_TypeId paramTypeId;
            DotsC_ParameterIndex paramId;
            DotsC_Int32 paramIndex;

            for (auto typeId : Safir::Dob::Typesystem::Operations::GetAllTypeIds())
            {

                Safir::Dob::Typesystem::Operations::HasProperty(typeId,
                                                         Safir::Dob::DistributionScopeOverrideProperty::ClassTypeId,
                                                         hasProperty,
                                                         isInherited);

                if (hasProperty && !isInherited)
                {
                    Safir::Dob::Typesystem::Properties::GetParameterReference(typeId, Safir::Dob::DistributionScopeOverrideProperty::ClassTypeId, 0, 0, paramTypeId, paramId, paramIndex);
                    auto distributionScope = static_cast<Safir::Dob::DistributionScope::Enumeration>(Safir::Dob::Typesystem::Parameters::GetEnumeration(paramTypeId, paramId, paramIndex));

                    switch (distributionScope)
                    {
                    case Safir::Dob::DistributionScope::Local:
                        m_local.push_back(typeId);
                        break;

                    case Safir::Dob::DistributionScope::Limited:
                        m_limited.push_back(typeId);
                        break;

                    default:
                        break;
                    }
                }
                else if (Safir::Dob::Typesystem::Operations::HasProperty(typeId, Safir::Dob::DistributionScopeProperty::ClassTypeId))
                {
                    Safir::Dob::Typesystem::Properties::GetParameterReference(typeId, Safir::Dob::DistributionScopeProperty::ClassTypeId, 0, 0, paramTypeId, paramId, paramIndex);
                    auto distributionScope = static_cast<Safir::Dob::DistributionScope::Enumeration>(Safir::Dob::Typesystem::Parameters::GetEnumeration(paramTypeId, paramId, paramIndex));

                    switch (distributionScope)
                    {
                    case Safir::Dob::DistributionScope::Local:
                        m_local.push_back(typeId);
                        break;

                    case Safir::Dob::DistributionScope::Limited:
                        m_limited.push_back(typeId);
                        break;

                    default:
                        break;
                    }
                }
            }

            std::sort(m_local.begin(), m_local.end());
            m_local.shrink_to_fit();

            std::sort(m_limited.begin(), m_limited.end());
            m_limited.shrink_to_fit();
        }

    private:
        Safir::Dob::Typesystem::TypeIdVector m_local;
        Safir::Dob::Typesystem::TypeIdVector m_limited;
    };

}
}
}
}
