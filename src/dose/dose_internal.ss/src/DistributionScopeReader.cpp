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
#include <Safir/Dob/Internal/DistributionScopeReader.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    std::once_flag DistributionScopeReader::SingletonHelper::m_onceFlag;

    const Safir::Dob::Typesystem::Internal::DistributionScopeReader& DistributionScopeReader::SingletonHelper::Instance()
    {
        static Safir::Dob::Typesystem::Internal::DistributionScopeReader instance;
        return instance;
    }

    const Safir::Dob::Typesystem::Internal::DistributionScopeReader& DistributionScopeReader::Instance()
    {
        std::call_once(SingletonHelper::m_onceFlag,[]{SingletonHelper::Instance();});
        return SingletonHelper::Instance();
    }
}
}
}
