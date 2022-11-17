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

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Typesystem/Internal/DistributionScopeReader.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    /**
     * @brief Keeps a singleton of the real class DistributionScopeReader to avoid having to
     * instantiate the class multiple times.
     */
    struct DOSE_INTERNAL_API DistributionScopeReader
    {
        static const Safir::Dob::Typesystem::Internal::DistributionScopeReader& Instance();
    };
}
}
}

