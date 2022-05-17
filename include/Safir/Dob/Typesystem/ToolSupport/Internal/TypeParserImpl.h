/******************************************************************************
*
* Copyright Saab AB, 2022 (http://safirsdkcore.com)
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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#pragma once

#include <memory>
#include <vector>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/InternalExportDefs.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeRepository.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
namespace Internal
{
    DOTS_INTERNAL_API std::shared_ptr<const TypeRepository> ParseTypeDefinitionsImpl(const std::vector<std::string>& roots);
}
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport
