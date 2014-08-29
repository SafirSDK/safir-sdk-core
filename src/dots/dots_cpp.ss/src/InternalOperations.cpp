/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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

#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Dob/Typesystem/Utilities.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    Int64 Generate64BitHash(const std::wstring & str)
    {
        if (str.empty())
        {
            throw SoftwareViolationException(L"Cannot generate a hash from an empty string",__WFILE__,__LINE__);
        }
        return LlufId_Generate64(Utilities::ToUtf8(str).c_str());
    }

    Int64 GenerateRandom64Bit()
    {
        return LlufId_GenerateRandom64();
    }


    std::wstring GetDouFilePath(const Dob::Typesystem::TypeId typeId)
    {
        const char* path=DotsC_GetDouFilePath(typeId);
        return Utilities::ToWstring(path);
    }
}
}
}
}
