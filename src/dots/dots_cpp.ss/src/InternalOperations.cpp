/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include <Safir/Dob/Typesystem/Utilities.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    char * CreateCopy(char const * const blob)
    {
        char * copy;
        DotsC_CreateCopyOfBlob(copy, blob);
        return copy;
    }

    void Delete(char * & blob)
    {
        DotsC_DeleteBlob(blob);
        blob = NULL;
    }

    //Sets all changed flags in the blob to false
    void SetChanged(char * const blob, const bool changed)
    {
        DotsC_SetChanged(blob, changed);
    }

    void SetChangedHere(char* const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index,
                        const bool changed)
    {
        DotsC_SetChangedHere(blob,member,index,changed);
    }


    //Compare the two blobs and set the change flags in "mine" on all members that have
    //changed between "base" and "mine".
    void Diff(char const * const base,
              char * const mine)
    {
        DotsC_SetChangedSinceLastRead(base,mine);
        //TODO: rename the function in DOTS.
    }

    Int64 Generate64BitHash(const std::wstring & str)
    {
        if (str.empty())
        {
            throw SoftwareViolationException(L"Cannot generate a hash from an empty string",__WFILE__,__LINE__);
        }
        return DotsId_Generate64(Utilities::ToUtf8(str).c_str());
    }

    Int64 GenerateRandom64Bit()
    {
        return DotsId_GenerateRandom64();
    }

}
}
}
}
