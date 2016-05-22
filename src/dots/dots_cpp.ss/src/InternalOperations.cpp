/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
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
#include <Safir/Dob/Typesystem/Object.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>

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

    std::string ToJson(const char * const blob)
    {
        int BUF_SIZE = 100000;
        std::vector<char> json8(BUF_SIZE);
        Int32 resultSize;
        DotsC_BlobToJson(&json8[0], blob, BUF_SIZE, resultSize);
        if (resultSize> BUF_SIZE)
        {
            BUF_SIZE = resultSize;
            json8.resize(BUF_SIZE);
            DotsC_BlobToJson(&json8[0], blob, BUF_SIZE, resultSize);
            if (resultSize != BUF_SIZE)
            {
                throw SoftwareViolationException(L"Error in serialization buffer sizes",__WFILE__,__LINE__);
            }
        }
        return std::string(json8.begin(), json8.begin() + resultSize - 1);
    }

    Dob::Typesystem::ObjectPtr ToObjectFromJson(const std::string & json8)
    {
        char * blob;
        std::vector<char> json8v(json8.size() +1);
        json8v.assign(json8.begin(),json8.end());
        json8v.push_back(0); //null termination
        DotsC_BytePointerDeleter deleter;
        DotsC_JsonToBlob(blob, deleter, &json8v[0]);
        if (blob == NULL)
        {
            throw IllegalValueException(L"Something is wrong with the JSON-formated object", __WFILE__,__LINE__);
        }
        ObjectPtr p = ObjectFactory::Instance().CreateObject(blob);
        deleter(blob);
        return p;
    }
}
}
}
}
