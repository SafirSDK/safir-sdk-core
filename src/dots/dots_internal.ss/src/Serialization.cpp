/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / joot
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
#include <Safir/Dob/Typesystem/Internal/Serialization.h>
#include "BasicTypes.h"
#include "BlobToXmlSerializer.h"
#include "BlobToJsonSerializer.h"
#include "XmlToBlobSerializer.h"
#include "JsonToBlobSerializer.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    void BinaryToBase64(const char* binary, size_t size, std::ostringstream& base64)
    {
        //TODO: fix implementation to avoid this copying
        std::string bin(binary, size);
        base64<<BasicTypes::ToBase64(bin);
    }

    void Base64ToBinary(const std::string& base64Str, std::vector<char>& binary)
    {
        //TODO: fix implementation to avoid this copying
        std::string bin;
        BasicTypes::FromBase64(base64Str, bin);
        binary.insert(binary.begin(), bin.begin(), bin.end());
    }

    void BinaryToXml(const TypeRepository* repository, const char* blob, std::ostringstream& xml)
    {
        BlobToXmlSerializer serializer(repository);
        serializer(blob, xml);
    }

    void XmlToBinary(const TypeRepository* repository, const char* xml, std::vector<char>& blob)
    {
        XmlToBlobSerializer serializer(repository);
        serializer(xml, blob);
    }

    void BinaryToJson(const TypeRepository* repository, const char* blob, std::ostringstream& json)
    {
        BlobToJsonSerializer serializer(repository);
        serializer(blob, json);
    }

    void JsonToBinary(const TypeRepository* repository, const char* json, std::vector<char>& blob)
    {
        JsonToBlobSerializer serializer(repository);
        serializer(json, blob);
    }
}
}
}
} //end namespace Safir::Dob::Typesystem::Internal
