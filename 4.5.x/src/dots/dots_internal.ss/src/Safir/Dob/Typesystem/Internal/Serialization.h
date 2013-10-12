/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
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
#ifndef __DOTS_INTERNAL_SERIALIZATION_H__
#define __DOTS_INTERNAL_SERIALIZATION_H__

#include <string>
#include <vector>
#include <sstream>
#include <Safir/Dob/Typesystem/Internal/ParseError.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    class TypeRepository;

    /**
     * Encodes binary data to base64.
     *
     * @param binary [in] - Pointer to binary data to be encoded.
     * @param size [in] - Size of binary data in bytes.
     * @param base64 [out] - Base64 encoded result.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if anything goes wrong.
     */
    DOTS_API void BinaryToBase64(const char* binary, size_t size, std::ostringstream& base64);

    /**
     * Decodes base64 data into binary data.
     *
     * @param base64Str [in] - String containing a valid base64 string.
     * @param binary [out] - Binary result of conversion.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if anything goes wrong. For example if base64Str is not well-formed.
     */
    DOTS_API void Base64ToBinary(const std::string& base64Str, std::vector<char>& binary);

    /**
     * Serializes binary representation of an object to xml.
     *
     * @param repository [in] - Type repository containing needed type information.
     * @param binary [in] - Binary data to be converted.
     * @param xml [out] - Xml result of conversion.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if binary can't be serialized to xml.
     */
    DOTS_API void BinaryToXml(const TypeRepository* repository, const char* binary, std::ostringstream& xml);

    /**
     * Converts a xml representation of an object to binary form.
     *
     * @param repository [in] - Type repository containing needed type information.
     * @param xml [in] - Xml serialized object.
     * @param blob [out] - Resulting binary data.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if xml can't be serialized to a binary.
     */
    DOTS_API void XmlToBinary(const TypeRepository* repository, const char* xml, std::vector<char>& blob);

    /**
     * Serializes binary representation of an object to json.
     *
     * @param repository [in] - Type repository containing needed type information.
     * @param blob [in] - Binary data to be converted.
     * @param xml [out] - Json result of conversion.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if binary can't be serialized to json.
     */
    DOTS_API void BinaryToJson(const TypeRepository* repository, const char* binary, std::ostringstream& json);

    /**
     * Converts a json representation of an object to binary form.
     *
     * @param repository [in] - Type repository containing needed type information.
     * @param json [in] - Json serialized object.
     * @param binary [out] - Resulting binary data.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if json can't be serialized to a binary.
     */
    DOTS_API void JsonToBinary(const TypeRepository* repository, const char* json, std::vector<char>& binary);

}
}
}
} //end namespace Safir::Dob::Typesystem::Internal

#endif
