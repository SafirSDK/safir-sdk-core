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
#include <Safir/Dob/Typesystem/Internal/Detail/BasicTypeOperations.h>
#include <Safir/Dob/Typesystem/Internal/Detail/BlobToXmlSerializer.h>
#include <Safir/Dob/Typesystem/Internal/Detail/BlobToJsonSerializer.h>
#include <Safir/Dob/Typesystem/Internal/Detail/XmlToBlobSerializer.h>
#include <Safir/Dob/Typesystem/Internal/Detail/JsonToBlobSerializer.h>
#include <Safir/Dob/Typesystem/Internal/Detail/RepositoryToStringHelper.h>

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
    inline void BinaryToBase64(const char* binary, size_t size, std::ostringstream& base64)
    {
        std::string bin(binary, size); //Improvement: fix implementation to accept 'const char*' and avoid this copying
        base64<<Detail::SerializationUtils::ToBase64(bin);
    }

    /**
     * Decodes base64 data into binary data.
     *
     * @param base64Str [in] - String containing a valid base64 string.
     * @param binary [out] - Binary result of conversion.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if anything goes wrong. For example if base64Str is not well-formed.
     */
    inline void Base64ToBinary(const std::string& base64Str, std::vector<char>& binary)
    {
        std::string bin;
        Detail::SerializationUtils::FromBase64(base64Str, bin); //Improvement: fix implementation to accept vector and avoid this copying
        binary.insert(binary.begin(), bin.begin(), bin.end());
    }

    /**
     * Serializes binary representation of an object to xml.
     *
     * @param repository [in] - Type repository containing needed type information.
     * @param binary [in] - Binary data to be converted.
     * @param xml [out] - Xml result of conversion.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if binary can't be serialized to xml.
     */
    template <class RepositoryT>
    void BinaryToXml(const RepositoryT* repository, const char* blob, std::ostringstream& xml)
    {
        (Detail::BlobToXmlSerializer<RepositoryT>(repository))(blob, xml);
    }

    /**
     * Converts a xml representation of an object to binary form.
     *
     * @param repository [in] - Type repository containing needed type information.
     * @param xml [in] - Xml serialized object.
     * @param blob [out] - Resulting binary data.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if xml can't be serialized to a binary.
     */
    template <class RepositoryT>
    void XmlToBinary(const RepositoryT* repository, const char* xml, std::vector<char>& blob)
    {
        (Detail::XmlToBlobSerializer<RepositoryT>(repository))(xml, blob);
    }

    /**
     * Serializes binary representation of an object to json.
     *
     * @param repository [in] - Type repository containing needed type information.
     * @param blob [in] - Binary data to be converted.
     * @param xml [out] - Json result of conversion.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if binary can't be serialized to json.
     */
    template <class RepositoryT>
    void BinaryToJson(const RepositoryT* repository, const char* blob, std::ostringstream& json)
    {
        (Detail::BlobToJsonSerializer<RepositoryT>(repository))(blob, json);
    }

    /**
     * Converts a json representation of an object to binary form.
     *
     * @param repository [in] - Type repository containing needed type information.
     * @param json [in] - Json serialized object.
     * @param binary [out] - Resulting binary data.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if json can't be serialized to a binary.
     */
    template <class RepositoryT>
    void JsonToBinary(const RepositoryT* repository, const char* json, std::vector<char>& blob)
    {
        (Detail::JsonToBlobSerializer<RepositoryT>(repository))(json, blob);
    }

    /**
     * Writes a complete text description of a type repository and all of its content.
     * This function is primarily intended for debugging. It can be used to compare if two repositiories are identical.
     *
     * Since CreateRoutines are only used for code generation and not needed for a repository to be fully
     * functional the caller must choose if CreateRoutins are to be included in the output. The reason is that
     * it shall be possible to compare outputs from two repositories where only one of them contains CreateRoutine
     * information.
     *
     * @param repository [in] - Type repository to convert to text.
     * @param includeCreateRoutines [in] - If true CreateRoutines will also be written to the output.
     * @param os [out] - Output stream. For example a ostringstream or cout.
     */
    template <class RepositoryT>
    void RepositoryToString(const RepositoryT* repository, bool includeCreateRoutines, std::ostream &os)
    {
        (Detail::ToStringHelper<RepositoryT>(repository, includeCreateRoutines))(os);
    }
}
}
}
} //end namespace Safir::Dob::Typesystem::Internal

#endif
