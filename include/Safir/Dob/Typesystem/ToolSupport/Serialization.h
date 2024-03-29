/******************************************************************************
*
* Copyright Saab AB, 2004-2015 (http://safirsdkcore.com)
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
* GNU General Public License for more Internals.
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
#include <Safir/Dob/Typesystem/ToolSupport/ParseError.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/BasicTypeOperations.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/BlobToXmlSerializer.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/BlobToJsonSerializer.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/XmlToBlobSerializer.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/JsonToBlobSerializer.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/RepositoryToStringHelper.h>

//There is a bug in some oldish versions of doxygen that causes spurious warnings to be issued.
//This renaming stops doxygen from thinking that Base64ToBinary in Typesystem::Utilities and the
//one in this file are the same function. Which of course they're not...
//This thing and related cmakery can probably be removed when we drop support for Ubuntu 20.04.
#ifdef DOXYGEN_BUG_WORKAROUND
#define Base64ToBinary Base64ToBinary_
#endif

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
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
        base64<<Internal::SerializationUtils::ToBase64(bin,false);
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
        Internal::SerializationUtils::FromBase64(base64Str, bin); //Improvement: fix implementation to accept vector and avoid this copying
        binary.insert(binary.begin(), bin.begin(), bin.end());
    }

    /**
     * Serializes binary representation of an object to xml.
     *
     * @param repository [in] - Type repository containing needed type information.
     * @param blob [in] - Binary data to be converted.
     * @param xml [out] - Xml result of conversion.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if binary can't be serialized to xml.
     */
    template <class RepositoryT>
    void BinaryToXml(const RepositoryT* repository, const char* blob, std::ostringstream& xml)
    {
        (Internal::BlobToXmlSerializer<RepositoryT>(repository))(blob, xml);
    }

    /**
     * Converts a xml representation of an object to binary form.
     *
     * @param repository [in] - Type repository containing needed type information.
     * @param xml [in] - Xml serialized object.
     * @param binary [out] - Resulting binary data.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if xml can't be serialized to a binary.
     */
    template <class RepositoryT>
    void XmlToBinary(const RepositoryT* repository, const char* xml, std::vector<char>& binary)
    {
        (Internal::XmlToBlobSerializer<RepositoryT>(repository))(xml, binary);
    }

    /**
     * Serializes binary representation of an object to json.
     *
     * @param repository [in] - Type repository containing needed type information.
     * @param blob [in] - Binary data to be converted.
     * @param json [out] - Json result of conversion.
     * @throws Safir::Dob::Typesystem::Parser:ParseError if binary can't be serialized to json.
     */
    template <class RepositoryT>
    void BinaryToJson(const RepositoryT* repository, const char* blob, std::ostringstream& json)
    {
        (Internal::BlobToJsonSerializer<RepositoryT>(repository))(blob, json);
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
    void JsonToBinary(const RepositoryT* repository, const char* json, std::vector<char>& binary)
    {
        (Internal::JsonToBlobSerializer<RepositoryT>(repository))(json, binary);
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
        (Internal::ToStringHelper<RepositoryT>(repository, includeCreateRoutines)).RepositoryToString(os);
    }

    /**
     * Writes a complete text description of a type. If typeId refers to a class, CreateRoutines will be omitted.
     *
     * @param repository [in] - Type repository containing all type information.
     * @param typeId [in] - The type to convert to text.
     * @param os [out] - Output stream. For example a ostringstream or cout.
     */
    template <class RepositoryT>
    void TypeToString(const RepositoryT* repository, DotsC_TypeId typeId, std::ostream &os)
    {
        (Internal::ToStringHelper<RepositoryT>(repository, false)).TypeInfoToString(typeId, os);
    }
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport

#ifdef DOXYGEN_BUG_WORKAROUND
#undefef Base64ToBinary
#endif

#endif
