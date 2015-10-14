/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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

#ifndef __DOTS_UTILITIES_H__
#define __DOTS_UTILITIES_H__

#include <Safir/Dob/Typesystem/Defs.h>
#include <string>
#include <boost/shared_ptr.hpp>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    //Forward declaration to avoid a bunch of includes.
    class Object;

    /** A smart pointer to an Object. */
    typedef boost::shared_ptr<Object> ObjectPtr;

    /**
     * Utility functions for users of the DOB type system.
     */
namespace Utilities
{
    /**
     * Convert a std::wstring to UTF8-encoded std::string.
     *
     * @param wstr [in] - The wstring to convert.
     * @return - The string in UTF8 format.
     */
    DOTS_CPP_API const std::string ToUtf8(const std::wstring & wstr);

    /**
     * Convert a UTF8-encoded std::string to std::wstring
     *
     * @param str [in] - The string to convert.
     * @return - The wide string.
     */
    DOTS_CPP_API const std::wstring ToWstring(const std::string & str);

    /**
     * Merge the changed members (recursively) from one object into another.
     *
     * This function will recurse through the members of the "from" object and
     * take all the members that have a change flag set and copy them into the "into"
     * object.
     *
     * @param into [in] - Object to merge into.
     * @param from [in,out] - Object whose changes shall be merged into "into".
     */
    DOTS_CPP_API void MergeChanges(ObjectPtr into, const ObjectPtr from);

    /**
     * Converts binary data to Base64.
     *
     * Will convert the binarySource to Base64 format.
     *
     * @param binarySource [in] - pointer to the binary data to be converted.
     * @param sourceSize [in] - number of bytes to convert.
     * @return The a string containing the Base64 representation of the binary source.
     */
    DOTS_CPP_API const std::string BinaryToBase64(char const * const binarySource, int sourceSize);

    /**
     * Converts binary data to Base64.
     *
     * Will convert the binarySource to Base64 format.
     *
     * @param bin [in] - binary data to convert.
     * @return A string containing the Base64 representation of the binary source.
     */
    DOTS_CPP_API const std::string BinaryToBase64(const Dob::Typesystem::Binary & bin);

    /**
     * Converts data in Base64 format into binary data format.
     *
     * Will convert data from Base64 format to binary format.
     *
     * @param base64Str [in] - Base64 data to convert.
     * @param binary [out] - binary serialization that will contain the result.
     */
    DOTS_CPP_API void Base64ToBinary(const std::string& base64Str, Dob::Typesystem::Binary & binary);
}
}
}
}


#endif

