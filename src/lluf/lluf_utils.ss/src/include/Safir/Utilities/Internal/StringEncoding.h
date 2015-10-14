/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
*
* Created by: Anders Widén
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
#ifndef __LLUF_STRING_ENCODING_H__
#define __LLUF_STRING_ENCODING_H__

#include <string>
#include <Safir/Utilities/Internal/UtilsExportDefs.h>

namespace Safir
{
namespace Utilities
{
namespace Internal
{

    /**
     * Convert a UTF16-encoded std::wstring to UTF8-encoded std::string.
     *
     * Note: Only UTF-16 BMP characters are supported
     *
     * @param wstr [in] - The wstring in UTF16 format to convert.
     * @return - The string in UTF8 format.
     */
    LLUF_UTILS_API const std::string ToUtf8(const std::wstring& wstr);

    /**
     * Convert a UTF8-encoded std::string to a UTF16-encoded std::wstring
     *
     * Note: Only UTF-16 BMP characters are supported
     *
     * @param str [in] - The string in UTF8 format to convert.
     * @return - The string in UTF16 format.
     */
    LLUF_UTILS_API const std::wstring ToUtf16(const std::string& str);
}
}
}

#endif

