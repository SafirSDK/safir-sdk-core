/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
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
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/

#pragma once

#include <Safir/Dob/Typesystem/Utilities.h>

/**
 * Convenience function for calling Safir::Dob::Typesystem::Utilities::ToUtf8 but without
 * all the long namespacing.
 *
 * Convert a std::wstring to UTF8-encoded std::string.
 *
 * @param wstr [in] - The wstring to convert.
 * @return - The string in UTF8 format.
 */
inline auto Str(const std::wstring& wstr)
{
    return Safir::Dob::Typesystem::Utilities::ToUtf8(wstr);
}

/**
 * Convenience function for calling Safir::Dob::Typesystem::Utilities::ToWstring but without
 * all the long namespacing.
 *
 * Convert a UTF8-encoded std::string to std::wstring
 *
 * @param str [in] - The string to convert.
 * @return - The wide string.
 */
inline auto Wstr(const std::string& str)
{
    return Safir::Dob::Typesystem::Utilities::ToWstring(str);
}
