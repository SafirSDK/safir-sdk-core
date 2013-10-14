/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
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
#include "Diagnostics.h"
#include "StringConversion.h"
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <vector>

namespace Safir
{
namespace Databases
{
namespace Odbc
{

std::pair<std::wstring,std::wstring> GetDiagRec(const SQLSMALLINT handleType,
                                                const SQLHANDLE handle)
{
    SQLWCHAR wszSqlState[6];
    SQLINTEGER lpNativeErrorPtr;
    std::vector<SQLWCHAR> wszMessageText(256,0);
    SQLSMALLINT textLength;   
    SQLRETURN ret;

    ret = ::SQLGetDiagRecW(handleType,
                           handle,
                           1,
                           wszSqlState,
                           &lpNativeErrorPtr,
                           &wszMessageText[0],
                           static_cast<SQLSMALLINT>(wszMessageText.size()),
                           &textLength);

    switch(ret)
    {
    case SQL_SUCCESS:
        return std::make_pair(ToWstring(wszSqlState),ToWstring(wszMessageText));
    case SQL_SUCCESS_WITH_INFO:
        wszMessageText.resize(textLength);
        ret = ::SQLGetDiagRecW(handleType,
                               handle,
                               1,
                               wszSqlState,
                               &lpNativeErrorPtr,
                               &wszMessageText[0],
                               static_cast<SQLSMALLINT>(wszMessageText.size()),
                               &textLength);
        if (ret == SQL_SUCCESS)
        {
            return std::make_pair(ToWstring(wszSqlState),ToWstring(wszMessageText));
        }
        else
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Failed to call SQLGetDiagRecW for unknown reason",
                                                                     __WFILE__, __LINE__);
        }

    case SQL_ERROR:
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Call to SQLGetDiagRecW failed with SQL_ERROR",
                                                                 __WFILE__, __LINE__);
    case SQL_INVALID_HANDLE:
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Call to SQLGetDiagRecW failed with SQL_INVALID_HANDLE",
                                                                 __WFILE__, __LINE__);
    default:
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Unexpected return code from SQLGetDiagRecW",
                                                                 __WFILE__, __LINE__);
    }
}


}
}
}

