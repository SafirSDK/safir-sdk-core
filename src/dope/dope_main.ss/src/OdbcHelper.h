/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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

#ifdef _MSC_VER
#  include <windows.h>
#endif

#include <boost/noncopyable.hpp>

#include <sqltypes.h>
#include <sql.h>
#include <sqlext.h>

#include <cstdint>
#include <string>

class OdbcHelper:
    private boost::noncopyable
{
public:
    OdbcHelper();

    static void AllocStatement(SQLHSTMT* statement,
                               SQLHDBC connection);

    void BindColumnInt64(SQLHSTMT statement,
                         unsigned short columnNumber,
                         int64_t* value);

    static void BindColumnBinary(SQLHSTMT statement,
                                 unsigned short columnNumber,
                                 const int maxSize,
                                 unsigned char* buffer,
                                 SQLLEN* sizePtr);

    void BindParamInt64(SQLHSTMT statement,
                        const SQLUSMALLINT paramNumber,
                        int64_t* value);

    static void BindParamString(SQLHSTMT statement,
                                const SQLUSMALLINT paramNumber,
                                const SQLUINTEGER maxSize,
                                wchar_t* string,
                                SQLLEN* sizePtr);

    static void Connect(SQLHDBC connection,
                        const std::string& connectionString);

    static void Execute(SQLHSTMT statement);

    static bool Fetch(SQLHSTMT statement);

    static void Prepare(SQLHSTMT statement,
                        const std::string& sql);

    static void ThrowException(SQLSMALLINT handleType,
                               SQLHANDLE handle);

private:
    SQLLEN m_int64Size;;
};
