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
#include "OdbcHelper.h"

OdbcHelper::OdbcHelper()
    : m_int64Size(sizeof(int64_t))
{

}

void OdbcHelper::ThrowException(SQLSMALLINT handleType,
                                SQLHANDLE handle)
{
    SQLCHAR     sqlState[6];
    SQLINTEGER  lpNativeErrorPtr;
    SQLCHAR     messageText[512];

    const SQLRETURN ret = ::SQLGetDiagRecA(handleType,
                                           handle,
                                           1,
                                           sqlState,
                                           &lpNativeErrorPtr,
                                           messageText,
                                           256,
                                           0);
    if (SQL_SUCCEEDED(ret))
    {
        const std::string string = std::string(reinterpret_cast<char*>(sqlState))
            + ":"
            + reinterpret_cast<char*>(messageText);

        throw OdbcException(string);
    }
}

void OdbcHelper::AllocStatement(SQLHSTMT* statement, SQLHDBC connection)
{
    const SQLRETURN ret = ::SQLAllocHandle(SQL_HANDLE_STMT, connection, statement);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,statement);
    }
}

void OdbcHelper::BindColumnInt64(SQLHSTMT statement,
                                 unsigned short columnNumber,
                                 int64_t* value)
{
    const SQLRETURN ret = ::SQLBindCol(statement,
                                       columnNumber,
                                       SQL_C_SBIGINT,
                                       value,
                                       sizeof(int64_t),
                                       &m_int64Size);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,statement);
    }
}

void OdbcHelper::BindColumnBinary(SQLHSTMT statement,
                                  unsigned short columnNumber,
                                  const int maxSize,
                                  unsigned char* buffer,
                                  SQLLEN* sizePtr)
{
    const SQLRETURN ret = ::SQLBindCol(statement,
                                       columnNumber,
                                       SQL_C_BINARY,
                                       buffer,
                                       maxSize,
                                       sizePtr);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,statement);
    }
}

void OdbcHelper::BindParamInt64(SQLHSTMT statement,
                                const SQLUSMALLINT paramNumber,
                                int64_t* value)
{
    const SQLRETURN ret = ::SQLBindParameter(statement,
                                             paramNumber,
                                             SQL_PARAM_INPUT,
                                             SQL_C_SBIGINT,
                                             SQL_BIGINT,
                                             20,
                                             0,
                                             value,
                                             sizeof(int64_t),
                                             &m_int64Size);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,statement);
    }
}

void OdbcHelper::BindParamString(SQLHSTMT statement,
                                 const SQLUSMALLINT paramNumber,
                                 const SQLUINTEGER maxSize,
                                 char* string,
                                 SQLLEN* sizePtr)
{
    const SQLUINTEGER number_of_chars = static_cast<SQLUINTEGER>(maxSize) + 1;
    const SQLUINTEGER size_of_char = static_cast<SQLUINTEGER>(sizeof(char));
    const SQLUINTEGER columnSize = number_of_chars* size_of_char;

    const SQLRETURN ret = ::SQLBindParameter(statement,
                                             paramNumber,
                                             SQL_PARAM_INPUT,
                                             SQL_C_CHAR,
                                             SQL_VARCHAR,
                                             columnSize,
                                             0,
                                             string,
                                             maxSize,
                                             sizePtr);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,statement);
    }
}


void OdbcHelper::Connect(SQLHDBC connection, const std::string& connectionString)
{
    const SQLRETURN ret = ::SQLDriverConnectA(connection,
                                              NULL,
                                              reinterpret_cast<SQLCHAR*>(const_cast<char*>(connectionString.c_str())),
                                              SQL_NTS,
                                              NULL,
                                              0,
                                              NULL,
                                              SQL_DRIVER_NOPROMPT);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_DBC, connection);
    }

    const SQLRETURN ret2 = ::SQLSetConnectAttr(connection,
                                               SQL_ATTR_AUTOCOMMIT,
                                               reinterpret_cast<SQLPOINTER>(SQL_AUTOCOMMIT_ON),
                                               SQL_IS_UINTEGER);
    if (!SQL_SUCCEEDED(ret2))
    {
        ThrowException(SQL_HANDLE_DBC, connection);
    }
}

void OdbcHelper::Execute(SQLHSTMT statement)
{
    const SQLRETURN ret = ::SQLExecute(statement);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,statement);
    }
}

bool OdbcHelper::Fetch(SQLHSTMT statement)
{
    const SQLRETURN ret = ::SQLFetch(statement);
    if (ret==SQL_NO_DATA_FOUND)
    {
        return false; //No data to process
    }
    else if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,statement);
    }
    return true;
}

void OdbcHelper::Prepare(SQLHSTMT statement,
                         const std::string& sql)
{
    // const_cast is used because StatementText is declared as input in the ODBC
    // specification and should be a const wchar_t*.
    const SQLRETURN ret = ::SQLPrepareA(statement,
                                        reinterpret_cast<SQLCHAR*>(const_cast<char*>(sql.c_str())),
                                        SQL_NTS);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT, statement);
    }
}
