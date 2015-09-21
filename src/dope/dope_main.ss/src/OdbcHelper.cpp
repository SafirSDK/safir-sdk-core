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
#include <stdexcept>

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
    SQLRETURN   ret;

    ret = ::SQLGetDiagRec(handleType,
                          handle,
                          1,
                          sqlState,
                          &lpNativeErrorPtr,
                          messageText,
                          256,
                          0);
    if (SQL_SUCCEEDED(ret))
    {
        std::string string = std::string(reinterpret_cast<char*>(sqlState))
            + ":"
            + reinterpret_cast<char*>(messageText);

        throw std::runtime_error(string.c_str());
    }
}

void OdbcHelper::AllocStatement(SQLHSTMT* statement, SQLHDBC connection)
{
    SQLRETURN ret = ::SQLAllocHandle(SQL_HANDLE_STMT, connection, statement);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,statement);
    }
}

void OdbcHelper::BindColumnInt64(SQLHSTMT statement,
                                 unsigned short columnNumber,
                                 int64_t* value)
{
    SQLRETURN ret;

    ret = ::SQLBindCol(statement,                                  // StatementHandle
                       columnNumber,                         // ColumnNumber,
                       SQL_C_SBIGINT,                          // TargetType,
                       value,                                  // TargetValuePtr,
                       sizeof(int64_t),  // BufferLength,
                       &m_int64Size);                                  // StrLen_or_Ind
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
    SQLRETURN ret;

    ret = ::SQLBindCol(statement,                      // StatementHandle
                       columnNumber,             // ColumnNumber,
                       SQL_C_BINARY,               // TargetType,
                       buffer,                     // TargetValuePtr,
                       maxSize,                    // BufferLength,
                       sizePtr);                   // StrLen_or_Ind
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,statement);
    }
}

void OdbcHelper::BindParamInt64(SQLHSTMT statement,
                                const SQLUSMALLINT paramNumber,
                                int64_t* value)
{
    SQLRETURN ret = ::SQLBindParameter(statement,                                  // StatementHandle
                                       paramNumber,                            // ParameterNumber,
                                       SQL_PARAM_INPUT,                        // InputOutputType
                                       SQL_C_SBIGINT,                          // ValueType
                                       SQL_BIGINT,                             // ParameterType
                                       20,                                     // ColumnSize
                                       0,                                      // DecimalDigits
                                       value,                                  // ParameterValuePtr
                                       sizeof(int64_t),  // BufferLength
                                       &m_int64Size);                                 // StrLen_or_Ind
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,statement);
    }
}

void OdbcHelper::BindParamString(SQLHSTMT statement,
                                 const SQLUSMALLINT paramNumber,
                                 const SQLUINTEGER maxSize,
                                 wchar_t* string,
                                 SQLLEN* sizePtr)
{
    const SQLUINTEGER number_of_chars = static_cast<SQLUINTEGER>(maxSize) + 1;
    const SQLUINTEGER size_of_char = static_cast<SQLUINTEGER>(sizeof(wchar_t));
    const SQLUINTEGER columnSize = number_of_chars* size_of_char;

    SQLRETURN ret = ::SQLBindParameter(statement,                          // StatementHandle
                                       paramNumber,                    // ParameterNumber,
                                       SQL_PARAM_INPUT,                // InputOutputType
                                       SQL_C_WCHAR,                    // ValueType
                                       SQL_WLONGVARCHAR,               // ParameterType
                                       //SQL_WVARCHAR,                   // ParameterType
                                       columnSize,                     // ColumnSize
                                       0,                              // DecimalDigits
                                       string,                         // ParameterValuePtr
                                       maxSize,                        // BufferLength
                                       sizePtr);                      // StrLen_or_Ind
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,statement);
    }
}


void OdbcHelper::Connect(SQLHDBC connection, const std::string& connectionString)
{
    SQLRETURN ret = ::SQLDriverConnect(connection,
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

    // SQLSetConnectAttr(SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_ON);
    ret = ::SQLSetConnectAttr(connection,
                              SQL_ATTR_AUTOCOMMIT,
                              reinterpret_cast<SQLPOINTER>(SQL_AUTOCOMMIT_ON),
                              SQL_IS_UINTEGER);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_DBC, connection);
    }
}

void OdbcHelper::Execute(SQLHSTMT statement)
{
    SQLRETURN ret = ::SQLExecute(statement);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,statement);
    }
}

bool OdbcHelper::Fetch(SQLHSTMT statement)
{
    SQLRETURN ret;
    bool bDataFound = true;

    ret = ::SQLFetch(statement);
    if (ret==SQL_NO_DATA_FOUND)
    {
        bDataFound = false;
    }
    else if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT,statement);
    }
    return bDataFound;
}

void OdbcHelper::Prepare(SQLHSTMT statement,
                         const std::string& sql)
{
    // const_cast is used because StatementText is declared as input in the ODBC
    // specification and should be a const wchar_t*.
    SQLRETURN ret = ::SQLPrepare(statement,
                                 reinterpret_cast<SQLCHAR*>(const_cast<char*>(sql.c_str())),
                                 SQL_NTS);
    if (!SQL_SUCCEEDED(ret))
    {
        ThrowException(SQL_HANDLE_STMT, statement);
    }
}
