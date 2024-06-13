/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: J�rgen Johansson / stjrjo
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
#include "Safir/Databases/Odbc/Environment.h"

#include <Safir/Databases/Odbc/ReconnectException.h>
#include <Safir/SwReports/SwReport.h>

#ifdef _MSC_VER
#pragma warning (disable: 4312)
#endif

namespace Safir
{

namespace Databases
{

namespace Odbc
{

Environment::Environment(void) : m_hEnv(SQL_NULL_HENV), bSetOdbcVersion( false )
{
}

Environment::~Environment(void)
{
}

void Environment::Alloc()
{
    SQLRETURN ret = SQL_SUCCESS;

    if (IsValid() && bSetOdbcVersion)
        return; // Environment already allocated

    if (!IsValid())
    {
        ret = ::SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &m_hEnv);
        if (!SQL_SUCCEEDED(ret)) 
        {
            wchar_t wszSqlState[6];
            SQLINTEGER lpNativeErrorPtr;
            wchar_t wszMessageText[512];
            SQLRETURN ret;

            ret = ::SQLGetDiagRecW( SQL_HANDLE_ENV,
                                    SQL_NULL_HANDLE,
                                    1,
                                    wszSqlState,
                                    &lpNativeErrorPtr,
                                    wszMessageText,
                                    512,
                                    0 );
            if (wcscmp(wszSqlState, L"HY000") == 0)     // General error
            {
                ThrowReconnectException(SQL_NULL_HANDLE,__WFILE__,__LINE__);
            }
            else if ((wcscmp(wszSqlState, L"HY013") == 0) ||    
                     (wcscmp(wszSqlState, L"HY001") == 0))
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Memory management error",__WFILE__,__LINE__);
            }
            else if (wcscmp(wszSqlState, L"HY009") == 0)    
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid use of null pointer",__WFILE__,__LINE__);
            }
            else if (wcscmp(wszSqlState, L"HY010") == 0)    
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Function sequence error",__WFILE__,__LINE__);
            }
            else if (wcscmp(wszSqlState, L"HY014") == 0)    // Limit on the number of handles exceeded
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Limit on the number of handles exceeded",__WFILE__,__LINE__);
            }
            else if (wcscmp(wszSqlState, L"HY092") == 0)    
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid attribute/option identifier",__WFILE__,__LINE__);
            }
            else if ((wcscmp(wszSqlState, L"HYC00") == 0) ||    // Optional feature not implemented
                     (wcscmp(wszSqlState, L"IM001") == 0))      // Driver does not support this function
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Feature not implemented",__WFILE__,__LINE__);
            }
            else if (wcscmp(wszSqlState, L"HYT01") == 0)    // Connection timeout expired
            {
                ThrowReconnectException(SQL_NULL_HANDLE,__WFILE__,__LINE__);
            }
        }
        if (ret == SQL_SUCCESS_WITH_INFO) // Can only be 01000 - non odbc error.
        {
            wchar_t wszSqlState[6];
            SQLINTEGER lpNativeErrorPtr;
            wchar_t wszMessageText[256];
            SQLRETURN ret;

            ret = ::SQLGetDiagRecW( SQL_HANDLE_DBC,
                                    m_hEnv,
                                    1,
                                    wszSqlState,
                                    &lpNativeErrorPtr,
                                    wszMessageText,
                                    256,
                                    0 );
            Safir::SwReports::SendErrorReport(  L"Non Odbc Error",
                                                L"Safir::Databases::Odbc::Environment::Alloc()",
                                                wszMessageText );
        }
    }

    if (!bSetOdbcVersion)
    {
        SetEnvAttr(SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3);    //SQL_OV_ODBC2
        //SetEnvAttr(SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC2);    //SQL_OV_ODBC3
        bSetOdbcVersion = true;
    }
}

void Environment::Free()
{
    SQLRETURN ret;

    if (!IsValid())
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Freeing an invalid environment",__WFILE__,__LINE__);

    ret = ::SQLFreeHandle(SQL_HANDLE_ENV, m_hEnv);
    if (!SQL_SUCCEEDED(ret)) 
    {
        wchar_t wszSqlState[6];
        SQLINTEGER lpNativeErrorPtr;
        wchar_t wszMessageText[512];
        SQLRETURN ret;

        ret = ::SQLGetDiagRecW( SQL_HANDLE_ENV,
                                m_hEnv,
                                1,
                                wszSqlState,
                                &lpNativeErrorPtr,
                                wszMessageText,
                                512,
                                0 );
        if (wcscmp(wszSqlState, L"HY010") == 0) // Function sequence error
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Cannot free environment due to existing connections",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HY017") == 0)    // Stmt already freed.
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Freeing an invalid environment",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"IM001") == 0)    // Driver not implemented this function
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver not implemented this function",__WFILE__,__LINE__);
        }
        else if ((wcscmp(wszSqlState, L"HY013") == 0) ||    
                 (wcscmp(wszSqlState, L"HY001") == 0))
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Memory management error",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HY000") == 0)    // General error
        {
            Safir::SwReports::SendProgramInfoReport(wszMessageText);
        }
        else if (wcscmp(wszSqlState, L"HYT01") == 0)    // Connection timeout expired.
        {
            Safir::SwReports::SendProgramInfoReport(wszMessageText);
        }
    }
    // Cant return SQL_SUCCESS_WITH_INFO

    m_hEnv = SQL_NULL_HENV;
    bSetOdbcVersion = false;
}

void Environment::SetEnvAttr(long lAttribute, long lValue)
{
    SQLRETURN ret;

    ret = ::SQLSetEnvAttr(  m_hEnv, 
                            lAttribute, 
                            reinterpret_cast<SQLPOINTER>(lValue),
                            SQL_IS_UINTEGER );
    if (!SQL_SUCCEEDED(ret)) 
    {
        wchar_t wszSqlState[6];
        SQLINTEGER lpNativeErrorPtr;
        wchar_t wszMessageText[512];
        SQLRETURN ret;

        ret = ::SQLGetDiagRecW( SQL_HANDLE_ENV,
                                m_hEnv,
                                1,
                                wszSqlState,
                                &lpNativeErrorPtr,
                                wszMessageText,
                                512,
                                0 );
        if (wcscmp(wszSqlState, L"HY000") == 0) // General error
        {
            ThrowReconnectException(__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HY009") == 0)    // Invalid use of null pointer
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid use of null pointer",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HY010") == 0)    // Function sequence error
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid handle",__WFILE__,__LINE__);
        }
        else if ((wcscmp(wszSqlState, L"HY024") == 0) ||    // Illegal value
                 (wcscmp(wszSqlState, L"HY001") == 0))      // Optional value not supported.    
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Illegal value",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HY090") == 0)    // Invalid string or buffer length
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid string or buffer length",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HYC00") == 0)    // Driver not implemented this function
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver not implemented this function",__WFILE__,__LINE__);
        }
        else if ((wcscmp(wszSqlState, L"HY013") == 0) ||    
                 (wcscmp(wszSqlState, L"HY001") == 0))
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Memory management error",__WFILE__,__LINE__);
        }
    }
    if (ret == SQL_SUCCESS_WITH_INFO) // Can only be 01000 or 01S02
    {
        wchar_t wszSqlState[6];
        SQLINTEGER lpNativeErrorPtr;
        wchar_t wszMessageText[256];
        SQLRETURN ret;

        ret = ::SQLGetDiagRecW( SQL_HANDLE_ENV,
                                m_hEnv,
                                1,
                                wszSqlState,
                                &lpNativeErrorPtr,
                                wszMessageText,
                                256,
                                0 );
        if (wcscmp(wszSqlState, L"01000") == 0)
        {
            wchar_t wszSqlState[6];
            SQLINTEGER lpNativeErrorPtr;
            wchar_t wszMessageText[256];
            SQLRETURN ret;

            ret = ::SQLGetDiagRecW( SQL_HANDLE_DBC,
                                    m_hEnv,
                                    1,
                                    wszSqlState,
                                    &lpNativeErrorPtr,
                                    wszMessageText,
                                    256,
                                    0 );
            Safir::SwReports::SendErrorReport(  L"Non Odbc Error",
                                                L"Safir::Databases::Odbc::Environment::SetEnvAttr()",
                                                wszMessageText );
        }
        if (wcscmp(wszSqlState, L"01S02") == 0)
        {
            Safir::SwReports::SendProgramInfoReport(L"Environment attribute not supported. A similiar attribute used instead");
        }
    }
}

void Environment::SetEnvAttr(long lAttribute, const std::wstring & wszValue, unsigned long ulLength)
{
    SQLRETURN ret;

    // const_cast is used because ValuePtr is declared as input in the ODBC 
    // specification and should be a const wchar_t *.
    ret = ::SQLSetEnvAttr(  m_hEnv, 
                            lAttribute, 
                            const_cast<wchar_t *>(wszValue.c_str()),
                            ulLength );
    if (!SQL_SUCCEEDED(ret)) 
    {
        wchar_t wszSqlState[6];
        SQLINTEGER lpNativeErrorPtr;
        wchar_t wszMessageText[512];
        SQLRETURN ret;

        ret = ::SQLGetDiagRecW( SQL_HANDLE_ENV,
                                m_hEnv,
                                1,
                                wszSqlState,
                                &lpNativeErrorPtr,
                                wszMessageText,
                                512,
                                NULL);
        if (wcscmp(wszSqlState, L"HY000") == 0) // General error
        {
            ThrowReconnectException(__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HY009") == 0)    // Invalid use of null pointer
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid use of null pointer",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HY010") == 0)    // Function sequence error
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid handle",__WFILE__,__LINE__);
        }
        else if ((wcscmp(wszSqlState, L"HY024") == 0) ||    // Illegal value
                 (wcscmp(wszSqlState, L"HY001") == 0))      // Optional value not supported.    
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Illegal value",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HY090") == 0)    // Invalid string or buffer length
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Invalid string or buffer length",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HYC00") == 0)    // Driver not implemented this function
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver not implemented this function",__WFILE__,__LINE__);
        }
        else if ((wcscmp(wszSqlState, L"HY013") == 0) ||    
                 (wcscmp(wszSqlState, L"HY001") == 0))
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Memory management error",__WFILE__,__LINE__);
        }
    }
    if (ret == SQL_SUCCESS_WITH_INFO) // Can only be 01000 or 01S02
    {
        wchar_t wszSqlState[6];
        SQLINTEGER lpNativeErrorPtr;
        wchar_t wszMessageText[256];
        SQLRETURN ret;

        ret = ::SQLGetDiagRecW( SQL_HANDLE_ENV,
                                m_hEnv,
                                1,
                                wszSqlState,
                                &lpNativeErrorPtr,
                                wszMessageText,
                                256,
                                0 );
        if (wcscmp(wszSqlState, L"01000") == 0)
        {
            wchar_t wszSqlState[6];
            SQLINTEGER lpNativeErrorPtr;
            wchar_t wszMessageText[256];
            SQLRETURN ret;

            ret = ::SQLGetDiagRecW( SQL_HANDLE_DBC,
                                    m_hEnv,
                                    1,
                                    wszSqlState,
                                    &lpNativeErrorPtr,
                                    wszMessageText,
                                    256,
                                    0 );
            Safir::SwReports::SendErrorReport(  L"Non Odbc Error",
                                                L"Safir::Databases::Odbc::Environment::SetEnvAttr()",
                                                wszMessageText );
        }
        if (wcscmp(wszSqlState, L"01S02") == 0)
        {
            Safir::SwReports::SendProgramInfoReport(L"Environment attribute not supported. A similiar attribute used instead");
        }
    }
}

void Environment::GetEnvAttr(long lAttribute, long & lValue) const
{
    SQLRETURN ret;

    ret = ::SQLGetEnvAttr(  m_hEnv, 
                            lAttribute, 
                            &lValue,
                            SQL_IS_UINTEGER,
                            NULL);
    if (!SQL_SUCCEEDED(ret)) 
    {
        wchar_t wszSqlState[6];
        SQLINTEGER lpNativeErrorPtr;
        wchar_t wszMessageText[512];
        SQLRETURN ret;

        ret = ::SQLGetDiagRecW( SQL_HANDLE_ENV,
                                m_hEnv,
                                1,
                                wszSqlState,
                                &lpNativeErrorPtr,
                                wszMessageText,
                                512,
                                0 );
        if (wcscmp(wszSqlState, L"HY000") == 0) // General error
        {
            ThrowReconnectException(__WFILE__,__LINE__);
        }
        else if ((wcscmp(wszSqlState, L"HY013") == 0) ||    
                 (wcscmp(wszSqlState, L"HY001") == 0))
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Memory management error",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HYC00") == 0)
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver not implemented this function",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HY092") == 0)
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Optional feature not implemented",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"IM001") == 0)    
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver does not support this function",__WFILE__,__LINE__);
        }
    }
    if (ret == SQL_SUCCESS_WITH_INFO) 
    {
        wchar_t wszSqlState[6];
        SQLINTEGER lpNativeErrorPtr;
        wchar_t wszMessageText[256];
        SQLRETURN ret;

        ret = ::SQLGetDiagRecW( SQL_HANDLE_ENV,
                                m_hEnv,
                                1,
                                wszSqlState,
                                &lpNativeErrorPtr,
                                wszMessageText,
                                256,
                                0 );
        if (wcscmp(wszSqlState, L"01000") == 0)
        {
            wchar_t wszSqlState[6];
            SQLINTEGER lpNativeErrorPtr;
            wchar_t wszMessageText[256];
            SQLRETURN ret;

            ret = ::SQLGetDiagRecW( SQL_HANDLE_DBC,
                                    m_hEnv,
                                    1,
                                    wszSqlState,
                                    &lpNativeErrorPtr,
                                    wszMessageText,
                                    256,
                                    0 );
            Safir::SwReports::SendErrorReport(  L"Non Odbc Error",
                                                L"Safir::Databases::Odbc::Environment::GetEnvAttr()",
                                                wszMessageText );
        }
        if (wcscmp(wszSqlState, L"01004") == 0)
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(wszMessageText, __WFILE__,__LINE__);
        }
    }
}

void Environment::GetEnvAttr(long lAttribute, wchar_t * wszValue, unsigned long ulLength) const
{
    SQLRETURN ret;

    ret = ::SQLGetEnvAttr(  m_hEnv, 
                            lAttribute, 
                            wszValue,
                            ulLength,
                            NULL );
    if (!SQL_SUCCEEDED(ret)) 
    {
        wchar_t wszSqlState[6];
        SQLINTEGER lpNativeErrorPtr;
        wchar_t wszMessageText[512];
        SQLRETURN ret;

        ret = ::SQLGetDiagRecW( SQL_HANDLE_ENV,
                                m_hEnv,
                                1,
                                wszSqlState,
                                &lpNativeErrorPtr,
                                wszMessageText,
                                512,
                                0 );
        if (wcscmp(wszSqlState, L"HY000") == 0) // General error
        {
            ThrowReconnectException(__WFILE__,__LINE__);
        }
        else if ((wcscmp(wszSqlState, L"HY013") == 0) ||    
                 (wcscmp(wszSqlState, L"HY001") == 0))
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Memory management error",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HYC00") == 0)
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver not implemented this function",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"HY092") == 0)
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Optional feature not implemented",__WFILE__,__LINE__);
        }
        else if (wcscmp(wszSqlState, L"IM001") == 0)    
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Driver does not support this function",__WFILE__,__LINE__);
        }
    }
    if (ret == SQL_SUCCESS_WITH_INFO) 
    {
        wchar_t wszSqlState[6];
        SQLINTEGER lpNativeErrorPtr;
        wchar_t wszMessageText[256];
        SQLRETURN ret;

        ret = ::SQLGetDiagRecW( SQL_HANDLE_ENV,
                                m_hEnv,
                                1,
                                wszSqlState,
                                &lpNativeErrorPtr,
                                wszMessageText,
                                256,
                                0 );
        if (wcscmp(wszSqlState, L"01000") == 0)
        {
            wchar_t wszSqlState[6];
            SQLINTEGER lpNativeErrorPtr;
            wchar_t wszMessageText[256];
            SQLRETURN ret;

            ret = ::SQLGetDiagRecW( SQL_HANDLE_DBC,
                                    m_hEnv,
                                    1,
                                    wszSqlState,
                                    &lpNativeErrorPtr,
                                    wszMessageText,
                                    256,
                                    0 );
            Safir::SwReports::SendErrorReport(  L"Non Odbc Error",
                                                L"Safir::Databases::Odbc::Environment::GetEnvAttr()",
                                                wszMessageText );
        }
        if (wcscmp(wszSqlState, L"01004") == 0)
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(wszMessageText, __WFILE__,__LINE__);
        }
    }
}

bool Environment::GetDiagRec(short sRecNumber,
                           std::wstring & SqlState,
                           boost::int32_t & NativeError,
                           std::wstring & MessageText,
                           bool & bDataRead) const
{
    wchar_t wszSqlState[6];
    wchar_t wszMessageText[SQL_MAX_MESSAGE_LENGTH];

    SQLRETURN ret;
    SQLINTEGER tmpNativeError;
    ret = ::SQLGetDiagRecW( SQL_HANDLE_ENV,
                            m_hEnv,
                            sRecNumber,
                            wszSqlState,
                            &tmpNativeError,
                            wszMessageText,
                            SQL_MAX_MESSAGE_LENGTH,
                            0 );
    NativeError = static_cast<boost::int32_t>(tmpNativeError);
    bDataRead = ((ret == SQL_SUCCESS) ||(ret == SQL_SUCCESS_WITH_INFO));
    SqlState = wszSqlState;
    MessageText = wszMessageText;
    return ((ret != SQL_NO_DATA) && (ret != SQL_INVALID_HANDLE) && (ret != SQL_ERROR));
}

void Environment::ThrowReconnectException(  const std::wstring & fileName,   
                                            const Safir::Dob::Typesystem::Int64 lineNumber) const
{
    ThrowReconnectException( m_hEnv, fileName, lineNumber );
}

void Environment::ThrowReconnectException(SQLHENV hEnv,
                                          const std::wstring & fileName,   
                                          const Safir::Dob::Typesystem::Int64 lineNumber) const
{
    wchar_t wszSqlState[6];
    SQLINTEGER lpNativeErrorPtr;
    wchar_t wszMessageText[512];
    SQLRETURN ret;

    ret = ::SQLGetDiagRecW( SQL_HANDLE_ENV,
                            hEnv,
                            1,
                            wszSqlState,
                            &lpNativeErrorPtr,
                            wszMessageText,
                            256,
                            0 );
    std::wstring string = wszSqlState;
    string += L":";
    string += wszMessageText;
    throw ReconnectException(string.c_str(),fileName,lineNumber);
}

} // End namespace Odbc

} // End namespace Databases

} // End namespace Safir
