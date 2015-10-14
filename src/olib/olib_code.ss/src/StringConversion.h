/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
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
#include <sstream>
#include <vector>
#include <boost/static_assert.hpp>

#ifndef __OLIB_STRING_CONVERSION_H__
#define __OLIB_STRING_CONVERSION_H__

namespace 
{
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#  define NO_WCHAR_CONVERSION
#elif defined(SAFIR_ODBC_IS_IODBC)
#  define NO_WCHAR_CONVERSION
#elif defined (SAFIR_ODBC_IS_UNIXODBC)
    //conversion needed
#else
#error "Can't work out if conversion is needed"
#endif

#ifndef NO_WCHAR_CONVERSION
    static inline size_t strlen(const SQLWCHAR* str)
    {
        size_t num = 0;
        while (*str != 0)
        {
            ++str;
            ++num;
        }
        return num;
    }
#endif

    static inline const std::wstring ToWstring(const SQLWCHAR* str)
    {
#ifdef NO_WCHAR_CONVERSION
        return std::wstring(str);
#else
        return std::wstring(str, str + strlen(str));
#endif
}

    static inline const std::wstring ToWstring(const std::vector<SQLWCHAR> str)
    {
#ifdef NO_WCHAR_CONVERSION
        return std::wstring(&str[0]);
#else
        std::wostringstream ostr;
        ostr << &str[0];
        return ostr.str();
#endif
}

// const_cast is used because string inputs are declared as input in the ODBC
// specification and should be a const wchar_t *.
#ifdef NO_WCHAR_CONVERSION
    static inline SQLWCHAR* ToSqlWchars(const std::wstring& str)
    {
        return const_cast<SQLWCHAR*>(str.c_str());
    }
#else
    class ToSqlWchars
    {
    public:
        explicit ToSqlWchars(const std::wstring& str)
            : m_chars(str.begin(),str.end())
        {m_chars.push_back(0);} //null termination!
        
        operator SQLWCHAR*()
        {
            return &m_chars[0];
        }
    private:
        std::vector<SQLWCHAR> m_chars;
    };
#endif

    static inline bool Equal(const SQLWCHAR* left, const wchar_t* right)
    {
#ifdef NO_WCHAR_CONVERSION
        return wcscmp(left,right) == 0;
#else
        return wcscmp(ToWstring(left).c_str(),right) == 0;
#endif
    }


    //not meant to be called, ever.
    static inline void size_checks()
    {
#ifdef NO_WCHAR_CONVERSION 
        BOOST_STATIC_ASSERT(sizeof(SQLWCHAR) == sizeof(wchar_t));
#else
        BOOST_STATIC_ASSERT(sizeof(SQLWCHAR) == 2);
        BOOST_STATIC_ASSERT(sizeof(wchar_t) == 4);
#endif
    }
    
}

#endif

