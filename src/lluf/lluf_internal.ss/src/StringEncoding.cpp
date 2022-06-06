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

#include <Safir/Utilities/Internal/StringEncoding.h>

#include <string>

namespace Safir
{
namespace Utilities
{
namespace Internal
{

const std::string ToUtf8(const std::wstring& wstr)
{
    if (wstr.empty())
    {
        return std::string();
    }

    char *pszBuf = new char[wstr.length() * 4 + 1];
    char *psz;
    unsigned long pos;


    std::wstring::const_iterator it;
    for( it = wstr.begin(), psz = pszBuf; it != wstr.end(); ++it )
    {
        pos = *it;
        if (pos < 0x80)
        {
            *psz++ = (char) pos;
        }
        else if (pos < 0x800)
        {
            *psz++ = (char) (0xC0 + (pos >> 6));
            *psz++ = (char) (0x80 + (pos & 0x3F));
        }
        else if (pos < 0x10000)
        {
            *psz++ = (char) (0xE0 + (pos >> 12));
            *psz++ = (char) (0x80 + ((pos >> 6) & 0x3F));
            *psz++ = (char) (0x80 + (pos & 0x3F));
        }
        else
            *psz++ = '#'; // Only the BMP is supported.
    }

    std::string str( pszBuf, psz - pszBuf );

    delete [] pszBuf;
    return str;
}

const std::wstring ToUtf16(const std::string& str)
{
    if (str.empty())
    {
        return std::wstring();
    }

    int left = 0;
    wchar_t *pwszBuf = new wchar_t[str.length() + 1];
    wchar_t *pwsz;
    unsigned long pos;

    pwsz = pwszBuf;

    std::string::const_iterator it;
    for( it = str.begin(); it != str.end(); ++it)
    {
        pos = (unsigned char) *it;
        if ((left == 0) ^ ((pos & 0xC0) != 0x80)) // Continuation byte mismatch
        {
            left = 0;
            *pwsz++ = L'#';
        }

        if (pos < 0x80) // 7-bit ASCII
        {
            *pwsz++ = (wchar_t) pos;
        }
        else if ((pos & 0xC0) == (0x80)) // Correct continuation
        {
            left--;
            *pwsz = (*pwsz << 6) + (wchar_t) (pos & 0x3F);
            if (left == 0)
                pwsz++;
        }
        else if ((pos & 0xE0) == (0xC0)) // First of 2
        {
            *pwsz = (wchar_t) (pos & 0x1F);
            left = 1;
        }
        else if ((pos & 0xF0) == (0xE0)) // First of 3
        {
            *pwsz = (wchar_t) (pos & 0x0F);
            left = 2;
        }
        else // Only the BMP is supported.
        {
            left = 0;
            *pwsz++ = L'#';
        }

    }

    std::wstring wstr( pwszBuf, pwsz - pwszBuf );

    delete [] pwszBuf;
    return wstr;
}
}
}
}

