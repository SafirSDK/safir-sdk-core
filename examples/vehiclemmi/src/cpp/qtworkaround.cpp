/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
*
* Created by: Petter Lönnstedt / stpeln
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
#include "qtworkaround.h"

namespace VehicleMmiCppQt
{
    
    QtWorkaround::QtWorkaround()
    {

    }


    QtWorkaround::~QtWorkaround()
    {

    }


    QString QtWorkaround::StdWStringToQString(const std::wstring& rcwstrString)
    {
        return WCharArrayToQString(rcwstrString.data(), int(rcwstrString.size()));
    }


    std::wstring QtWorkaround::QStringToStdWString(const QString& rcqstrString)
    {
        std::wstring wstrString;
        wstrString.resize(rcqstrString.length());

     #if defined(_MSC_VER) && _MSC_VER >= 1400
        // VS2005 crashes if the string is empty
        if (!rcqstrString.length())
        {
            return wstrString;
        }
     #endif

        wstrString.resize(QStringToWCharArray(rcqstrString,&(*wstrString.begin())));
        return wstrString;
    }

//disable "conditional expression is constant" for the sizeof checks below.
#ifdef _MSC_VER
# pragma warning (push)
# pragma warning (disable: 4127)
#endif

    int QtWorkaround::QStringToWCharArray(const QString& rcqstrString, wchar_t *pArray)
    {
        if (sizeof(wchar_t) == sizeof(QChar))
        {
            memcpy(pArray, rcqstrString.utf16(), sizeof(wchar_t)*rcqstrString.length());
            return rcqstrString.length();
        }
        else
        {
            wchar_t *a = pArray;
            const unsigned short *uc = rcqstrString.utf16();
            for (int i = 0; i < rcqstrString.length(); ++i)
            {
                uint u = uc[i];
                if (u >= 0xd800 && u < 0xdc00 && i < rcqstrString.length()-1)
                {
                    ushort low = uc[i+1];
                    if (low >= 0xdc00 && low < 0xe000)
                    {
                        ++i;
                        u = (u - 0xd800)*0x400 + (low - 0xdc00) + 0x10000;
                    }
                }
                *a = wchar_t(u);
                ++a;
            }
            return static_cast<int>(a - pArray);
        }
    }


    QString QtWorkaround::WCharArrayToQString(const wchar_t *pcString, int nSize)
    {
        if (sizeof(wchar_t) == sizeof(QChar))
        {
            return QString::fromUtf16((ushort *)pcString, nSize);
        }
        else
        {
            return QString::fromUcs4((uint *)pcString, nSize);
        }
    }

#ifdef _MSC_VER
# pragma warning (pop)
#endif

} // namespace VehicleMmiCppQt

