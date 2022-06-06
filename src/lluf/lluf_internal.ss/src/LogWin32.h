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
#ifndef LOGWIN32_H
#define LOGWIN32_H

#ifdef _MSC_VER

#include <windows.h>
#include <string>
#include <boost/noncopyable.hpp>

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    class WindowsLogger :
            private boost::noncopyable
    {
    public:
        WindowsLogger(const std::wstring& processName);
        ~WindowsLogger();

        void Send(const WORD eventType, const std::wstring& log) const;

    private:

        HANDLE                                m_sourceHandle;
        std::wstring                          m_processName;
    };

}
}
}
#endif
#endif
