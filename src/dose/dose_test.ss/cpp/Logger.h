/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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

#ifndef __LOGGER_H__
#define __LOGGER_H__
#include <Safir/Dob/ConnectionInfo.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <boost/noncopyable.hpp>
#include <iostream>
#include <sstream>
#define lout Logger::Instance()


class Logger:
    private boost::noncopyable
{
public:
    typedef std::basic_ostream<wchar_t, std::char_traits<wchar_t> > stream_type;
    typedef std::basic_ios<wchar_t, std::char_traits<wchar_t> > ios_type;

    static Logger & Instance();

    std::wstring Dump();

    void Clear() {m_buf.str(L"");}

    inline Logger& operator << (const Safir::Dob::ConnectionInfoPtr& connInfo)
    {
        connInfo->ConnectionId().SetNull();
        if (!connInfo->ConnectionName().IsNull())
        {
            const size_t index = connInfo->ConnectionName().GetVal().find_last_of(L"#");
            connInfo->ConnectionName() = connInfo->ConnectionName().GetVal().substr(0,index);
        }
        const std::wstring xml = Safir::Dob::Typesystem::Serialization::ToXml(connInfo);
        std::wcout << xml;
        m_buf << xml;

        if (m_buf.str().size() > BUF_MAX_SIZE)
        {
            m_buf.str(L"");
            m_buf << "buffer overflowed, clearing" << std::endl;
        }
        return *this;
    }

    inline Logger & operator << (stream_type & (* _Pfn)(stream_type&))
    {
        (*_Pfn)(std::wcout);
        (*_Pfn)(m_buf);

        if (m_buf.str().size() > BUF_MAX_SIZE)
        {
            m_buf.str(L"");
            m_buf << "buffer overflowed, clearing" << std::endl;
        }
        return *this;
    }

    inline Logger & operator << (std::ios_base & (* _Pfn)(std::ios_base &))
    {
        (*_Pfn)(std::wcout);
        (*_Pfn)(m_buf);

        if (m_buf.str().size() > BUF_MAX_SIZE)
        {
            m_buf.str(L"");
            m_buf << "buffer overflowed, clearing" << std::endl;
        }
        return *this;
    }

    Logger& operator<<(ios_type & (*_Pfn)(ios_type&))
    {
        (*_Pfn)(std::wcout);
        (*_Pfn)(m_buf);

        if (m_buf.str().size() > BUF_MAX_SIZE)
        {
            m_buf.str(L"");
            m_buf << "buffer overflowed, clearing" << std::endl;
        }

        return *this;
    }

    template <class T>
    inline Logger & operator << (const T & data)
    {
        std::wcout << data;
        m_buf << data;

        if (m_buf.str().size() > BUF_MAX_SIZE)
        {
            m_buf.str(L"");
            m_buf << "buffer overflowed, clearing" << std::endl;
        }

        return *this;
    }
private:
    Logger();
    ~Logger();

    static const size_t BUF_MAX_SIZE = 1000000; //1M

    std::wostringstream m_buf;
};

#endif

