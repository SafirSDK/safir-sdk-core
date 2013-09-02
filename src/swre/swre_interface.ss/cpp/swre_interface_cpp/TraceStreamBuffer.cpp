/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Application/Internal/TraceStreamBuffer.h>
#include <Safir/SwReports/Internal/Interface.h>

namespace Safir
{
namespace Application
{
namespace Internal
{
    //-------------------------------------------------------
    TraceStreamBuffer::TraceStreamBuffer(const std::wstring & prefix):
        m_prefix(prefix),
        m_prefixId(0)
    {
        
    }

    //-------------------------------------------------------
    TraceStreamBuffer::~TraceStreamBuffer()
    {
        Flush();
    }

    //-------------------------------------------------------
    TraceStreamBuffer::_Tr::int_type 
    TraceStreamBuffer::uflow()
    {
        return _Tr::eof();
    }

    //-------------------------------------------------------
    TraceStreamBuffer::_Tr::int_type 
    TraceStreamBuffer::underflow()
    {
        return _Tr::eof();
    }

    //-------------------------------------------------------
    TraceStreamBuffer::_Tr::int_type 
    TraceStreamBuffer::overflow(_Tr::int_type c)
    {
        if (m_prefixId == 0)
        {
            AddPrefix();
        }
        bool success;
        SwreC_TraceAppendWcharPrefix(m_prefixId,_Tr::to_char_type(c),success);
        if (!success)
        {
            Safir::Dob::Typesystem::LibraryExceptions::Instance().Throw();
        }
        return _Tr::not_eof(c);
    }

    using Safir::Dob::Typesystem::Utilities::ToUtf8;

    //-------------------------------------------------------
    std::streamsize TraceStreamBuffer::xsputn(const wchar_t* s, std::streamsize num)
    {
        if (m_prefixId == 0)
        {
            AddPrefix();
        }
        bool success;
        SwreC_TraceAppendStringPrefix(m_prefixId, ToUtf8(std::wstring(s, num)).c_str(), success);
        if (!success)
        {
            Safir::Dob::Typesystem::LibraryExceptions::Instance().Throw();
        }
        return num;
    }

    //-------------------------------------------------------
    int 
    TraceStreamBuffer::sync()
    {
        Flush();
        return 0;
    }

    void
    TraceStreamBuffer::Flush()
    {
        bool success;
        SwreC_TraceFlushBuffer(success);
        if (!success)
        {
            Safir::Dob::Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    Safir::Dob::Typesystem::Int64 TraceStreamBuffer::GetPrefixId() const 
    {
        if (m_prefixId == 0)
        {
            AddPrefix();
        } 
        
        return m_prefixId;
    }

    void
    TraceStreamBuffer::AddPrefix() const
    {
        if (m_prefixId == 0)
        { 
            bool success;
            SwreC_TracePrefixAdd(ToUtf8(m_prefix).c_str(), m_prefixId, success);
            if (!success)
            {
                Safir::Dob::Typesystem::LibraryExceptions::Instance().Throw();
            }
        }
    }
}
}
}


