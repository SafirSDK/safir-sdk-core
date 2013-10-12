/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#ifndef __SWRE_TRACESTREAMBUFFER_H__
#define __SWRE_TRACESTREAMBUFFER_H__

#include <streambuf>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Application/Internal/SwReportExportDefs.h>

namespace Safir
{
namespace Application
{
namespace Internal
{

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable:4251)
#endif

    class SWRE_API TraceStreamBuffer :
        public std::basic_streambuf<wchar_t, std::char_traits<wchar_t> >
    {
        typedef std::char_traits<wchar_t> _Tr;
    public:
        explicit TraceStreamBuffer(const std::wstring & prefix);
        virtual ~TraceStreamBuffer();

        Safir::Dob::Typesystem::Int64 GetPrefixId() const;
    private:
        virtual _Tr::int_type uflow();
        virtual _Tr::int_type underflow();
        virtual _Tr::int_type overflow(_Tr::int_type c = _Tr::eof());

        // Take care of character sequences
        virtual std::streamsize xsputn(const wchar_t* s, std::streamsize num);
        virtual int sync();

        void AddPrefix() const;

        std::wstring m_prefix;
        mutable Safir::Dob::Typesystem::Int64 m_prefixId;

    };

#ifdef _MSC_VER
#pragma warning (pop)
#endif

}
}
}

#endif

