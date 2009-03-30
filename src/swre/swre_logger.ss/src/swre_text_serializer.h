/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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
#ifndef __SWRE_TEXT_SERIALIZER_H
#define __SWRE_TEXT_SERIALIZER_H

#include <Safir/SwReports/Internal/Report.h>
#include <Safir/SwReports/Internal/Error.h>

#include <string>

namespace Safir
{
namespace Swre
{

class TextSerializer
{

public:

    // Singleton handle
    static TextSerializer& Inst();

    ~TextSerializer();

    std::wstring SerializeReport(const Safir::SwReports::Internal::ReportPtr report);

    // Determines if the type sequence number shall be included in the
    // serialized report
    void SetIncludeTypeSequenceNumber(bool on);

private:

    // This is a singleton so don't allow explicit construction
    TextSerializer();
    TextSerializer(const TextSerializer& rhs);

    void GetCommonInfo(std::wstring& str,
                       const Safir::SwReports::Internal::ReportPtr report);

    void GetCommonErrorInfo(std::wstring& str,
                            const Safir::SwReports::Internal::ErrorPtr errReport);

    void GetText(std::wstring& str,
                 const Safir::SwReports::Internal::ReportPtr errReport);

    bool m_includeTypeSequenceNumber;

};
}
}


#endif
