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
#include <Safir/SwReports/Internal/FatalErrorReport.h>
#include <Safir/SwReports/Internal/ErrorReport.h>
#include <Safir/SwReports/Internal/ResourceReport.h>
#include <Safir/SwReports/Internal/ProgrammingErrorReport.h>
#include <Safir/SwReports/Internal/ProgramInfoReport.h>

#include <Safir/Time/TimeProvider.h>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <sstream>

#include "swre_text_serializer.h"

namespace Safir
{
namespace Swre
{

static const std::wstring nullIndStr = L"*** NOT DEFINED ***";

//-----------------------------------------------------------------------------
TextSerializer::TextSerializer()
    : m_includeTypeSequenceNumber(false)
{
}

//-----------------------------------------------------------------------------
TextSerializer::~TextSerializer()
{
}

//-----------------------------------------------------------------------------
TextSerializer& TextSerializer::Inst()
{
    static TextSerializer inst;
    return inst;
}

//-----------------------------------------------------------------------------
std::wstring TextSerializer::SerializeReport(const Safir::SwReports::Internal::ReportPtr report)
{
    std::wstring reportType;
    std::wstring reportSpecificInfo;

    switch (report->GetTypeId())
    {
        case Safir::SwReports::Internal::FatalErrorReport::ClassTypeId:
        {
            reportType = L"Fatal Error";

            GetCommonErrorInfo(reportSpecificInfo, boost::static_pointer_cast<Safir::SwReports::Internal::Error>(report));
        }
        break;

        case Safir::SwReports::Internal::ErrorReport::ClassTypeId:
        {
            reportType = L"Error";

            GetCommonErrorInfo(reportSpecificInfo, boost::static_pointer_cast<Safir::SwReports::Internal::Error>(report));
        }
        break;

        case Safir::SwReports::Internal::ResourceReport::ClassTypeId:
        {
            const Safir::SwReports::Internal::ResourceReportPtr resourceReport = boost::static_pointer_cast<Safir::SwReports::Internal::ResourceReport>(report);

            reportType = L"Resource";

            reportSpecificInfo = L"Resource Id => ";
            reportSpecificInfo += resourceReport->Id().IsNull() ? nullIndStr : resourceReport->Id().GetVal();
            reportSpecificInfo += L'\n';

            std::wostringstream oStr;
            resourceReport->Allocated().IsNull() ? (oStr << nullIndStr) : (oStr << std::boolalpha << resourceReport->Allocated().GetVal());
            reportSpecificInfo += L"Resource Allocated => ";
            reportSpecificInfo += oStr.str();
            reportSpecificInfo += L'\n';
        }
        break;

        case Safir::SwReports::Internal::ProgrammingErrorReport::ClassTypeId:
        {
            reportType = L"Programming Error";

            GetCommonErrorInfo(reportSpecificInfo, boost::static_pointer_cast<Safir::SwReports::Internal::Error>(report));
        }
        break;

        case Safir::SwReports::Internal::ProgramInfoReport::ClassTypeId:
        {
            reportType = L"Program Info";
        }
        break;

    }

    std::wstring serRep(L"========== Software Report Received ==========\n"
                        L"Type => ");

    serRep +=  reportType;
    serRep += L'\n';
    GetCommonInfo(serRep, report);
    serRep += reportSpecificInfo;
    GetText (serRep, report);
    serRep += L'\n';

    return serRep;
}

//-----------------------------------------------------------------------------
void TextSerializer::SetIncludeTypeSequenceNumber(bool on)
{
    m_includeTypeSequenceNumber = on;
}

//-----------------------------------------------------------------------------
void TextSerializer::GetCommonInfo(std::wstring& str,
                                   const Safir::SwReports::Internal::ReportPtr report)
{
    str += L"Time => ";

    std::wstring s =
        boost::posix_time::to_iso_extended_wstring
        (Safir::Time::TimeProvider::ToPtime(report->SourceTimestamp().GetVal()));
        //(Safir::Time::TimeProvider::GetTime(*report->SourceTimestamp().GetPtr()));
//     std::wstring s =
//        boost::posix_time::to_iso_extended_wstring
//        (report->SourceTimestamp().SetVal(Safir::Time::TimeProvider::GetUtcTime()));

     //report->SourceTimestamp().SetVal(Safir::Time::TimeProvider::GetUtcTime());
    // Remove microseconds from string (simply truncate the string)
    // (STAWI: Haven't yet find a way to tell the boost library to produce a string containing
    //  millisecond resolution)
    str += s.substr(0, s.size() - 3);
    str += L'\n';

    str += L"Application connection => ";
    str += report->ConnectionName().IsNull() ? nullIndStr : report->ConnectionName().GetVal();
    str += L'\n';

    std::wostringstream connSeqStr;
    report->ConnectionSequenceNumber().IsNull() ? (connSeqStr << nullIndStr) : (connSeqStr << report->ConnectionSequenceNumber().GetVal());
    str += L"Sequence number => ";
    str += connSeqStr.str();
    str += L'\n';

    if (m_includeTypeSequenceNumber)
    {
        std::wostringstream typeSeqStr;
        report->TypeSequenceNumber().IsNull() ? (typeSeqStr << nullIndStr) : (typeSeqStr << report->TypeSequenceNumber().GetVal());
        str += L"Report type sequence number => ";
        str += typeSeqStr.str();
        str += L'\n';
    }

    str += L"Node => ";
    str += report->NodeName().IsNull() ? nullIndStr : report->NodeName().GetVal();
    str += L'\n';
}

//-----------------------------------------------------------------------------
void TextSerializer::GetCommonErrorInfo(std::wstring& str,
                                        const Safir::SwReports::Internal::ErrorPtr errReport)
{
    str += L"Error Code => ";
    str += errReport->ErrorCode().IsNull() ? nullIndStr : errReport->ErrorCode().GetVal();
    str += L'\n';

    str += L"Location => ";
    str += errReport->Location().IsNull() ? nullIndStr : errReport->Location().GetVal();
    str += L'\n';
}

//-----------------------------------------------------------------------------
void TextSerializer::GetText(std::wstring& str,
                             const Safir::SwReports::Internal::ReportPtr report)
{
    str += L"Text =>\n";
    str += report->Text().IsNull() ? nullIndStr : report->Text().GetVal();
    str += L'\n';
}
}
}
