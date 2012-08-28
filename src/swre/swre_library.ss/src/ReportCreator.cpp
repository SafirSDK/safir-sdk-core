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
#include "ReportCreator.h"
#include <Safir/SwReports/Internal/FatalErrorReport.h>
#include <Safir/SwReports/Internal/ErrorReport.h>
#include <Safir/SwReports/Internal/ResourceReport.h>
#include <Safir/SwReports/Internal/ProgrammingErrorReport.h>
#include <Safir/SwReports/Internal/ProgramInfoReport.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Time/TimeProvider.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>

namespace Safir
{
namespace SwReports
{
namespace Internal
{

    ReportCreator::ReportType
    ReportCreator::GetReportType(const Safir::SwReports::Internal::ReportConstPtr & report)
    {
        switch(report->GetTypeId())
        {
        case FatalErrorReport::ClassTypeId:
            return FatalError;
        case ErrorReport::ClassTypeId:
            return Error;
        case ResourceReport::ClassTypeId:
            return Resource;
        case ProgrammingErrorReport::ClassTypeId:
            return ProgrammingError;
        case ProgramInfoReport::ClassTypeId:
            return ProgramInfo;
        default:
            return FatalError;
        }
    }

    //-----------------------------------------------------------------------------
    void ReportCreator::GetNextSequenceNumbers(const std::wstring & connectionName,
                                               const ReportType reportType,
                                               Safir::Dob::Typesystem::Int32 & connectionSeqNbr,
                                               Safir::Dob::Typesystem::Int32 & reportTypeSeqNbr)
    {
        boost::lock_guard<boost::mutex> lck(m_sequenceNumbersLock);

        SequenceNumberMap::iterator it = m_sequenceNumbers.find(connectionName);
        if (it != m_sequenceNumbers.end())
        {
            // The connection is already in the map

            // Increment connection seq nbr
            if ((*it).second.connectionSeqNbr >= int32Max)
            {
                (*it).second.connectionSeqNbr = 1;
            }
            else
            {
                ++((*it).second.connectionSeqNbr);
            }
            connectionSeqNbr = (*it).second.connectionSeqNbr;

            // Increment report type seq nbr
            if ((*it).second.reportTypeSeqNbr[reportType] >= int32Max)
            {
                (*it).second.reportTypeSeqNbr[reportType] = 1;
            }
            else
            {
                ++((*it).second.reportTypeSeqNbr[reportType]);
            }
            reportTypeSeqNbr = (*it).second.reportTypeSeqNbr[reportType];
        }
        else
        {
            // New connection. Insert it in map
            SeqNbrs seqNbrs;
            seqNbrs.reportTypeSeqNbr[reportType] = 1;
            m_sequenceNumbers[connectionName] = seqNbrs;

            connectionSeqNbr = seqNbrs.connectionSeqNbr;
            reportTypeSeqNbr = seqNbrs.reportTypeSeqNbr[reportType];
        }
    }

    //-----------------------------------------------------------------------------
    void
    ReportCreator::SetConnectionInfoIfNotSet(Safir::SwReports::Internal::ReportPtr report)
    {
        if (report->ConnectionName().IsNull())
        {
            try
            {
                Safir::Dob::SecondaryConnection conn;
                conn.Attach();
                SetConnectionInfoIfNotSet(report,conn);
            }
            catch(const Safir::Dob::NotOpenException &)
            {

            }
        }
    }

    //-----------------------------------------------------------------------------
    void ReportCreator::SetConnectionInfoIfNotSet(Safir::SwReports::Internal::ReportPtr report,
                                                  const Safir::Dob::ConnectionBase & connection)
    {
        if (report->ConnectionName().IsNull())
        {
            try
            {
                Safir::Dob::ConnectionAspectMisc connectionAspectMisc(connection);
                std::wstring connectionName = connectionAspectMisc.GetConnectionName();

                Safir::Dob::Typesystem::Int32 connectionSeqNbr;
                Safir::Dob::Typesystem::Int32 reportTypeSeqNbr;

                GetNextSequenceNumbers(connectionName,
                                       GetReportType(report),
                                       connectionSeqNbr,
                                       reportTypeSeqNbr);

                report->ConnectionSequenceNumber().SetVal(connectionSeqNbr);
                report->TypeSequenceNumber().SetVal(reportTypeSeqNbr);
                report->ConnectionName().SetVal(connectionName);
            }
            catch(const Safir::Dob::NotOpenException &)
            {

            }
        }
    }

    //-----------------------------------------------------------------------------
    void ReportCreator::SetCommonInfo(Safir::SwReports::Internal::ReportPtr report)
    {
        report->SourceTimestamp().SetVal(Safir::Time::TimeProvider::GetUtcTime());
        report->NodeName().SetVal(Safir::Dob::NodeParameters::Nodes(Safir::Dob::ThisNodeParameters::NodeNumber())->NodeName().GetVal());

        SetConnectionInfoIfNotSet(report);
    }

    void FixStringLength(Safir::Dob::Typesystem::StringContainer& stringContainer, const Safir::Dob::Typesystem::Int32 maxLength)
    {
        if (stringContainer.GetVal().length() > static_cast<size_t>(maxLength))
        {
            stringContainer.SetVal(stringContainer.GetVal().substr(0,maxLength));
        }
    }

    //-----------------------------------------------------------------------------
    ReportPtr
    ReportCreator::CreateFatalErrorReport(const std::wstring& errorCode,
                                          const std::wstring& location,
                                          const std::wstring& text)
    {
        Safir::SwReports::Internal::FatalErrorReportPtr report = Safir::SwReports::Internal::FatalErrorReport::Create();

        SetCommonInfo(report);

        report->ErrorCode().SetVal(errorCode);
        report->Location().SetVal(location);
        report->Text().SetVal(text);

        FixStringLength(report->ErrorCode().GetContainer(),Safir::SwReports::Internal::FatalErrorReport::ErrorCodeMaxStringLength());
        FixStringLength(report->Location().GetContainer(),Safir::SwReports::Internal::FatalErrorReport::LocationMaxStringLength());
        FixStringLength(report->Text().GetContainer(),Safir::SwReports::Internal::FatalErrorReport::TextMaxStringLength());

        return report;
    }

    //-----------------------------------------------------------------------------
    ReportPtr
    ReportCreator::CreateErrorReport(const std::wstring& errorCode,
                                     const std::wstring& location,
                                     const std::wstring& text)
    {
        Safir::SwReports::Internal::ErrorReportPtr report = Safir::SwReports::Internal::ErrorReport::Create();

        SetCommonInfo(report);

        report->ErrorCode().SetVal(errorCode);
        report->Location().SetVal(location);
        report->Text().SetVal(text);

        FixStringLength(report->ErrorCode().GetContainer(),Safir::SwReports::Internal::ErrorReport::ErrorCodeMaxStringLength());
        FixStringLength(report->Location().GetContainer(),Safir::SwReports::Internal::ErrorReport::LocationMaxStringLength());
        FixStringLength(report->Text().GetContainer(),Safir::SwReports::Internal::ErrorReport::TextMaxStringLength());

        return report;
    }

    //-----------------------------------------------------------------------------
    ReportPtr
    ReportCreator::CreateResourceReport(const std::wstring& resourceId,
                                        bool                allocated,
                                        const std::wstring& text)
    {
        Safir::SwReports::Internal::ResourceReportPtr report = Safir::SwReports::Internal::ResourceReport::Create();

        SetCommonInfo(report);

        report->Id().SetVal(resourceId);
        report->Allocated().SetVal(allocated);
        report->Text().SetVal(text);

        FixStringLength(report->Id().GetContainer(),Safir::SwReports::Internal::ResourceReport::IdMaxStringLength());
        FixStringLength(report->Text().GetContainer(),Safir::SwReports::Internal::ResourceReport::TextMaxStringLength());

        return report;
    }

    //-----------------------------------------------------------------------------
    ReportPtr
    ReportCreator::CreateProgrammingErrorReport(const std::wstring& errorCode,
                                                const std::wstring& location,
                                                const std::wstring& text)
    {
        Safir::SwReports::Internal::ProgrammingErrorReportPtr report = Safir::SwReports::Internal::ProgrammingErrorReport::Create();

        SetCommonInfo(report);

        report->ErrorCode().SetVal(errorCode);
        report->Location().SetVal(location);
        report->Text().SetVal(text);

        FixStringLength(report->ErrorCode().GetContainer(),Safir::SwReports::Internal::ProgrammingErrorReport::ErrorCodeMaxStringLength());
        FixStringLength(report->Location().GetContainer(),Safir::SwReports::Internal::ProgrammingErrorReport::LocationMaxStringLength());
        FixStringLength(report->Text().GetContainer(),Safir::SwReports::Internal::ProgrammingErrorReport::TextMaxStringLength());

        return report;
    }

    //-----------------------------------------------------------------------------
    ReportPtr
    ReportCreator::CreateProgramInfoReport(const std::wstring& text)
    {
        Safir::SwReports::Internal::ProgramInfoReportPtr report = Safir::SwReports::Internal::ProgramInfoReport::Create();

        SetCommonInfo(report);

        report->Text().SetVal(text);

        FixStringLength(report->Text().GetContainer(),Safir::SwReports::Internal::ProgramInfoReport::TextMaxStringLength());

        return report;
    }

}
}
}
