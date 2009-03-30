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
#ifndef __SWRE_REPORT_CREATOR_H__
#define __SWRE_REPORT_CREATOR_H__

#include <string>
#include <Safir/Dob/Typesystem/Defs.h>
#include <vector>
#include <map>
#include <Safir/SwReports/Internal/Report.h>
#include <Safir/Dob/Connection.h>
#include <ace/Recursive_Thread_Mutex.h>


namespace Safir
{
namespace SwReports
{
namespace Internal
{

    class ReportCreator
    {
    public:
        ReportPtr CreateFatalErrorReport(const std::wstring & errorCode,
                                         const std::wstring & location,
                                         const std::wstring & text);


        ReportPtr CreateErrorReport(const std::wstring & errorCode,
                                    const std::wstring & location,
                                    const std::wstring & text);

        ReportPtr CreateResourceReport(const std::wstring & resourceId,
                                       const bool allocated,
                                       const std::wstring & text);

        ReportPtr CreateProgrammingErrorReport(const std::wstring & errorCode,
                                               const std::wstring & location,
                                               const std::wstring & text);

        ReportPtr CreateProgramInfoReport(const std::wstring & text);

        //Set info using a specific connection (only for use in atexit handler)
        void SetConnectionInfoIfNotSet(Safir::SwReports::Internal::ReportPtr report,
                                       const Safir::Dob::ConnectionBase & connection);

        //set using the default connection of this thread
        void SetConnectionInfoIfNotSet(Safir::SwReports::Internal::ReportPtr report);
    private:

        enum ReportType
        {
            FatalError,
            Error,
            Resource,
            ProgrammingError,
            ProgramInfo,

            NbrOfReportTypes    // Keep last
        };

        static ReportType GetReportType(const Safir::SwReports::Internal::ReportConstPtr & report);

        void GetNextSequenceNumbers(const std::wstring & connectionName,
                                    const ReportType reportType,
                                    Safir::Dob::Typesystem::Int32 & connectionSeqNbr,
                                    Safir::Dob::Typesystem::Int32 & reportTypeSeqNbr);

        void SetCommonInfo(Safir::SwReports::Internal::ReportPtr report);



        // Currently the DOB doesn't export constants for max ranges so we have to define our own
        static const int int32Max = 2147483647;

        struct SeqNbrs
        {
            SeqNbrs(): connectionSeqNbr(1), reportTypeSeqNbr(NbrOfReportTypes, 0){}

            Safir::Dob::Typesystem::Int32 connectionSeqNbr;
            std::vector<Safir::Dob::Typesystem::Int32> reportTypeSeqNbr;
        };

        typedef std::map <std::wstring, SeqNbrs> SequenceNumberMap;

        SequenceNumberMap m_sequenceNumbers;
        ACE_Recursive_Thread_Mutex m_sequenceNumbersLock;
    };


}
}
}
#endif

