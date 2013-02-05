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
#ifndef __SWRE_REPORT_FILTER_H
#define __SWRE_REPORT_FILTER_H

#include <Safir/SwReports/Internal/Report.h>
#include <boost/regex.hpp>

namespace Safir
{
namespace Swre
{

class ReportFilter
{

public:

    // Singleton handle
    static ReportFilter& Inst();

    ~ReportFilter();

    // Set loggin on/off for the different report types
    void SetFatalErrorLogging(bool logOn);
    void SetErrorLogging(bool logOn);
    void SetResourceLogging(bool logOn);
    void SetProgrammingErrorLogging(bool logOn);
    void SetProgramInfoLogging(bool logOn);

    // Method     : SetConnectionNameRegEx
    // Parameters : regEx [in] - A POSIX basic regular expression.
    //              includeIfMatch [in] - True =>  Include report if match.
    //                                     False => Don't include report if match.
    // Returns    : True if the regular expression is valid
    // Comments   : Sets the regular expression to be used for matching Connection Name. 
    bool SetConnectionNameRegEx(const std::wstring& regEx, bool includeIfMatch);

    // Method     : SetNodeNameRegEx
    // Parameters : regEx [in] - A POSIX basic regular expression.
    //              includeIfMatch [in] - True =>  Include report if match.
    //                                     False => Don't include report if match.
    // Returns    : True if the regular expression is valid
    // Comments   : Sets the regular expression to be used for matching Node Name. 
    bool SetNodeNameRegEx(const std::wstring& regEx, bool includeIfMatch);

    // Method     : ReportIsToBeLogged
    // Parameters : report [in] - A software report
    // Returns    : True if the software report shall be logged.
    // Comments   : Determines if a software report shall be logged. 
    bool ReportIsToBeLogged(const Safir::SwReports::Internal::ReportConstPtr report) const;
    
private:

    // This is a singleton so don't allow explicit construction
    ReportFilter();
    ReportFilter(const ReportFilter& rhs);

    bool m_logFatalError;
    bool m_logError;
    bool m_logResource;
    bool m_logProgrammingError;
    bool m_logProgramInfo;

    boost::wregex m_connectionNameRegEx;
    boost::wregex m_nodeNameRegEx;

    bool m_includeIfConnectionMatch;
    bool m_includeIfNodeMatch;
};
}
}


#endif
