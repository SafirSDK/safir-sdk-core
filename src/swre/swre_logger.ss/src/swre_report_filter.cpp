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
#include "swre_report_filter.h"


#include <Safir/SwReports/Internal/FatalErrorReport.h>
#include <Safir/SwReports/Internal/ErrorReport.h>
#include <Safir/SwReports/Internal/ResourceReport.h>
#include <Safir/SwReports/Internal/ProgrammingErrorReport.h>
#include <Safir/SwReports/Internal/ProgramInfoReport.h>



const boost::wregex::flag_type regExpFlags = boost::regex::perl | boost::regex::icase;

namespace Safir
{
namespace Swre
{

//-----------------------------------------------------------------------------
ReportFilter::ReportFilter()
    : m_logFatalError(true),
      m_logError(true),
      m_logResource(true),
      m_logProgrammingError(true),
      m_logProgramInfo(true),
      m_connectionNameRegEx(L".*", regExpFlags),
      m_nodeNameRegEx(L".*", regExpFlags),
      m_includeIfConnectionMatch(true),
      m_includeIfNodeMatch(true)
{
}

//-----------------------------------------------------------------------------
ReportFilter::~ReportFilter()
{
}

//-----------------------------------------------------------------------------
ReportFilter& ReportFilter::Inst()
{
    static ReportFilter inst;
    return inst;
}

//-----------------------------------------------------------------------------
void ReportFilter::SetFatalErrorLogging(bool logOn)
{
    m_logFatalError = logOn;
}

//-----------------------------------------------------------------------------
void ReportFilter::SetErrorLogging(bool logOn)
{
    m_logError = logOn;
}

//-----------------------------------------------------------------------------
void ReportFilter::SetResourceLogging(bool logOn)
{
    m_logResource = logOn;
}

//-----------------------------------------------------------------------------
void ReportFilter::SetProgrammingErrorLogging(bool logOn)
{
    m_logProgrammingError = logOn;
}

//-----------------------------------------------------------------------------
void ReportFilter::SetProgramInfoLogging(bool logOn)
{
    m_logProgramInfo = logOn;
}

//-----------------------------------------------------------------------------
bool ReportFilter::SetConnectionNameRegEx(const std::wstring& regEx, bool includeIfMatch)
{
    try
    {
        m_connectionNameRegEx.assign(regEx, regExpFlags);
    }
    catch (boost::bad_expression /* e*/ )
    {
        return false;
    }

    m_includeIfConnectionMatch = includeIfMatch;

    return true;
}


bool ReportFilter::SetNodeNameRegEx(const std::wstring& regEx, bool includeIfMatch)
{

    try
    {
        m_nodeNameRegEx.assign(regEx, regExpFlags);
    }
    catch (boost::bad_expression /* e*/ )
    {
        return false;
    }

    m_includeIfNodeMatch = includeIfMatch;

    return true;
}


//-----------------------------------------------------------------------------
bool ReportFilter::ReportIsToBeLogged(const Safir::SwReports::Internal::ReportConstPtr report) const
{
    switch (report->GetTypeId())
    {
    case Safir::SwReports::Internal::FatalErrorReport::ClassTypeId:
        {
            if (!m_logFatalError)
            {
                return false;
            }
        }
        break;

        case Safir::SwReports::Internal::ErrorReport::ClassTypeId:
        {
            if (!m_logError)
            {
                return false;
            }
        }
        break;

        case Safir::SwReports::Internal::ResourceReport::ClassTypeId:
        {
            if (!m_logResource)
            {
                return false;
            }
        }
        break;

        case Safir::SwReports::Internal::ProgrammingErrorReport::ClassTypeId:
        {
            if (!m_logProgrammingError)
            {
                return false;
            }
        }
        break;

        case Safir::SwReports::Internal::ProgramInfoReport::ClassTypeId:
        {
            if (!m_logProgramInfo)
            {
                return false;
            }
        }
        break;  


        default:
        {
            return false;
        }
    }

    std::wstring connectionName;
    if (!report->ConnectionName().IsNull())
    {
        connectionName = report->ConnectionName().GetVal();     
    }

    std::wstring nodeName;
    if (!report->NodeName().IsNull())
    {
        nodeName = report->NodeName().GetVal();     
    }


    bool connectionMatch = boost::regex_match(connectionName, m_connectionNameRegEx);

    if (connectionMatch != m_includeIfConnectionMatch)
    {
        return false;
    }

    bool nodeMatch = boost::regex_match(nodeName, m_nodeNameRegEx);

    if (nodeMatch != m_includeIfNodeMatch)
    {
        return false;
    }

    return true;    
}




}
}
