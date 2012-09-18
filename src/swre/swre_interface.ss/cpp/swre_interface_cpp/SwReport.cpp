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
#include "Safir/SwReports/SwReport.h"

#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/SwReports/Internal/Interface.h>

namespace Safir 
{ 
namespace SwReports 
{
    void Start(const bool crashReporting)
    {
        bool success;
        SwreC_Start(crashReporting, success);
        if (!success)
        {
            Safir::Dob::Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void Stop()
    {
        SwreC_Stop();
    }

    using Safir::Dob::Typesystem::Utilities::ToUtf8;

    //-----------------------------------------------------------------------------
    void SendFatalErrorReport(const std::wstring& errorCode,
                              const std::wstring& location,
                              const std::wstring& text)
    {
        bool success;
        SwreC_SendFatalErrorReport(ToUtf8(errorCode).c_str(),ToUtf8(location).c_str(),ToUtf8(text).c_str(),success);
        if (!success)
        {
            Safir::Dob::Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    //-----------------------------------------------------------------------------
    void SendErrorReport(const std::wstring& errorCode,
                         const std::wstring& location,
                         const std::wstring& text)
    {
        bool success;
        SwreC_SendErrorReport(ToUtf8(errorCode).c_str(),ToUtf8(location).c_str(),ToUtf8(text).c_str(),success);
        if (!success)
        {
            Safir::Dob::Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    //-----------------------------------------------------------------------------
    void SendResourceReport(const std::wstring& resourceId,
                            const bool          allocated,
                            const std::wstring& text)
    {
        bool success;
        SwreC_SendResourceReport(ToUtf8(resourceId).c_str(),allocated,ToUtf8(text).c_str(),success);
        if (!success)
        {
            Safir::Dob::Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    //-----------------------------------------------------------------------------
    void SendProgrammingErrorReport(const std::wstring& errorCode,
                                    const std::wstring& location,
                                    const std::wstring& text)
    {
        bool success;
        SwreC_SendProgrammingErrorReport(ToUtf8(errorCode).c_str(),ToUtf8(location).c_str(),ToUtf8(text).c_str(),success);
        if (!success)
        {
            Safir::Dob::Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    //-----------------------------------------------------------------------------
    void SendProgramInfoReport(const std::wstring& text)
    {
        bool success;
        SwreC_SendProgramInfoReport(ToUtf8(text).c_str(),success);
        if (!success)
        {
            Safir::Dob::Typesystem::LibraryExceptions::Instance().Throw();
        }
    }
}
}
