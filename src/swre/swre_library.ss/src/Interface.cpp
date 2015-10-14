/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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
#include <Safir/SwReports/Internal/Interface.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Logging/Log.h>
#include "Library.h"
#include <iostream>


using namespace Safir::SwReports::Internal;


std::wstring ToWstring(const char * const str)
{
    return Safir::Dob::Typesystem::Utilities::ToWstring(str);
}

void SwreC_StartTraceBackdoor(const char * const connectionNameCommonPart,
                              const char * const connectionNameInstancePart,
                              bool & success)
{
    using Safir::Dob::Typesystem::Utilities::ToWstring;

    success = false;
    try
    {
        Library::Instance().StartTraceBackdoor(ToWstring(connectionNameCommonPart),
                                               ToWstring(connectionNameInstancePart));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void SwreC_StopTraceBackdoor()
{
    try
    {
        Library::Instance().StopTraceBackdoor();
    }
    catch (...)
    {
        Safir::Logging::SendSystemLog
            (Safir::Logging::Error,
             L"Got an exception in SwreC_StopTraceBackdoor. "
             L"Please tell your nearest Safir SDK Core developer!");
    }
}

void SwreC_StartCrashReporting(bool & success)
{
    success = false;
    try
    {
        Library::Instance().StartCrashReporting();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


void SwreC_StopCrashReporting()
{
    try
    {
        Library::Instance().StopCrashReporting();
    }
    catch (...)
    {
        Safir::Logging::SendSystemLog
            (Safir::Logging::Error,
             L"Got an exception in SwreC_StopCrashReporting. "
             L"Please tell your nearest Safir SDK Core developer!");
    }
}

void SwreC_TraceAppendString(const Safir::Dob::Typesystem::Int64 prefixId,
                             const char * const str,
                             bool & success)
{
    success = false;
    try
    { 
        Library::Instance().TraceString(prefixId, str);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS
}

void SwreC_TraceAppendSubstring(const Safir::Dob::Typesystem::Int64 prefixId,
                                const char * const str,
                                const Safir::Dob::Typesystem::Int32 offset,
                                const Safir::Dob::Typesystem::Int32 length,
                                bool & success)
{
    success = false;
    try
    { 
        Library::Instance().TraceString(prefixId, 
                                        str, 
                                        static_cast<size_t>(offset), 
                                        static_cast<size_t>(length));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS
}


void SwreC_TraceAppendChar(const Safir::Dob::Typesystem::Int64 prefixId,
                           const char ch,
                           bool & success)
{
    success = false;
    try
    {
        Library::Instance().TraceChar(prefixId, ch);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS
}

void SwreC_TraceAppendWChar(const Safir::Dob::Typesystem::Int64 prefixId,
                            const wchar_t ch,
                            bool & success)
{
    success = false;
    try
    {
        Library::Instance().TraceWChar(prefixId, ch);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS
}


void SwreC_TraceFlush(bool & success)
{
    success = false;
    try
    {
        Library::Instance().TraceFlush();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void
SwreC_TracePrefixAdd(const char * const prefix,
                     Safir::Dob::Typesystem::Int64 & prefixId,
                     bool & success)
{
    success = false;
    try
    {
        prefixId = Library::Instance().AddPrefix(Safir::Dob::Typesystem::Utilities::ToWstring(prefix));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void 
SwreC_TracePrefixSetEnabled(const Safir::Dob::Typesystem::Int64 id,
                            const bool enabled,
                            bool & success)
{
    success = false;
    try
    {
        Library::Instance().EnablePrefix(id,enabled);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

bool
SwreC_TracePrefixIsEnabled(const Safir::Dob::Typesystem::Int64 id)
{
    return Library::Instance().IsEnabledPrefix(id);
}

volatile bool * 
SwreC_TracePrefixGetIsEnabledPointer(const Safir::Dob::Typesystem::Int64 id)
{
    return Library::Instance().GetPrefixStatePointer(id);
}

void SwreC_SendFatalErrorReport(const char * const errorCode,
                                const char * const location,
                                const char * const text,
                                bool & success)
{
    success = false;

    try
    {
        Library::Instance().SendFatalErrorReport(ToWstring(errorCode),ToWstring(location),ToWstring(text));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


void SwreC_SendErrorReport(const char * const errorCode,
                           const char * const location,
                           const char * const text,
                           bool & success)
{
    success = false;

    try
    {
        Library::Instance().SendErrorReport(ToWstring(errorCode),ToWstring(location),ToWstring(text));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void SwreC_SendResourceReport(const char * const resourceId,
                              bool               allocated,
                              const char * const text,
                              bool & success)
{
    success = false;

    try
    {
        Library::Instance().SendResourceReport(ToWstring(resourceId),allocated,ToWstring(text));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void SwreC_SendProgrammingErrorReport(const char * const errorCode,
                                      const char * const location,
                                      const char * const text,
                                      bool & success)
{
    success = false;

    try
    {
        Library::Instance().SendProgrammingErrorReport(ToWstring(errorCode),ToWstring(location),ToWstring(text));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


void SwreC_SendProgramInfoReport(const char * const text,
                                 bool & success)
{
    success = false;

    try
    {
        Library::Instance().SendProgramInfoReport(ToWstring(text));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}






