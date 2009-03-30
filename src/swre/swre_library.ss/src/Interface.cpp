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
#include <Safir/SwReports/Internal/Interface.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include "Library.h"
#include <iostream>

using namespace Safir::SwReports::Internal;


std::wstring ToWstring(const char * const str)
{
    return Safir::Dob::Typesystem::Utilities::ToWstring(str);
}

void SwreC_SetProgramName(const char * const programName,
                          bool & success)
{
    success = false;
    try
    {
        Library::Instance().SetProgramName(ToWstring(programName));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}


void SwreC_Stop()
{
    try
    {
        Library::Instance().Stop();
    }
    catch (...)
    {
        std::wcerr << "Got an exception in SwreC_Stop. Please tell your nearest Safir System Kernel developer" << std::endl;
    }
}

void SwreC_TraceAppendString(const char * const /*str*/,
                             bool & /*success*/)
{
    /*success = false;
    try
    {
        const std::wstring wstr = Safir::Dob::Typesystem::Utilities::ToWstring(str);
        Library::Instance().Trace(wstr);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS*/
}

void 
SwreC_TraceAppendStringPrefix(const Safir::Dob::Typesystem::Int64 prefixId,
                              const char * const str,
                              bool & success)
{
    success = false;
    try
    { 
        Library::Instance().TraceString(prefixId, Safir::Dob::Typesystem::Utilities::ToWstring(str));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS
}

void SwreC_TraceAppendChar(const char /*ch*/,
                           bool & /*success*/)
{/*
 success = false;
 try
 {
 //TODO
 success = true;
 }
 CATCH_LIBRARY_EXCEPTIONS*/
}

void 
SwreC_TraceAppendCharPrefix(const Safir::Dob::Typesystem::Int64 prefixId,
                            const char ch,
                            bool & success)
{
    success = false;
    try
    { 
        std::string str;
        str.push_back(ch);
        Library::Instance().TraceString(prefixId, Safir::Dob::Typesystem::Utilities::ToWstring(str));
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS
}


void 
SwreC_TraceAppendWcharPrefix(const Safir::Dob::Typesystem::Int64 prefixId,
                             const wchar_t ch,
                             bool & success)
{
    success = false;
    try
    {
        Library::Instance().Trace(prefixId,ch);
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void SwreC_TraceSyncBuffer(bool & success)
{
    success = false;
    try
    {
        Library::Instance().TraceSync();
        success = true;
    }
    CATCH_LIBRARY_EXCEPTIONS;
}

void SwreC_TraceFlushBuffer(bool & success)
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







