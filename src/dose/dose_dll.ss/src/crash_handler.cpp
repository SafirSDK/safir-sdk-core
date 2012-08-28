/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Amin Allalou 
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

#include "crash_handler.h"
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Utilities/Breakpad/exception_handler.h>
#include <boost/bind.hpp>

namespace
{
    //Callback functions for writing core dump
    //Returning false will leave the crash as unhandled
#if defined _MSC_VER
    static bool callback(const wchar_t *dumpPath, 
                         const wchar_t *id,
                         void *context, 
                         EXCEPTION_POINTERS *exinfo,
                         MDRawAssertionInfo *assertion,
                         bool succeeded) 
    {
        return false;
    }
#elif defined __GNUC__
    static bool callback(const char* dumpPath,
                         const char* id,
                         void* context,
                         bool succeeded)
    {
        return false;
    }
#endif


    const std::string GetDumpPath()
    {
        const char * const env = getenv("SAFIR_RUNTIME");
        return std::string(env) + "/dump/reports";
    }
}

boost::once_flag CrashHandler::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;

CrashHandler & CrashHandler::SingletonHelper::Instance()
{
    static CrashHandler instance;
    return instance;
}

CrashHandler & CrashHandler::Instance()
{
    boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
    return SingletonHelper::Instance();
}


CrashHandler::CrashHandler()
{
#if defined _MSC_VER
    const std::wstring dumpPath = Safir::Dob::Typesystem::Utilities::ToWstring(GetDumpPath());
    m_handler = new google_breakpad::ExceptionHandler(dumpPath,
                                                      NULL,
                                                      callback,
                                                      NULL,
                                                      google_breakpad::ExceptionHandler::HANDLER_ALL);
#elif defined __GNUC__
    const std::string dumpPath = GetDumpPath();
    m_handler=new google_breakpad::ExceptionHandler(dumpPath, 
                                                    NULL, 
                                                    callback, 
                                                    NULL, 
                                                    true);
#else
    #error No google breakpad for this platform!
#endif
}


CrashHandler::~CrashHandler()
{    
    //TODO: how to stop!
}
