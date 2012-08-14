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

//Singleton class crash_handler that is responsible for initiating the crash_handler thread. 

//#pragma once
#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4267)
#endif

#include <ace/Thread_Mutex.h>
#include <ace/Thread.h>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif

#if defined _MSC_VER
    #include <Safir\Utilities\Breakpad\exception_handler.h>
#elif defined __GNUC__
    #include <Safir/Utilities/Breakpad/exception_handler.h>
#endif

//Callback functions for writing core dump
//Returning false will leave the crash as unhandled
#if defined _MSC_VER
static bool callback(const wchar_t *dump_path, const wchar_t *id,
                        void *context, EXCEPTION_POINTERS *exinfo,
                        MDRawAssertionInfo *assertion,
                        bool succeeded) {
    return false;
}
#elif defined __GNUC__
static bool callback(const char* dump_path,
                            const char* id,
                            void* context,
                            bool succeeded){   
    return false;
}
#endif

//Singleton class responsible for initiating the crash handler thread. 
class Crash_Handler
{
public:
    static void Instance();
private:
    Crash_Handler();
    ~Crash_Handler();
    
    //Singleton stuff
    static google_breakpad::ExceptionHandler * volatile m_handler;
    static ACE_Thread_Mutex m_instantiationLock;
};

 
