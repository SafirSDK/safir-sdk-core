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
#include <ace/Guard_T.h>

 
google_breakpad::ExceptionHandler * volatile Crash_Handler::m_handler=NULL;
ACE_Thread_Mutex Crash_Handler::m_instantiationLock;

void Crash_Handler::Instance()
{
    if (m_handler == NULL)
    {
        ACE_Guard<ACE_Thread_Mutex> lck(m_instantiationLock);
        if (m_handler == NULL)
        {
#if defined _MSC_VER
            char * env = getenv("SAFIR_RUNTIME");
            std::wstring dump_path;
            if (env != NULL)
            {
                dump_path = Safir::Dob::Typesystem::Utilities::ToWstring(strcat(env,"/dump/reports"));
                 
            }
            else
                dump_path=L".";
 
            m_handler=new google_breakpad::ExceptionHandler( dump_path,
                                                        NULL,
                                                        callback,
                                                        NULL,
                                                        google_breakpad::ExceptionHandler::HANDLER_ALL);
#elif defined __GNUC__
           char *dump_path=getenv("SAFIR_RUNTIME");
           if(dump_path==NULL)
               dump_path=".";
           m_handler=new google_breakpad::ExceptionHandler(strcat(dump_path,"/dump/reports"), NULL, callback, NULL, true);
#endif
        }
    }
}

Crash_Handler::Crash_Handler(void)
{
}

Crash_Handler::~Crash_Handler(void)
{    
}
