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
#include <boost/thread/once.hpp>

//fwd declaration
namespace google_breakpad 
{
    class ExceptionHandler;
};

/** Singleton class responsible for initiating the crash handler thread. */
class CrashHandler
{
public:
    static CrashHandler& Instance();
private:
    CrashHandler();
    ~CrashHandler();

    google_breakpad::ExceptionHandler * m_handler;

    /**
     * This class is here to ensure that only the Instance method can get at the 
     * instance, so as to be sure that boost call_once is used correctly.
     * Also makes it easier to grep for singletons in the code, if all 
     * singletons use the same construction and helper-name.
     */
    struct SingletonHelper
    {
    private:
        friend CrashHandler& CrashHandler::Instance();
        
        static CrashHandler& Instance();
        static boost::once_flag m_onceFlag;
    };
};

 
