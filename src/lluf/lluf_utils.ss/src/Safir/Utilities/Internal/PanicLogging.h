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
#ifndef __LLUF_PANIC_LOGGING_H__
#define __LLUF_PANIC_LOGGING_H__

#include <Safir/Utilities/Internal/UtilsExportDefs.h>
#include <string>

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    class LLUF_UTILS_API PanicLogging
    {
    public:
        /** Note that the only extra information that is certain to be 
         * included in the log is the PID of the calling process.
         * Try to add as much information as possible to identify where
         * the log came from!
         */
        static void Log(const std::string& text);
    private:
        PanicLogging(); //not instantiable
    };
}
}
}

#endif

