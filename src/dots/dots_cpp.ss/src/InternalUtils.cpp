/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/CrashReporter.h>
#include <iostream>


namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    void EnsureFailed (const std::wstring & str)
    {
        Safir::Utilities::Internal::SystemLog().Send(Safir::Utilities::Internal::SystemLog::Critical,
                                                     L"ENSURE failed: " + str);

        const bool success = Safir::Utilities::CrashReporter::Dump();
        
        if (!success)
        {
            Safir::Utilities::Internal::SystemLog().Send(Safir::Utilities::Internal::SystemLog::Critical,
                                                         L"ENSURE failed to generate a dump! It looks like CrashReporter is not started.");
        }

        throw SoftwareViolationException(str, __WFILE__,__LINE__);
    }
}
}
}
}
