/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagstrom
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
#include <Safir/Application/Tracer.h>
#include <Safir/Application/TracerParameters.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <iostream>
#include <string>

int main(int argc, char** argv)
{
    std::string mode;
    if (argc == 2)
    {
        mode = argv[1];
    }

    Safir::Application::Tracer tracer(L"TestPrefix");

    if (mode == "enable" || mode == "midline" || mode == "tracer_facility" || mode == "overflow")
    {
        tracer.Enable(true);
    }

    if (mode == "midline")
    {
        // Test mid-line syslog insertion
        // Write partial line (no endl)
        tracer << L"partial";

        // Send syslog while mid-line - should insert newline first
        Safir::Utilities::Internal::Log::Send(
            Safir::Utilities::Internal::Log::Error,
            Safir::Utilities::Internal::Log::Local0,
            L"midline error");

        // Continue with another line
        tracer << L"after midline" << std::endl;

        // Flush to ensure output is written
        tracer << std::flush;
    }
    else if (mode == "tracer_facility")
    {
        // Test that messages using the tracer's own facility are NOT forwarded
        // (they would be redundant since they're already tracer output)
        tracer << L"before tracer_facility" << std::endl;

        // Send syslog using the tracer's own facility - should NOT appear in output
        const auto tracerFacility = static_cast<Safir::Utilities::Internal::Log::Facility>(
            Safir::Application::TracerParameters::SyslogFacility());
        Safir::Utilities::Internal::Log::Send(
            Safir::Utilities::Internal::Log::Error,
            tracerFacility,
            L"THIS SHOULD NOT APPEAR");

        // Send syslog using a different facility - should appear
        Safir::Utilities::Internal::Log::Send(
            Safir::Utilities::Internal::Log::Error,
            Safir::Utilities::Internal::Log::Local0,
            L"this should appear");

        tracer << L"after tracer_facility" << std::endl;
        tracer << std::flush;
    }
    else if (mode == "overflow")
    {
        // Test that syslog sent while holding the lock doesn't deadlock
        // Write enough data to trigger the "not flushing" error (>50000 chars)
        tracer << L"start" << std::endl;
        for (int i = 0; i < 5000; ++i)
        {
            tracer << L"1234567890\n";  // 11 chars per line, 55000 total
        }
        // If we get here without deadlock, the try_lock worked
        tracer << L"end" << std::endl;
        tracer << std::flush;
    }
    else
    {
        // Standard test mode ("enable" or no args)

        // Write some tracer output first
        tracer << L"before syslog" << std::endl;

        // Send syslog messages with different severities
        Safir::Utilities::Internal::Log::Send(
            Safir::Utilities::Internal::Log::Error,
            Safir::Utilities::Internal::Log::Local0,
            L"This is an error message");

        Safir::Utilities::Internal::Log::Send(
            Safir::Utilities::Internal::Log::Warning,
            Safir::Utilities::Internal::Log::Local0,
            L"This is a warning message");

        Safir::Utilities::Internal::Log::Send(
            Safir::Utilities::Internal::Log::Critical,
            Safir::Utilities::Internal::Log::Local0,
            L"This is a critical message");

        // Write some more tracer output after
        tracer << L"after syslog" << std::endl;

        // Flush to ensure output is written
        tracer << std::flush;
    }

    return 0;
}
