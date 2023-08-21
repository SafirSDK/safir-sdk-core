/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com/)
*
* Created by: Anders Widén / anders.widen@consoden.se
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

#include "StatusApp.h"
#include <Safir/Dob/LowMemoryException.h>
#include <Safir/Utilities/CrashReporter.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>

int main(int /*argc*/, char* [] /*argv*/)
{

    //ensure call to CrashReporter::Stop at application exit
    boost::shared_ptr<void> crGuard(static_cast<void*>(0),
                                    [](void*){Safir::Utilities::CrashReporter::Stop();});

    std::atomic<bool> success(true);
    try
    {
        Safir::Utilities::CrashReporter::Start();

        Safir::Control::StatusApp app;
        app.Run();

        crGuard.reset();
    }
    catch(const Safir::Dob::LowMemoryException&)
    {
        SEND_SYSTEM_LOG(Critical,
                        << "Failed to start safir_status due to low Dob shared memory.");

        success.exchange(false);
    }
    catch(const std::exception & e)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "Caught std::exception! Contents of exception is: "
                        << "  '" << e.what() << "'.");

        success.exchange(false);
    }
    catch (...)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "Caught ... exception!");

        success.exchange(false);
    }

    return success ? 0 : 1;
}
