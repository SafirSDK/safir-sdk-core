/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#include <Safir/Utilities/CrashReporter.h>
#include <iostream>

#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

void callback(const char* const dumpPath)
{
    std::wcout << "callback with dumpPath = '" << dumpPath << "'" << std::endl;
}

int main()
{
    Safir::Utilities::CrashReporter::RegisterCallback(callback);
    Safir::Utilities::CrashReporter::Start();
    boost::this_thread::sleep_for(boost::chrono::milliseconds(500)); //sleep a little while to let bg thread start.
    std::wcout << "Started" << std::endl;
    boost::this_thread::sleep_for(boost::chrono::seconds(60));
    Safir::Utilities::CrashReporter::Stop();
    return 0;
}


