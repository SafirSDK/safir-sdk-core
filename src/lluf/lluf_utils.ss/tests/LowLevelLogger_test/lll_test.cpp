/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#include <Safir/Utilities/Internal/LowLevelLogger.h>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4244)
#  pragma warning (disable : 4100)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

#include <iostream>
int main()
{
    for(int i = 0; i < 10000000; ++i)
    {
        lllog(5) << "Hello, World!"<<std::endl;
        lllog(9) << "Goodbye cruel world!"<<std::endl;
        lllog(1) << 1234567890 << std::endl;
        boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
        std::wcout << "Logging at "
                   << Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().LogLevel() << std::endl;
    }

    return 0;
}

