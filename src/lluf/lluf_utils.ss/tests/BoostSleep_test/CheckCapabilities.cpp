/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <iostream>
#include <boost/thread.hpp>
#include <boost/timer.hpp>


int main()
{
    int errors = 0;

#ifndef BOOST_THREAD_USES_CHRONO
    std::wcout << "BOOST_THREAD_USES_CHRONO is not defined!" << std::endl;
    ++errors;
#else
    if (!boost::chrono::steady_clock::is_steady)
    {
        std::wcout << "steady_clock is not steady!!!" << std::endl;
        ++errors;
    }
#endif

#ifndef BOOST_CHRONO_HAS_CLOCK_STEADY
    std::wcout << "BOOST_CHRONO_HAS_CLOCK_STEADY is not defined!" << std::endl;
    ++errors;
#endif


#ifndef BOOST_THREAD_SLEEP_FOR_IS_STEADY
    std::wcout << "BOOST_THREAD_SLEEP_FOR_IS_STEADY is not defined!" << std::endl;
    ++errors;
#endif



    return errors;
}


