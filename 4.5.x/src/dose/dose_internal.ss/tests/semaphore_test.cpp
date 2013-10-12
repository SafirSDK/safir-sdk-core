/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström <lars.hagstrom@consoden.se>
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
#include <Safir/Dob/Internal/Semaphore.h>
#include <iostream>

using namespace Safir::Dob::Internal;

int main(int, char**)
{
    NamedSemaphore sem("TEST_SEMAPHORE");
    if (sem.try_wait()) 
    {
        std::wcout << "try_wait on semaphore with count 0 succeeded, erroneously" << std::endl;
        return 1;
    }

    sem.post();

    if (!sem.try_wait()) 
    {
        std::wcout << "failed to wait on semaphore with count 1" << std::endl;
        return 1;
    }

    sem.post();
    sem.wait();


    //open the same semaphore again
    NamedSemaphore sem2("TEST_SEMAPHORE");
    sem2.post();

    if (!sem.try_wait()) 
    {
        std::wcout << "failed to wait on semaphore with count 1, take 2" << std::endl;
        return 1;
    }
    
    if (sem2.try_wait()) 
    {
        std::wcout << "try_wait on semaphore with count 0 succeeded, erroneously, take 2" << std::endl;
        return 1;
    }

    sem.post();
    if (!sem2.try_wait()) 
    {
        std::wcout << "failed to wait on semaphore with count 1, take 3" << std::endl;
        return 1;
    }

    std::wcout << "success" << std::endl;
    return 0;
}
