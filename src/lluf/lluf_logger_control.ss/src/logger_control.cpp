/******************************************************************************
*
* Copyright Saab Systems AB, 2007
*
******************************************************************************/
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
#include <iostream>
#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/mapped_region.hpp>

void PrintHelp()
{
    std::wcout<< "Program to turn on or off low level logging to %SAFIR_RUNTIME%/log/Dob-LowLevelLog" << std::endl;
    std::wcout<< "Logging can also be turned on by creating a file called 'logging_on' in that directory." << std::endl;
    std::wcout<< "Command line parameter must be 'on' or 'off'"<<std::endl;
    exit(1);
}

int main(int argc, char * argv[])
{
    using namespace std;
    if (argc != 2)
    {
        PrintHelp();
    }
    std::string command = argv[1];
    bool on = false;
    if (command == "on")
    {
        on = true;
    }
    else if (command == "off")
    {
        on = false;
    }
    else
    {
        PrintHelp();
    }

    try
    {
        boost::interprocess::shared_memory_object shm(boost::interprocess::open_only,"LLUF_LLL_SHM", boost::interprocess::read_write);
        boost::interprocess::mapped_region shmRegion(shm,boost::interprocess::read_write);
        *static_cast<bool*>(shmRegion.get_address()) = on;
        std::wcout << "Logging should now be " << (on?"on":"off") << std::endl;
    }
    catch (const std::exception &)
    {
        std::wcout << "Failed to turn logging " << (on?"on":"off") << ". Is any app using the logger running?" << std::endl;
        return 1;
    }

    return 0;
}
