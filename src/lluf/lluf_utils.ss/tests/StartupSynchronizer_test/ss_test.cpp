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
#include <Safir/Utilities/StartupSynchronizer.h>
#include <ace/OS_NS_unistd.h>

class Synchronized:
    public Safir::Utilities::Synchronized
{
    virtual void Create()
    {
        std::wcout << "-- Create" << std::endl;
        ACE_OS::sleep(5);
        std::wcout << "-- Create complete" << std::endl;
    }
    virtual void Use()
    {
        std::wcout << "-- Use" << std::endl;
        ACE_OS::sleep(5);
        std::wcout << "-- Use complete" << std::endl;
    }

    virtual void Destroy()
    {
        std::wcout << "-- Destroy" << std::endl;
        ACE_OS::sleep(5);
        std::wcout << "-- Destroy complete" << std::endl;
    }
};

int main()
{
    Synchronized synched;

    Safir::Utilities::StartupSynchronizer ss("StartupSynchronizer_test",&synched);
    ss.Start();
}


