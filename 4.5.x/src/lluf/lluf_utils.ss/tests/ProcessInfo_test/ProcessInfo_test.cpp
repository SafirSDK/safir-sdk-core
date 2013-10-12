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
#include <Safir/Utilities/ProcessInfo.h>
#include <iostream>

int main()
{
    Safir::Utilities::ProcessInfo pi(Safir::Utilities::ProcessInfo::GetPid());
    if (pi.GetProcessName() == "ProcessInfo_test" || pi.GetProcessName() == "ProcessInfo_test.exe")
    {
        return 0;
    }
    else
    {
        std::wcout << "Not ok! GetProcessName returned '"
                   << pi.GetProcessName().c_str() << "'" << std::endl;
        return 1;
    }
}

