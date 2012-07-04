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
#include <Safir/Utilities/Internal/LowLevelLoggerThreaded.h>

#include <iostream>
#include "common.h"

int main()
{
    Timer blocked;
    Timer all;
    {
        ScopeTimer tall(all);
        for (unsigned long i = 0; i < NUM_LOGS; ++i)
        {
            {
                ScopeTimer ts(blocked);
                lllout << "blahonga blahonga blahonga!!!" << std::endl;
            }
            work();
        }
    }

    
    std::wcerr << "Blocked time " << blocked.elapsed() << std::endl;
    std::wcerr << "All time " << all.elapsed() << std::endl;
    std::wcerr << "Percent " << blocked.elapsed() / all.elapsed() *100 << std::endl;
    return 0;
}
