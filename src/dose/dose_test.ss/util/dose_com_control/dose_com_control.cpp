/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
*
* Created by: Mikael Wennerberg / stmiwn
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
#include <vector>
#include "dose_com_utils.h"

void Usage()
{
    std::wcout << "Turn outbound communication on/off. Inbound communication is not affected. "   <<  std::endl;
    std::wcout << "Usage: dose_com_control <command> "   << std::endl;
    std::wcout << "commands:"   << std::endl;
    std::wcout << "  off : Turn communication OFF."   << std::endl;
    std::wcout << "  on  : Turn Communication ON."   << std::endl;
    std::wcout << "  s   : Show communication status."   << std::endl;
}

int main(int argc, char* argv[])
{
    const std::vector<std::string> arguments(&argv[0],&argv[argc]);

    if (arguments.size() != 2)
    {
        Usage();
    }   
    else
    {
        DOSE_SHARED_DATA_S * pShm = (DOSE_SHARED_DATA_S *) Get_NodeSharedData_Pointer();  

        if (pShm->MyIpAddr_nw == 0)
        {
            std::wcout << "No dose_main running!"  << std::endl;
        }
        else
        {
            if (arguments[1].compare("off") == 0)
            {
                pShm->InhibitOutgoingTraffic = 1;
                std::wcout << "Communication is OFF."  << std::endl;
            }
            else if (arguments[1].compare("on") == 0)
            {            
                pShm->InhibitOutgoingTraffic = 0;
                std::wcout << "Communication is ON."  << std::endl;
            }
            else if (arguments[1].compare("s") == 0)
            {
                if (pShm->InhibitOutgoingTraffic)
                {
                    std::wcout << "Communication is OFF."  << std::endl;
                }
                else
                {
                    std::wcout << "Communication is ON."  << std::endl;
                }
            }
            else
            {
                Usage();
            }
        }
    }

    return 0;
}

