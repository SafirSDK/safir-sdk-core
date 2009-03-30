/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Erik Adolfsson / sterad
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

#include "DobHandler.h"
#include <iostream>

int main(int argc, char* argv[])
{
    int choice = -1;
    DobHandler* dobHandler;

    std::cout<<std::endl;
    std::cout<<" 1: Time Subscriber\n";
    std::cout<<" 2: Time Owner\n";
    std::cout<<" Make your choice: ";
    std::cin>>choice;
    std::cout<<std::endl;
    switch(choice)
    {
    case 1:
        dobHandler = new SubscriptionHandler();
        break;
    case 2:
        dobHandler = new RequestHandler();
        break;
    default:
        dobHandler = NULL;
        break;
    }

    if (dobHandler != NULL)
        dobHandler->Start();

    delete dobHandler;
    return 0;
}

