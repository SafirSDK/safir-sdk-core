/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
#include "../../src/include/Safir/Utilities/Internal/AsioPeriodicTimer.h"
#include <iostream>

//Note: This test does not actually check that the timer is at all correct in timing.
//All it does is check that it will repeat cyclically. Timing is difficult to test
//in unit test, since it may fail if there is a lot of load on the computer.
int main()
{
    try
    {
        boost::asio::io_service ioService;

        int countdown = 10;

        std::function<void()> stopCb;

        Safir::Utilities::Internal::AsioPeriodicTimer timer(ioService,
                                                            boost::chrono::milliseconds(10),
                                                            [&](const boost::system::error_code& error)
                                                            {
                                                                std::wcout << "Got callback" << std::endl;
                                                                if (!error)
                                                                {
                                                                    --countdown;
                                                                    if (countdown == 0)
                                                                    {
                                                                        stopCb();
                                                                    }
                                                                }
                                                                else
                                                                {
                                                                    std::wcout << "Asio error " << error << std::endl;
                                                                    exit(2);
                                                                }
                                                            });

        stopCb = [&timer]{timer.Stop();};
        timer.Start();
        ioService.run();

        if (countdown == 0)
        {
            std::wcout << "Success!" << std::endl;
            return 0;
        }
        else
        {
            std::wcout << "Failure" << std::endl;
            return 1;
        }
    }
    catch (const std::exception& e)
    {
        std::wcout << "caught exception " << e.what() << std::endl;
        return 1;
    }
}
