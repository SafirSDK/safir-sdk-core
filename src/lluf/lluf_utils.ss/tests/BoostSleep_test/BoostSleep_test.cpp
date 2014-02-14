/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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
#include <boost/thread.hpp>
#include <boost/timer.hpp>

void Timeout()
{
    boost::timer stopwatch;
    try
    {
        boost::this_thread::sleep(boost::posix_time::seconds(60)*10); //ten minutes
        
        std::wcout << "Timeout!" << std::endl;
        std::wcout << "Elapsed time " << stopwatch.elapsed() << " seconds." << std::endl;
        exit(31);
    }
    catch (const boost::thread_interrupted&)
    {
        //ok, read was successful, we just let the thread go away.
    }
}


int main()
{

    std::wcout << "first test" << std::endl;
    for(int i = 0; i < 10000; ++i)
    {
        std::vector<boost::shared_ptr<boost::thread> > threads;

        for(int j = 0; j < 50; ++j)
        {
            threads.push_back(boost::shared_ptr<boost::thread>(new boost::thread(Timeout)));
        }

        while (!threads.empty())
        {
            threads.back()->interrupt();
            threads.back()->join();
            threads.pop_back();
        }
    }

    std::wcout << "second test" << std::endl;
    for(int i = 0; i < 10000; ++i)
    {
        boost::timer stopwatch;

        boost::thread thread(Timeout);
        thread.interrupt();
        thread.join();
        if (stopwatch.elapsed() > 60) //more than 1 minute!
        {
            std::wcout << "Interrupt was too slow! elapsed = " << stopwatch.elapsed() << std::endl;
            exit(14);
        }
    }

    return 0;
}
