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
#include <boost/timer/timer.hpp>

void Timeout()
{
    boost::timer::cpu_timer stopwatch;
    try
    {
        boost::this_thread::sleep(boost::posix_time::minutes(10)); 
        
        std::wcout << "Timeout!" << std::endl;
        std::wcout << "Elapsed time: " << boost::timer::format(stopwatch.elapsed()).c_str() << "." << std::endl;
        exit(31);
    }
    catch (const boost::thread_interrupted&)
    {
        //ok, read was successful, we just let the thread go away.
    }
}


void run_test()
{

    std::wcout << "first test" << std::endl;
    for(int i = 0; i < 10000; ++i)
    {
        std::vector<boost::shared_ptr<boost::thread> > threads;

        for(int j = 0; j < 50; ++j)
        {
            threads.push_back(boost::shared_ptr<boost::thread>(new boost::thread(Timeout)));

            //interrupt some of the threads immediately
            if (j%4 == 0)
            {
                threads.back()->interrupt();
            }
        }

        for (std::vector<boost::shared_ptr<boost::thread> >::iterator it = threads.begin(); it != threads.end(); ++it)
        {
            (*it)->interrupt();
        }

        while (!threads.empty())
        {
            threads.back()->join();
            threads.pop_back();
        }
    }
    
    std::wcout << "second test" << std::endl;
    double sleepTime = 0;
    double maxSleepTime = 0;
    for(int i = 0; i < 10000; ++i)
    {
        boost::timer::cpu_timer stopwatch;

        boost::thread thread(Timeout);
        thread.interrupt();
        thread.join();
        const double elapsed = stopwatch.elapsed().wall / 1000000000.0;
        sleepTime += elapsed;
        maxSleepTime = std::max(maxSleepTime, elapsed);

        if (elapsed > 30) //more than 30 seconds
        {
            std::wcout << "Interrupt was too slow! elapsed: " << boost::timer::format(stopwatch.elapsed()).c_str() << "." << std::endl;
            exit(14);
        }
    }

    std::wcout << "Average interrupt time is " << sleepTime/10000*1000 << " milliseconds" << std::endl;
    std::wcout << "Maximum interrupt time is " << maxSleepTime*1000 << " milliseconds" << std::endl;
}

int main()
{
    try
    {
        run_test();
    }
    catch(const std::exception& e)
    {
        std::wcout << "Caught exception: " << e.what() << std::endl;
        return 1;
    }
    catch (...)
    {
        std::wcout << "Caught ... exception" << std::endl;
        return 1;
    }
    return 0;
}
