/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include <boost/thread.hpp>
#include "Sender.h"
#include "Receiver.h"

int main(int argc, char* argv[])
{
    const int executionTime=argc>1 ? boost::lexical_cast<int>(argv[1]) : 30;
    srand(time(0));

    boost::asio::io_service ioService;
    auto work=boost::make_shared<boost::asio::io_service::work>(ioService);

    boost::thread_group threads;
    for (unsigned int i=0; i<10; ++i)
    {
        threads.create_thread([&]{ioService.run();});
    }

    //unicast nodeType
    //Sender su1(ioService, 1, 0);
    //Receiver ru3(ioService, 3, 0);
    //Receiver ru4(ioService, 4, 0);

    //multicast nodeType
    Sender sm2(ioService, 2, 1);
    Receiver rm5(ioService, 5, 1);
    Receiver rm6(ioService, 6, 1);


    //su1.Seed(2);
    sm2.Seed(1);
    //ru3.Seed(1);
    //ru4.Seed(1);
    rm5.Seed(2);
    rm6.Seed(2);

    //Shutdown after specified time
    boost::this_thread::sleep_for(boost::chrono::seconds(executionTime));
    std::cout<<"Stopping..."<<std::endl;
    work.reset();
    //su1.Stop();
    sm2.Stop();
    //ru3.Stop();
    //ru4.Stop();
    rm5.Stop();
    rm6.Stop();
    threads.join_all();

    std::cout<<"Stopped"<<std::endl;

    //Print summary
    //su1.Print();
    sm2.Print();
    //ru3.Print();
    //ru4.Print();
    rm5.Print();
    rm6.Print();

    return 0;
}
