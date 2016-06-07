/******************************************************************************
*
* Copyright Saab AB, 2016 (http://safirsdkcore.com)
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
#include "../../src/PingHandler.h"

#ifdef _MSC_VER
#pragma warning(disable:4355)
#endif


class PingHandlerTest
{
public:
    PingHandlerTest()
        :m_work(new boost::asio::io_service::work(m_ioService))
        ,m_strand(m_ioService)
        ,m_pingHandler(new PingHandler(m_strand, interval, [=]{OnPing();}))
    {
        m_ioService.dispatch([=]{m_pingHandler->Start();});
        m_pingTime=boost::chrono::steady_clock::now();
        m_ioService.run();
    }

private:
    static const int interval=1;
    boost::asio::io_service m_ioService;
    boost::shared_ptr<boost::asio::io_service::work> m_work;
    boost::asio::strand m_strand;
    boost::chrono::steady_clock::time_point m_pingTime;
    boost::shared_ptr<PingHandler> m_pingHandler;

    void OnPing()
    {
        static int count=0;

        boost::chrono::duration<double> elapsed=boost::chrono::steady_clock::now()-m_pingTime;
        //std::cout<<"ping "<<elapsed.count()<<std::endl;
        if (elapsed>boost::chrono::seconds(interval+1))
        {
            std::cout<<"took to long between pings"<<std::endl;
            exit(1);
        }

        if (++count==5)
        {
            m_pingHandler->Stop();
            m_pingHandler.reset();
            m_work.reset();

        }

        m_pingTime=boost::chrono::steady_clock::now();
    }

};
