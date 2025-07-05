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
        :m_work(boost::asio::make_work_guard(m_ioContext))
        ,m_strand(m_ioContext)
        ,m_pingHandler(new PingHandler(m_strand, interval, [this]{OnPing();}))
    {
        boost::asio::dispatch(m_ioContext,[this]{m_pingHandler->Start();});
        m_pingTime=std::chrono::steady_clock::now();
        m_ioContext.run();
    }

private:
    static const int interval=1;
    boost::asio::io_context m_ioContext;
    boost::asio::executor_work_guard<boost::asio::io_context::executor_type> m_work;
    boost::asio::io_context::strand m_strand;
    std::chrono::steady_clock::time_point m_pingTime;
    std::shared_ptr<PingHandler> m_pingHandler;

    void OnPing()
    {
        static int count=0;

        std::chrono::duration<double> elapsed=std::chrono::steady_clock::now()-m_pingTime;
        //std::cout<<"ping "<<elapsed.count()<<std::endl;
        if (elapsed>std::chrono::seconds(interval+1))
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

        m_pingTime=std::chrono::steady_clock::now();
    }

};
