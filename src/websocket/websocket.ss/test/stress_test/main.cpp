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
#include <iostream>
#include <map>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4005)
#pragma warning(disable: 4100)
#pragma warning(disable: 4355)
#pragma warning(disable: 4127)
#pragma warning(disable: 4267)
#pragma warning(disable: 4996)
#endif

#include <boost/lexical_cast.hpp>
#include <boost/make_shared.hpp>
#include <boost/asio.hpp>
#include "ServiceHandler.h"
#include "ServiceUser.h"

#ifdef _MSC_VER
#pragma warning(pop)
#endif

class Test
{
public:
    Test(int numUsers, int numSimultanousUsers, int requestsPerUser)
        :m_ioService()
        ,m_work(new boost::asio::io_service::work(m_ioService))
        ,m_handler([=]{HandlerSetupReady();})
        ,m_users()
        ,m_numUsers(numUsers)
        ,m_numSimultanousUsers(numSimultanousUsers)
        ,m_requestsPerUser(requestsPerUser)
        ,m_totalNumRequests(0)
        ,m_userCount(0)
    {
    }

    void Run()
    {
        m_ioService.post([=]{m_handler.Run();});
        m_ioService.run();
    }

private:
    typedef boost::shared_ptr<ServiceUser> ServiceUserPtr;
    boost::asio::io_service m_ioService;
    boost::shared_ptr<boost::asio::io_service::work> m_work;
    ServiceHandler m_handler;
    std::map<int, ServiceUserPtr> m_users;

    int m_numUsers;
    int m_numSimultanousUsers;
    int m_requestsPerUser;

    boost::int64_t m_totalNumRequests;
    int m_userCount;

    void HandlerSetupReady()
    {
        m_ioService.post([=]
        {
            while (m_userCount<m_numSimultanousUsers)
            {
                StartUser();
            }
        });
    }

    void UserDone(int id, bool fail)
    {
        m_ioService.post([=]
        {
            if (fail)
            {
                m_users[id]=boost::make_shared<ServiceUser>(id, m_requestsPerUser, [=](int id, bool fail){UserDone(id, fail);});
                m_users[id]->Run();
                return;
            }

            m_totalNumRequests+=m_requestsPerUser;
            m_users.erase(id);
            if (m_userCount<m_numUsers)
            {
                StartUser();
            }
            else if (m_users.empty())
            {
                //all users are finished, close down
                std::cout<<"All users are finished"<<std::endl;
                m_handler.Stop();
                m_work.reset();
            }
            else
            {
                std::cout<<"****** Running Users *********"<<std::endl;
                for (auto it=m_users.begin(); it!=m_users.end(); ++it)
                {
                    std::cout<<"USER_"<<it->first<<std::endl;
                }
                std::cout<<"*******************************"<<std::endl;
            }
        });
    }

    void StartUser()
    {
        ++m_userCount;
        if (m_userCount%100==0)
        {
            std::cout<<"TestManager: Start user number "<<m_userCount<<std::endl;
        }
         m_users[m_userCount]=boost::make_shared<ServiceUser>(m_userCount, m_requestsPerUser, [=](int id, bool fail){UserDone(id, fail);});
         m_users[m_userCount]->Run();
    }

};

int main(int argc, const char** argv)
{
    if (argc!=4)
    {
        std::cout<<"Usage:  safir_websocket_stresstest <total_num_users> <simultaneous_users> <requests_per_user>"<<std::endl;
        return 0;
    }

    int totalNumUsers=boost::lexical_cast<int>(argv[1]);
    int simultaneousUsers=boost::lexical_cast<int>(argv[2]);
    int reqPerUser=boost::lexical_cast<int>(argv[3]);

    Test test(totalNumUsers, simultaneousUsers, reqPerUser);
    test.Run();

    boost::this_thread::sleep_for(boost::chrono::seconds(2)); //let io_service stop

    std::cout<<"Test passed!"<<std::endl;

    return 0;
}
