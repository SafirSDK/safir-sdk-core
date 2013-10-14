/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
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
#include <Safir/Dob/Internal/Semaphore.h>
#include "../common/StatisticsCollection.h"


class App
{
public:
    App(const std::string& name1, const std::string& name2): m_mySem(name1),m_otherSem(name2),
        m_sendStat(StatisticsCollection::Instance().AddHzCollector(L"Signal"))
    {
        m_otherSem.post();
    }

    void App::Run()
    {
        int counter = 0;
        for (;;)
        {
            m_mySem.wait();
            m_otherSem.post();
            m_sendStat->Tick();
        }
    }

private:
    Safir::Dob::Internal::NamedSemaphore m_mySem;
    Safir::Dob::Internal::NamedSemaphore m_otherSem;
    HzCollector *m_sendStat;

};

int main(int argc, char* argv[])
{
    if (argc != 3)
    {
        std::wcout << "enter 2 arguments. <wait_semaphore> <post_semaphore>" << std::endl; 
        return -1;
    }

    std::string s1 = "Test_Named_Sem_";
    std::string s2 = "Test_Named_Sem_";
    s1.append(argv[1]);
    s2.append(argv[2]);
    App app(s1, s2);
    app.Run();

    return 0;
}

