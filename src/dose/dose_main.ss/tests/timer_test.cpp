/******************************************************************************
*
* Copyright Saab Systems AB, 2012 (http://www.safirsdk.com)
*
* Created by: Lars Hagström <lars@foldspace.nu>
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
#include "../src/dose_main_timers.h"

/* 
 * NOTE: This test is meant as a unit test for regression, not as a
 * performance test for the underlying OS timers. So the error margins
 * are set quite generously.
 */

namespace 
{
    boost::asio::io_service ioService;
}

using namespace Safir::Dob::Internal;

class TimerTesterBase
    : public TimeoutHandler
{
public:
    TimerTesterBase(const std::wstring& name)
        : m_ok(false)
        , m_constructionTime(GetUtcTime())
    {
        m_timerId = TimerHandler::Instance().RegisterTimeoutHandler(name, *this);
    }

    ~TimerTesterBase() 
    {
        if (!m_ok)
        {
            std::wostringstream ostr;
            ostr << TimerHandler::Instance().GetTimerName(m_timerId)
                 << " test failed!";
                
            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(), __WFILE__, __LINE__);
        }
        else
        {
            std::wcout << TimerHandler::Instance().GetTimerName(m_timerId)
                       << " test succeeded!" << std::endl;

        }
        
    }
protected:
    TimerId m_timerId;
    bool m_ok;
    const double m_constructionTime;
};


class SingleTimerTester
    : public TimerTesterBase
{
public:
    SingleTimerTester() : TimerTesterBase(L"Single Timer")
    {
        TimerInfoPtr timerInfo(new EmptyTimerInfo(m_timerId));
        TimerHandler::Instance().Set(Replace, timerInfo, GetUtcTime());
    }

    void HandleTimeout(const TimerInfoPtr& /*timer*/)
    {
        m_ok = true;
        ioService.stop();
    }
};

class ReplaceTimerTester
    : public TimerTesterBase
{
public:
    ReplaceTimerTester() : TimerTesterBase(L"Replace Timer")
    {
        TimerInfoPtr timerInfo(new EmptyTimerInfo(m_timerId));
        TimerHandler::Instance().Set(Replace, timerInfo, GetUtcTime() + 10);
        TimerHandler::Instance().Set(Replace, timerInfo, GetUtcTime() + 0.01); //replace the previous timer
    }

    void HandleTimeout(const TimerInfoPtr& /*timer*/)
    {
        if (GetUtcTime() - m_constructionTime < 1.0)
        {
            m_ok = true;
        }
        ioService.stop();
    }
};

class DiscardTimerTester
    : public TimerTesterBase
{
public:
    DiscardTimerTester() : TimerTesterBase(L"Discard Timer")
    {
        TimerInfoPtr timerInfo(new EmptyTimerInfo(m_timerId));
        TimerHandler::Instance().Set(Discard, timerInfo, GetUtcTime() + 0.01); 
        TimerHandler::Instance().Set(Discard, timerInfo, GetUtcTime() + 10); //this should be discarded

    }

    void HandleTimeout(const TimerInfoPtr& /*timer*/)
    {
        if (GetUtcTime() - m_constructionTime < 1.0)
        {
            m_ok = true;
        }
        ioService.stop();
    }
};


class DeadlineTimerTester
    : public TimerTesterBase
{
public:
    DeadlineTimerTester() : TimerTesterBase(L"Deadline Timer"),
                            m_timerInfo(new EmptyTimerInfo(m_timerId)),
                            m_repeats(1000),
                            m_count(m_repeats),
                            m_delay(0.01),
                            m_last(m_constructionTime)
    {
        TimerHandler::Instance().Set(Replace, m_timerInfo, m_last + m_delay); 
    }

    void HandleTimeout(const TimerInfoPtr& /*timer*/)
    {
        --m_count;

        TimerHandler::Instance().Set(Replace, m_timerInfo, m_constructionTime + (m_repeats - m_count) * m_delay); 

        const double now = GetUtcTime();

        if (fabs(now - m_last) > m_delay + 2) //2s error is ok for a single timer, remember we might be running
            //on oversubscribed virtual hw.
        {
            std::wcout << "Diff is " << now - m_last << std::endl;
            ioService.stop();
        }
        m_last = now;

        if (m_count == 0)
        {
            const double elapsed = now - m_constructionTime;
            const double error = fabs(elapsed/(m_repeats*m_delay) - 1) * 100;

            if (error < 15.0) //15% total error allowed
            {
                m_ok = true;
            }
            else
            {
                std::wcout << "Total error was " << error << std::endl;
            }
            ioService.stop();
        }
    }

private:
    TimerInfoPtr m_timerInfo;
    const int m_repeats;
    int m_count;
    const double m_delay;
    double m_last;
};


int main(int,char**)
{
    TimerHandler::Instantiate(ioService);

    try 
    {
        {
            SingleTimerTester tester;
            
            ioService.run();
            ioService.reset();
        }
            
        {
            ReplaceTimerTester tester;
            
            ioService.run();
            ioService.reset();
        }

        {
            DiscardTimerTester tester;
            
            ioService.run();
            ioService.reset();
        }

        {
            DeadlineTimerTester tester;
            
            ioService.run();
            ioService.reset();
        }

        //TODO: test Remove!

    }
    catch (const std::exception& e)
    {
        std::wcout << "Caught exception!\n" << e.what() << std::endl;
        return 1;
    }
    return 0;
}


