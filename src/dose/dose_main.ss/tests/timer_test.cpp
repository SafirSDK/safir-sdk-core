/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
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
        : m_timerHandler(ioService)
        , m_ok(false)
        , m_constructionTime(boost::chrono::steady_clock::now())
    {
        m_timerId = m_timerHandler.RegisterTimeoutHandler(name, *this);
    }

    ~TimerTesterBase()
    {
        if (!m_ok)
        {
            std::wostringstream ostr;
            ostr << m_timerHandler.GetTimerName(m_timerId)
                 << " test failed!";

            throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(), __WFILE__, __LINE__);
        }
        else
        {
            std::wcout << m_timerHandler.GetTimerName(m_timerId)
                       << " test succeeded!" << std::endl;
        }
    }

protected:
    TimerHandler m_timerHandler;
    TimerId m_timerId;
    bool m_ok;
    const boost::chrono::steady_clock::time_point m_constructionTime;
};


class SingleTimerTester
    : public TimerTesterBase
{
public:
    SingleTimerTester(const std::wstring& name, const double delay) : TimerTesterBase(name)
    {
        TimerInfoPtr timerInfo(new EmptyTimerInfo(m_timerId));
        m_timerHandler.SetRelative(Replace, timerInfo, delay);
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
        m_timerHandler.SetRelative(Replace, timerInfo, 10000);
        m_timerHandler.SetRelative(Replace, timerInfo, 0.01); //replace the previous timer
    }

    void HandleTimeout(const TimerInfoPtr& /*timer*/)
    {
        if (boost::chrono::steady_clock::now() - m_constructionTime < boost::chrono::seconds(100))
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
        m_timerHandler.SetRelative(Discard, timerInfo, 0.01);
        m_timerHandler.SetRelative(Discard, timerInfo, 10); //this should be discarded

    }

    void HandleTimeout(const TimerInfoPtr& /*timer*/)
    {
        if (boost::chrono::steady_clock::now() - m_constructionTime < boost::chrono::seconds(1))
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
                            m_delayMillis(10),
                            m_delay(m_delayMillis)
    {
        m_timerHandler.Set(Replace, m_timerInfo, m_constructionTime + m_delay);
    }

    void HandleTimeout(const TimerInfoPtr& /*timer*/)
    {
        --m_count;

        m_timerHandler.Set(Replace,
                                     m_timerInfo,
                                     m_constructionTime + (m_repeats - m_count) * m_delay);

        const boost::chrono::steady_clock::time_point now = boost::chrono::steady_clock::now();

        if (m_count == 0)
        {
            const double elapsed = (now - m_constructionTime).count() / 1.0e9;
            const double error = fabs(elapsed/(m_repeats*m_delayMillis/1000.0) - 1) * 100;

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
    const int m_delayMillis;
    const boost::chrono::milliseconds m_delay;
};


class TimerStarver
    : public TimerTesterBase
{
public:
    TimerStarver()
        : TimerTesterBase(L"Starver Timer")
        , m_timeoutCount(0)
    {
        m_timerInfo.reset(new EmptyTimerInfo(m_timerId));
        m_timerHandler.SetRelative(Discard, m_timerInfo, 0.0001);
        m_ok = true;
    }

    void HandleTimeout(const TimerInfoPtr& /*timer*/)
    {
        ++m_timeoutCount;
        const boost::chrono::steady_clock::time_point now = boost::chrono::steady_clock::now();
        boost::this_thread::sleep_for(boost::chrono::milliseconds(20));
        m_timerHandler.Set(Discard, m_timerInfo, now + boost::chrono::milliseconds(10));
    }

    ~TimerStarver()
    {
        std::wcout << "TimerStarver timed out " << m_timeoutCount << " times" <<std::endl;
        m_timerHandler.Remove(m_timerInfo);
    }
private:
    long m_timeoutCount;
    TimerInfoPtr m_timerInfo;
};

int main(int,char**)
{
    try
    {
        {
            SingleTimerTester tester(L"Starved Timer",5.0);
            TimerStarver starver;

            ioService.run();
            ioService.reset();
        }

        {
            SingleTimerTester tester(L"Single Timer",0.1);

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

    }
    catch (const std::exception& e)
    {
        std::wcout << "Caught exception!\n" << e.what() << std::endl;
        return 1;
    }
    return 0;
}
