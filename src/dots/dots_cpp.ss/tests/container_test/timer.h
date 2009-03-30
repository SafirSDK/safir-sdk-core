/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#include <boost/noncopyable.hpp>
#ifndef __TIMER_H__
#define __TIMER_H__
#include <iostream>
#ifdef _WIN32
#include <windows.h>
#endif
class Timer:
    private boost::noncopyable
{
public:
    const long m_Repeats;

    Timer(const std::string & name, const long repeats):
        m_Repeats(repeats),
        m_Name(name)
    {
        m_Name.resize(30,' ');
#ifdef _WIN32
        if (!QueryPerformanceCounter(&m_StartTime))
        {
            std::cout << "Timer does not work!!!"<<std::endl;
        }
        //m_StartTime = clock();
#else
        clock_gettime(CLOCK_REALTIME,&m_StartTime);
#endif
    }
  
    ~Timer()
    {
#ifdef _WIN32
        LARGE_INTEGER endTime;
        if (!QueryPerformanceCounter(&endTime))
        {
            std::cout << "Timer does not work!!!"<<std::endl;
        }
        LARGE_INTEGER frequency;
        if (!QueryPerformanceFrequency(&frequency))
        {
            std::cout << "Timer does not work!!!"<<std::endl;
        }

        const double floatTime = (endTime.QuadPart - m_StartTime.QuadPart)/(double)frequency.QuadPart;
#else

        timespec endTime;
        clock_gettime(CLOCK_REALTIME,&endTime);
        if (m_StartTime.tv_nsec > endTime.tv_nsec)
        {
            --endTime.tv_sec;
            endTime.tv_nsec += 1000000000;
        }
        endTime.tv_sec -= m_StartTime.tv_sec;
        endTime.tv_nsec -= m_StartTime.tv_nsec;
        const double floatTime = endTime.tv_sec + endTime.tv_nsec / 1e9;
#endif
        /*      std::cout << "Timer '" 
                << m_Name 
                << "' took "
                << floatTime << " seconds for "
                << m_Repeats << " repeats"<<std::endl;*/
            std::cout << "Timer '" 
                  << m_Name.c_str() 
                  << "' took "
                  <<floatTime/m_Repeats *1000000.0 << " us per repeat"<<std::endl;


        /*
        std::cout << "\"" << m_Name <<"\""
                  << ", "
                  << m_Repeats
                  << ","
                  <<floatTime<<std::endl;*/

        /*
        std::cout << "\"" << m_Name <<"\""
                  << "\t"
                  << m_Repeats
                  << "\t"
                  <<floatTime<<std::endl;*/

  
    }
  

    const long Repeats() const {return m_Repeats;}
protected:
    std::string m_Name;

#ifdef _WIN32
    LARGE_INTEGER m_StartTime;
#else
    timespec m_StartTime;
#endif
};

#endif
