/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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

#ifndef __DOSE_TIME_REGISTRATION_H__
#define __DOSE_TIME_REGISTRATION_H__


#include <Safir/Dob/Internal/DistributionData.h>

#ifdef REGISTER_TIMES
#include <boost/noncopyable.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/lexical_cast.hpp>
#include <Safir/Utilities/ProcessInfo.h>


#if defined _WIN32
#  define USE_WINDOWS_PERFCOUNTER
#elif defined(linux) || defined(__linux) || defined(__linux__)
#  define USE_LINUX_CLOCK_REALTIME
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{

    namespace
    {
        const boost::posix_time::ptime _1_JAN_1970 (boost::gregorian::date(1970,boost::gregorian::Jan,1));
    }

    class TimeRegistration:
        private boost::noncopyable
    {
    public:
        explicit TimeRegistration(const std::string & name):
            m_file(GetLogFilePath(name)),
            m_numWrites(0)
        {
            m_file << "Id Time" << std::endl;
            m_file.setf(ios::fixed, ios::floatfield);
            m_file.setf(ios::showpoint);
            m_file.precision(40);

#ifdef USE_WINDOWS_PERFCOUNTER
            const BOOL success = QueryPerformanceFrequency(&m_frequency);
            ENSURE(success, << "Failed to QueryPerformanceFrequency");
#endif
        }

        void Register(const DistributionData & data)
        {
#if (defined USE_LINUX_CLOCK_REALTIME) //use linux native time
            timespec t;
            clock_gettime(CLOCK_REALTIME,&t);
            const double now = t.tv_sec + t.tv_nsec / 1.0e9;
#elif (defined USE_WINDOWS_PERFCOUNTER)
            LARGE_INTEGER t;
            QueryPerformanceCounter(&t);
            const double now = t.QuadPart/(double)m_frequency.QuadPart;
#else //use boost
            const boost::posix_time::time_duration d = boost::posix_time::microsec_clock::universal_time() - _1_JAN_1970;
            const double now = (double)d.ticks() / d.ticks_per_second();
#endif
            m_file << data.GetId() << " " << now << "\n";

            ++m_numWrites;
            if (m_numWrites == 10000)
            {
                m_file << std::flush;
                m_numWrites = 0;
            }
        }

    private:

        static boost::filesystem::path GetLogFilePath(const std::string & name)
        {
            const char * ENV_NAME = "SAFIR_RUNTIME";
            char * env = getenv(ENV_NAME);
            ENSURE(env != NULL, << "Environment variable " << ENV_NAME << " must be set!" << std::endl);

            boost::filesystem::path filename(env,boost::filesystem::native);

            filename /= "log";
            filename /= "Dob-TimeRegistration";
            boost::filesystem::create_directory(filename);
            filename /= boost::lexical_cast<std::string>(Safir::Utilities::ProcessInfo::GetPid()) + "_" + name + ".csv";

            return filename;
        }

        boost::filesystem::ofstream m_file;
        unsigned int m_numWrites;

#ifdef USE_WINDOWS_PERFCOUNTER
        LARGE_INTEGER m_frequency;
#endif
    };

}
}
}
#else


namespace Safir
{
namespace Dob
{
namespace Internal
{

    class TimeRegistration:
        private boost::noncopyable
    {
    public:
        explicit TimeRegistration(const std::string & ) {}

        void Register(const DistributionData & ){}
    };

}
}
}

#endif

#endif

