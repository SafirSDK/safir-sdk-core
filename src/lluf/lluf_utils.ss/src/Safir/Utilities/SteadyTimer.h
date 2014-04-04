//
// This file provides a steady_timer for boost version < 1.49.
// It is almost completely based on the steady_deadline_timer from
// Marat Abrarov's asio-samples project. So the copyright is as follows:
//
// Copyright (c) 2010-2011 Marat Abrarov (abrarov@mail.ru)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#ifndef __SAFIR_UTILITIES_STEADY_TIMER_H__
#define __SAFIR_UTILITIES_STEADY_TIMER_H__

#if ((BOOST_VERSION / 100000) >= 1 && (BOOST_VERSION / 100 % 1000) >= 49)

#  include <boost/asio/steady_timer.hpp>
#  include <boost/chrono.hpp>

#else

#  include <boost/asio.hpp>
#  include <boost/date_time/posix_time/ptime.hpp>
#  include <boost/chrono.hpp> 

namespace boost
{
namespace asio
{

    struct steady_time_traits
    {
        typedef boost::chrono::steady_clock   steady_clock_type;
        typedef steady_clock_type::duration   duration_type;
        typedef steady_clock_type::time_point time_type;

        static time_type add(const time_type& time, const duration_type& duration)
        {
            return time + duration;
        }
  
        static bool less_than(const time_type& time1, const time_type & time2)
        {
            return time1 < time2;
        }

        static time_type now()
        {
            return steady_clock_type::now();
        }

        static duration_type subtract(const time_type& time1, const time_type& time2)
        {
            return time1 - time2;
        }
    
        static boost::posix_time::time_duration to_posix_duration(const duration_type& duration)
        {
            return boost::posix_time::microseconds(duration.count()/1000);
        }  
    }; // struct steady_time_traits

    typedef boost::asio::basic_deadline_timer<steady_time_traits::time_type, 
                                              steady_time_traits> steady_timer;

    inline steady_timer::duration_type to_steady_timer_duration(const boost::posix_time::time_duration& posix_duration)
    {
        return boost::chrono::nanoseconds(posix_duration.total_nanoseconds());
    }

}
}

#  endif
#endif
