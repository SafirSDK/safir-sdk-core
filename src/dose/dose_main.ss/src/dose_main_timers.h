/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / stawi
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

#ifndef __DOSE_MAIN_TIMERS_H__
#define __DOSE_MAIN_TIMERS_H__

#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <boost/shared_ptr.hpp>
#include <set>
#include <boost/noncopyable.hpp>
#include <vector>
#include <map>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{

    typedef Safir::Dob::Typesystem::Int32 TimerId;

    class TimerInfoBase:
        private boost::noncopyable
    {
    public:
        TimerInfoBase(const TimerId timerId);

        virtual ~TimerInfoBase();

        const TimerId GetTimerId() const {return m_timerId;}

        virtual bool operator<(const TimerInfoBase & other) const = 0;

    private:
        const TimerId m_timerId;
    };

    typedef boost::shared_ptr<TimerInfoBase> TimerInfoPtr;
    typedef boost::shared_ptr<const TimerInfoBase> TimerInfoConstPtr;


    class TimerInfoPtrLess:
        public std::binary_function<const TimerInfoConstPtr &,const TimerInfoConstPtr &,bool>
    {
    public:
        result_type operator() (first_argument_type first, second_argument_type second) const
        {
            return *first < *second;
        }
    };

    //UserData must provide an operator< with strict weak ordering
    template <class UserDataType>
    class TimerInfo :
        public TimerInfoBase
    {
    public:
        TimerInfo(const TimerId timerId, const UserDataType & userData);

        UserDataType & UserData() {return m_userData;}
        const UserDataType & UserData() const {return m_userData;}

        virtual bool operator<(const TimerInfoBase & other) const;
    private:
        UserDataType m_userData;
    };


    class EmptyTimerInfo :
        public TimerInfoBase
    {
    public:
        EmptyTimerInfo(const TimerId timerId);

        virtual bool operator<(const TimerInfoBase & other) const;
    };

    class TimeoutHandler
    {
    public:
        virtual ~TimeoutHandler() {}

        virtual void HandleTimeout(const TimerInfoPtr & timer) = 0;
    };


    enum SetPolicy
    {
        Replace, //replace existing timer (if it is there)
        Discard, //discard the new timer if it is already set
    };

    /**
     * Note: This class is NOT thread safe!
     *       Only call this from the main dose_main thread!
     */
    class TimerHandler:
        private boost::noncopyable
    {
    public:
        static void Instantiate(boost::asio::io_service & ioService);

        /** Instantiate must have been called for this to function. */
        static TimerHandler & Instance();

        void Stop();

        const TimerId RegisterTimeoutHandler(const std::wstring & timerName, TimeoutHandler & timeoutHandler);

        const std::wstring & GetTimerName(const TimerId timerId) const;

        void Set(const SetPolicy policy,
                 const TimerInfoPtr & timerInfo,
                 const boost::chrono::steady_clock::time_point& when);

        void SetRelative(const SetPolicy policy,
                         const TimerInfoPtr & timerInfo,
                         const Safir::Dob::Typesystem::Si64::Second inSeconds)
        {
            Set(policy,
                timerInfo,
                boost::chrono::steady_clock::now() + boost::chrono::microseconds(static_cast<boost::int64_t>(inSeconds*1.0e6)));
        }

        //
        // Remove (all timers that match will be removed)
        //
        void Remove(const TimerId timerId);  //O(n)

        //can be a new timerInfo with the same contents. It does not have to be the same pointer!
        void Remove(const TimerInfoPtr & timerInfo); //O(1)

        void Remove(const TimerInfoBase & timerInfo); //O(1)

        //
        // Exists (true if any timer matches)
        //
        bool IsSet(const TimerId timerId) const;  //O(n)

        //can be a new timerInfo with the same contents. It does not have to be the same pointer!
        bool IsSet(const TimerInfoPtr & timerInfo) const; //O(1)

    private:

        //returns the next timeout time in absolute time
        const boost::chrono::steady_clock::time_point NextTimeout() const;

        void HandleTimeout(const boost::system::error_code & error);

        void ScheduleTimer();

        explicit TimerHandler(boost::asio::io_service & ioService);
        ~TimerHandler();

        //must use custom comparer to compare pointer contents rather than pointers.
        typedef std::map<TimerInfoPtr,boost::chrono::steady_clock::time_point, TimerInfoPtrLess> TimerTable;
        typedef std::multimap<boost::chrono::steady_clock::time_point,TimerTable::iterator> TimerQueue;
        typedef std::vector<std::pair<std::wstring, TimeoutHandler *> > TimeoutHandlerTable;

        TimerTable m_timerTable;
        TimerQueue m_timerQueue;

        TimeoutHandlerTable m_timeoutHandlerTable;

        boost::asio::io_service & m_ioService;
        boost::asio::steady_timer m_steadyTimer;

        static TimerHandler* m_instance;
    };



    template <class UserDataType>
    TimerInfo<UserDataType>::TimerInfo(const TimerId timerId,
                                       const UserDataType & userData):
        TimerInfoBase(timerId),
        m_userData(userData)
    {

    }

    template <class UserDataType>
    bool
    TimerInfo<UserDataType>::operator<(const TimerInfoBase & other) const
    {
        if (GetTimerId() != other.GetTimerId())
        {
            return GetTimerId() < other.GetTimerId();
        }
        else
        {
#ifndef NDEBUG
            if (typeid(*this) != typeid(other))
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Two incompatible types are being used with the same TimerId",__WFILE__,__LINE__);
            }
#endif
            return m_userData < static_cast<const TimerInfo &>(other).m_userData;
        }
    }
}
}
}

#endif
