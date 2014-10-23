/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
#include <ace/Event_Handler.h>
#include <ace/Time_Value.h>

#ifdef TimerId
#undef TimerId
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{

    ACE_Time_Value GetUtcTime();

    ACE_Time_Value GetMonotonicTime();


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
        public ACE_Event_Handler,
        private boost::noncopyable
    {
    public:
        static TimerHandler & Instance();

        const TimerId RegisterTimeoutHandler(const std::wstring & timerName, TimeoutHandler & timeoutHandler);

        const std::wstring & GetTimerName(const TimerId timerId) const;

        void Set(const SetPolicy policy,
                 const TimerInfoPtr & timerInfo,
                 const ACE_Time_Value& when);

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


        //will only handle one timeout at a time, to allow event handling between each timeout.
        //void HandleTimeout();

    private:

        //returns the time to the next timeout (relative time)
        const ACE_Time_Value NextTimeout() const;

        int handle_timeout(const ACE_Time_Value& current_time, const void* act);

        void ScheduleTimer();

        TimerHandler();
        ~TimerHandler();

        //must use custom comparer to compare pointer contents rather than pointers.
        typedef std::map<TimerInfoPtr,ACE_Time_Value, TimerInfoPtrLess> TimerTable;
        typedef std::multimap<ACE_Time_Value,TimerTable::iterator> TimerQueue;
        typedef std::vector<std::pair<std::wstring, TimeoutHandler *> > TimeoutHandlerTable;

        TimerTable m_timerTable;
        TimerQueue m_timerQueue;

        TimeoutHandlerTable m_timeoutHandlerTable;
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

