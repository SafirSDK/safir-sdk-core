/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

#ifndef __DOTS_EXCEPTION_KEEPER_H__
#define __DOTS_EXCEPTION_KEEPER_H__

#include <boost/noncopyable.hpp>
#include <Safir/Dob/Typesystem/Internal/KernelDefs.h>
#include <map>
//disable warnings in boost and ace
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4267)
#endif

#include <ace/Thread.h>
#include <ace/Thread_Mutex.h>

//and enable the warnings again
#if defined _MSC_VER
  #pragma warning (pop)
#endif

#include <string>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * A class to hold the current exception for each thread.
     */
    class ExceptionKeeper:
        private boost::noncopyable
    {
    public:
        static ExceptionKeeper & Instance();

        void Set(const DotsC_TypeId exceptionId, const std::string & description);

        void AppendDescription(const std::string & moreDescription);

        bool IsSet() const;

        bool GetAndClear(DotsC_TypeId & exceptionId, std::string & description);

        void Peek(DotsC_TypeId & exceptionId, std::string & description) const;

        void Clear();
    private:
        ExceptionKeeper();
        ~ExceptionKeeper();

        struct ExceptionData
        {
            ExceptionData():isSet(false){}
            ExceptionData(const DotsC_TypeId id, const std::string & descr):
                exceptionId(id), description(descr){}

            bool isSet;
            DotsC_TypeId exceptionId;
            std::string description;
        };

        ExceptionData & GetDataForCurrentThread() const;

        typedef std::map<ACE_thread_t, ExceptionData> ThreadExceptionTable;

        mutable ThreadExceptionTable m_threadExceptionTable;

        mutable ACE_Thread_Mutex m_lock;

        //Singleton stuff
        static ExceptionKeeper * volatile m_instance;

        static ACE_Thread_Mutex m_instantiationLock;
    };
}
}
}
}

#endif

