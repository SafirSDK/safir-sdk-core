/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#include "dots_exception_keeper.h"
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <sstream>
#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    boost::once_flag ExceptionKeeper::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;

    ExceptionKeeper & ExceptionKeeper::SingletonHelper::Instance()
    {
        static ExceptionKeeper instance;
        return instance;
    }

    ExceptionKeeper & ExceptionKeeper::Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
        return SingletonHelper::Instance();
    }

    ExceptionKeeper::ExceptionKeeper()
    {

    }

    ExceptionKeeper::~ExceptionKeeper()
    {

    }


    ExceptionKeeper::ExceptionData &
    ExceptionKeeper::GetDataForCurrentThread() const
    {
        boost::lock_guard<boost::mutex> lck(m_lock);
        const boost::thread::id threadId = boost::this_thread::get_id();
        ThreadExceptionTable::iterator it = m_threadExceptionTable.find(threadId);
        if (it == m_threadExceptionTable.end()) //not found
        {
            it = m_threadExceptionTable.insert(std::make_pair(threadId,ExceptionData())).first;
        }
        return it->second;
    }

    void
    ExceptionKeeper::Set(const DotsC_TypeId exceptionId, const std::string & description)
    {
        ExceptionData & exc = GetDataForCurrentThread();

        lllog(2) << "ExceptionKeeper::Set: id = " << exceptionId << " description = " << description.c_str() <<std::endl;
        if (exc.isSet)
        {
            SEND_SYSTEM_LOG(Critical,
                            << "ExceptionKeeper::Set: Trying to set new exception data over old (and uncleared) exception data");
            SEND_SYSTEM_LOG(Critical,
                            << "Old = (" << exc.exceptionId << ", " << exc.description.c_str() << ")");
            SEND_SYSTEM_LOG(Critical,
                            << "New = (" << exceptionId << ", " << description.c_str() << ")");
        }
        exc.exceptionId = exceptionId;
        exc.description = description;
        exc.isSet = true;
    }

    void
    ExceptionKeeper::AppendDescription(const std::string & moreDescription)
    {
        lllog(2) << "ExceptionKeeper::AppendDescription: moreDescription = " << moreDescription.c_str() << std::endl;
        ExceptionData & exc = GetDataForCurrentThread();
        if (!exc.isSet)
        {
            SEND_SYSTEM_LOG(Critical,
                            << "ExceptionKeeper::AppendDescription: Called even though there is no exception set!!!");
        }
        exc.description.append("\n-------------- More Description -------------\n");
        exc.description.append(moreDescription);
    }

    bool
    ExceptionKeeper::IsSet() const
    {
        return GetDataForCurrentThread().isSet;
    }

    bool
    ExceptionKeeper::GetAndClear(DotsC_TypeId & exceptionId, std::string & description)
    {
        ExceptionData & exc = GetDataForCurrentThread();
        if (!exc.isSet)
        {
            SEND_SYSTEM_LOG(Critical,
                            << "ExceptionKeeper::GetAndClear: when no exception is set!");
            return false;
        }
        else
        {
            exceptionId = exc.exceptionId ;
            description = exc.description;
            lllog(2) << "ExceptionKeeper::GetAndClear: id = " << exceptionId << " description = " << description.c_str() <<std::endl;

            exc.isSet = false;
            return true;
        }
    }

    void
    ExceptionKeeper::Peek(DotsC_TypeId & exceptionId, std::string & description) const
    {
        ExceptionData & exc = GetDataForCurrentThread();
        if (!exc.isSet)
        {
            SEND_SYSTEM_LOG(Critical,
                            << "ExceptionKeeper::Peek: Called when no exception is set!");
            exceptionId = 0;
            description.clear();
        }
        else
        {
            exceptionId = exc.exceptionId ;
            description = exc.description;
            lllog(2) << "ExceptionKeeper::Peek: id = " << exceptionId << " description = " << description.c_str() <<std::endl;
        }
    }

    void
    ExceptionKeeper::Clear()
    {
        ExceptionData & exc = GetDataForCurrentThread();
        if (!exc.isSet)
        {
            SEND_SYSTEM_LOG(Critical,
                            << "ExceptionKeeper::Clear: Called even though there is no exception set!!!");
        }
        else
        {
            lllog(2) << "ExceptionKeeper::Clear: called." << std::endl;
            exc.isSet = false;
        }
    }

}
}
}
}
