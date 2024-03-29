/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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
#pragma once

#include <Safir/Dob/Typesystem/LanguageInterfaceDefs.h>
#include <mutex>
#include <map>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable: 4267)
  #pragma warning (disable: 4100)
#endif

#include <boost/thread.hpp>

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
    class ExceptionKeeper
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

        ExceptionKeeper(const ExceptionKeeper&) = delete;
        ExceptionKeeper& operator=(const ExceptionKeeper&) = delete;

        struct ExceptionData
        {
            ExceptionData():isSet(false), exceptionId(0){}

            bool isSet;
            DotsC_TypeId exceptionId;
            std::string description;
        };

        ExceptionData & GetDataForCurrentThread() const;

        typedef std::map<boost::thread::id, ExceptionData> ThreadExceptionTable;

        mutable ThreadExceptionTable m_threadExceptionTable;

        mutable std::mutex m_lock;

        /**
         * This class is here to ensure that only the Instance method can get at the 
         * instance, so as to be sure that std call_once is used correctly.
         * Also makes it easier to grep for singletons in the code, if all 
         * singletons use the same construction and helper-name.
         */
        struct SingletonHelper
        {
        private:
            friend ExceptionKeeper& ExceptionKeeper::Instance();

            static ExceptionKeeper& Instance();
            static std::once_flag m_onceFlag;
        };
    };
}
}
}
}



