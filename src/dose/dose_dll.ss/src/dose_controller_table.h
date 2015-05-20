/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

#ifndef __DOSE_THREAD_INFO_H__
#define __DOSE_THREAD_INFO_H__

#include <map>
#include <vector>
#include <string>
#include <cstdint>
#include <boost/thread/mutex.hpp>
#include <boost/shared_ptr.hpp>

#ifdef _MSC_VER
#  pragma warning(push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4100)
#endif

#include <boost/thread.hpp>

#ifdef _MSC_VER
#  pragma warning(pop)
#endif


namespace Safir
{
namespace Dob
{
namespace Internal
{
    //forward declaration
    class Controller;
    typedef boost::shared_ptr<Controller> ControllerPtr;


    class ControllerTable
        : private boost::noncopyable
    {
    public:
        static ControllerTable & Instance();

        std::int32_t AddController(ControllerPtr controller);
        void RemoveController(const std::int32_t ctrl);

        ControllerPtr GetController(const std::int32_t ctrl);

        /**
         * Set the thread that the controller acts in.
         */
        void SetThread(const std::int32_t ctrl);

        /**
         * Remove the controller from the thread.
         */
        void UnsetThread(const std::int32_t ctrl);

        /**
         * Get the first controller that is set to the current thread.
         */
        std::int32_t GetFirstControllerInThread() const;

        /**
         * Get the named controller that is set to the current thread.
         */
        std::int32_t GetNamedController(const std::string & connectionNameCommonPart,
                                const std::string & connectionNameInstancePart) const;


    private:
        ControllerTable();
        ~ControllerTable();

        /**
         * This class is here to ensure that only the Instance method can get at the
         * instance, so as to be sure that boost call_once is used correctly.
         * Also makes it easier to grep for singletons in the code, if all
         * singletons use the same construction and helper-name.
         */
        struct SingletonHelper
        {
        private:
            friend ControllerTable& ControllerTable::Instance();

            static ControllerTable& Instance();
            static boost::once_flag m_onceFlag;
        };

        mutable boost::mutex m_lock;

        typedef std::map<std::int32_t, ControllerPtr> ControllerMap;
        ControllerMap m_controllers;


        typedef std::vector<std::int32_t> ControllerIdList;
        typedef std::map<boost::thread::id, ControllerIdList> ThreadControllersTable;
        ThreadControllersTable m_threadControllersTable;
    };

}
}
}

#endif
