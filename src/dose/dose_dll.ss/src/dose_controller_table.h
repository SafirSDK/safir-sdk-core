/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagstr�m / stlrha
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
#include <boost/thread/mutex.hpp>
#include <boost/shared_ptr.hpp>

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4244)
#endif

#include <boost/thread.hpp>

#ifdef _MSC_VER
  #pragma warning(pop)
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
    typedef boost::shared_ptr<const Controller> ControllerConstPtr;


    class ControllerTable
    {
    public:
        static ControllerTable & Instance();

        long AddController(ControllerPtr controller);
        void RemoveController(const long ctrl);

        ControllerPtr GetController(const long ctrl);
        ControllerConstPtr GetController(const long ctrl) const;

        ControllerPtr GetControllerByName(const std::string & name);

        /**
         * Set the thread that the controller acts in.
         */
        void SetThread(const long ctrl);

        /**
         * Remove the controller from the thread.
         */
        void UnsetThread(const long ctrl, const bool checkThread);

        /**
         * Get the first controller that is set to the current thread.
         */
        long GetFirstControllerInThread() const;

        /**
         * Get the named controller that is set to the current thread.
         */
        long GetNamedControllerInThread(const std::string & connectionNameCommonPart,
                                        const std::string & connectionNameInstancePart) const;

        /**
         * Check that the controller is set to the current thread.
         * Only done in debug builds
         */
        void CheckThread(const long ctrl) const;

    private:
        ControllerTable();
        ~ControllerTable();

        //declare but dont implement to prevent copying
        explicit ControllerTable(const ControllerTable &);
        ControllerTable & operator=(const ControllerTable &);

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

        ControllerConstPtr GetControllerInternal(const long ctrl) const;

        mutable boost::mutex m_lock;

        typedef std::map<long, ControllerPtr> ControllerMap;
        ControllerMap m_controllers;

        struct ControllerInfo
        {
            ControllerInfo(const long ctrl):m_ctrl(ctrl) {}
            long m_ctrl; //controller id
            //NodeNumber m_lastDataSender; //maybe we want something like this here...
        };

        typedef std::vector<ControllerInfo> ControllerInfoList;
        typedef std::map<boost::thread::id, ControllerInfoList > ThreadControllersTable;

        ThreadControllersTable m_threadControllersTable;

        const bool m_threadWarningsEnabled;
    };

}
}
}

#endif
