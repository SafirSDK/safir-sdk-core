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

#include "dose_controller_table.h"
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <iostream>
#include <string>
#include "dose_controller.h"

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4702 4244)
#endif

#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    boost::once_flag ControllerTable::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;

    ControllerTable & ControllerTable::SingletonHelper::Instance()
    {
        static ControllerTable instance;
        return instance;
    }

    ControllerTable & ControllerTable::Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
        return SingletonHelper::Instance();
    }

    ControllerTable::ControllerTable()
    {

    }

    ControllerTable::~ControllerTable()
    {

    }

    std::int32_t ControllerTable::AddController(ControllerPtr controller)
    {
        boost::lock_guard<boost::mutex> lck(m_lock);

        for (std::int32_t ctrl = 1; ctrl < 100000; ++ ctrl)
        {
            if (m_controllers.insert(std::make_pair(ctrl, controller)).second)
            {
                controller->SetInstanceId(ctrl);
                return ctrl;
            }
        }
        ENSURE(false, << "Too many controllers in one application");
        return false;
    }

    void ControllerTable::RemoveController(const std::int32_t ctrl)
    {
        boost::lock_guard<boost::mutex> lck(m_lock);

        ControllerMap::iterator it = m_controllers.find(ctrl);

        if (it != m_controllers.end())
        {
            if (it->second != NULL)
            {
                it->second->Disconnect();
            }
            m_controllers.erase(it);
        }
    }


    ControllerPtr ControllerTable::GetController(const std::int32_t ctrl)
    {
        boost::lock_guard<boost::mutex> lck(m_lock);

        ControllerMap::const_iterator it = m_controllers.find(ctrl);

        if (it == m_controllers.end())
        {
            // Return a null pointer if not found
            return ControllerPtr();
        }

        return it->second;
    }

    void ControllerTable::SetThread(const std::int32_t ctrl)
    {
        boost::lock_guard<boost::mutex> lck(m_lock);

        const boost::thread::id tid = boost::this_thread::get_id();
        ThreadControllersTable::iterator findIt = m_threadControllersTable.find(tid);
        if (findIt == m_threadControllersTable.end())  //need to insert the thread id into the map
        {
            //controller id list added will be empty to start off with.
            findIt = m_threadControllersTable.insert(std::make_pair(tid,ControllerIdList())).first;
        }

        findIt->second.push_back(ctrl);
    }

    std::int32_t
    ControllerTable::GetFirstControllerInThread() const
    {
        boost::lock_guard<boost::mutex> lck(m_lock);
        ThreadControllersTable::const_iterator findIt = m_threadControllersTable.find(boost::this_thread::get_id());
        if (findIt == m_threadControllersTable.end() || findIt->second.empty())
        {
            throw Safir::Dob::NotOpenException(L"Failed to find unnamed connection in this thread",__WFILE__,__LINE__);
        }
        else
        {
            return findIt->second.front();
        }
    }

    std::int32_t
    ControllerTable::GetNamedController(const std::string & connectionNameCommonPart,
                                        const std::string & connectionNameInstancePart) const
    {
        boost::lock_guard<boost::mutex> lck(m_lock);

        for (const auto& pair: m_controllers)
        {
            if (pair.second->NameIsEqual(connectionNameCommonPart,connectionNameInstancePart))
            {
                return pair.first;
            }
        }

        std::wostringstream msg;
        msg << "Failed to find named connection (" << connectionNameCommonPart.c_str() << ", " << connectionNameInstancePart.c_str() << ")";
        throw Safir::Dob::NotOpenException(msg.str() ,__WFILE__,__LINE__);
    }

    void
    ControllerTable::UnsetThread(const std::int32_t ctrl)
    {
        boost::lock_guard<boost::mutex> lck(m_lock);

        ThreadControllersTable::iterator findIt = m_threadControllersTable.find(boost::this_thread::get_id());
        if (findIt != m_threadControllersTable.end())
        {
            for (ControllerIdList::iterator it = findIt->second.begin();
                 it != findIt->second.end(); ++it)
            {
                if (ctrl == *it)
                {
                    findIt->second.erase(it);
                    return;
                }
            }
        }
    }

}
}
}
