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

#include "dose_controller_table.h"
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/PanicLogging.h>
#include <iostream>
#include <string>
#include "dose_controller.h"
#include <ace/Guard_T.h>

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4702)
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

    ControllerTable * volatile ControllerTable::m_instance = NULL;

    ACE_Thread_Mutex ControllerTable::m_instantiationLock;

    //
    //Instance
    //
    ControllerTable & ControllerTable::Instance()
    {
        if (m_instance == NULL)
        {
            ACE_Guard<ACE_Thread_Mutex> lck(m_instantiationLock);

            if (m_instance == NULL)
            {
                m_instance = new ControllerTable();
            }
        }
        return *m_instance;
    }

    ControllerTable::ControllerTable():
        m_threadWarningsEnabled(Safir::Dob::NodeParameters::ThreadingWarningsEnabled())
    {

    }

    ControllerTable::~ControllerTable()
    {

    }

    long ControllerTable::AddController(ControllerPtr controller)
    {
        ACE_Guard<ACE_Thread_Mutex> lck(m_lock);

        long ctrl = -1;

        for (;;)
        {
            // Generate a unique id for the controller
            ctrl = abs(static_cast<long>(Safir::Dob::Typesystem::Internal::GenerateRandom64Bit()));

            if (m_controllers.insert(std::make_pair(ctrl, controller)).second)
            {
                break;
            }
        }

        controller->SetInstanceId(ctrl);

        return ctrl;
    }

    void ControllerTable::RemoveController(const long ctrl)
    {
        ACE_Guard<ACE_Thread_Mutex> lck(m_lock);

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

    void ControllerTable::CheckThread(const long ctrl) const
    {
        if (!m_threadWarningsEnabled)
        {
            return;
        }

        ACE_Guard<ACE_Thread_Mutex> lck(m_lock);

        ThreadControllersTable::const_iterator findIt = m_threadControllersTable.find(ACE_Thread::self());
        if (findIt != m_threadControllersTable.end())
        {
            for (ControllerInfoList::const_iterator it = findIt->second.begin();
                 it != findIt->second.end(); ++it)
            {
                if (ctrl == it->m_ctrl)
                {
                    return;
                }
            }
        }


        std::wostringstream ostr;
        ostr << "You are trying to use a connection from a different thread than it was Opened in." << std::endl
             << "Getting connection name (this may fail if you've done something really wrong): " <<std::endl;
        const char * name;
        try
        {
            ControllerConstPtr ctrlPtr = GetControllerInternal(ctrl);
            if (ctrlPtr != NULL)
            {
                name = ctrlPtr->GetConnectionName();
                ostr << "Got it: '" << name << "'" << std::endl;
            }
        }
        catch (const std::exception & exc)
        {
            ostr << "Failed: "<< exc.what() <<std::endl;
        }

        bool found = false;
        //try to find it anyway (we want to remove it so nothing strange happens..
        for (ThreadControllersTable::const_iterator it = m_threadControllersTable.begin();
             it != m_threadControllersTable.end(); ++it)
        {
            for (ControllerInfoList::const_iterator it2 = it->second.begin();
                 it2 != it->second.end(); ++it2)
            {
                if (ctrl == it2->m_ctrl)
                {
                    ostr << "The thread id that you called from was "
                         << ACE_Thread::self()
                         << " but DOSE expected you to call it from "
                         << it->first
                         <<std::endl;

                    found = true;
                    break;
                }
            }
        }

        if (!found)
        {
            ostr << "Could not find the connection in any thread at all!" <<std::endl;
        }

        std::wcout << ostr.str();
        lllout << ostr.str();
        Safir::Utilities::Internal::PanicLogging::Log(Safir::Dob::Typesystem::Utilities::ToUtf8(ostr.str()));
    }

    ControllerPtr ControllerTable::GetController(const long ctrl)
    {
        //Avoid code duplication between the const and non-const version of this method.
        return boost::const_pointer_cast<Controller>(static_cast<const ControllerTable*>(this)->GetController(ctrl));
    }

    ControllerConstPtr ControllerTable::GetController(const long ctrl) const
    {
        ACE_Guard<ACE_Thread_Mutex> lck(m_lock);

        return GetControllerInternal(ctrl);
    }

    ControllerPtr ControllerTable::GetControllerByName(const std::string & name)
    {
        ACE_Guard<ACE_Thread_Mutex> lck(m_lock);

        for (ControllerMap::iterator it=m_controllers.begin();
             it != m_controllers.end(); ++it)
        {
            if (it->second != NULL)
            {
                const char* tmpName;
                //if it is not connected there is no connection name to get
                if (it->second->IsConnected())
                {
                    tmpName = it->second->GetConnectionName();
                    CheckThread(it->first); //check the threading if we're in debug build

                    if (name == std::string(tmpName))
                    {
                        return it->second;
                    }
                }
            }
        }
        ENSURE(false, << "An unknown controller name was used! " << name.c_str());
        return ControllerPtr(); //Keep compiler happy
    }

    void ControllerTable::SetThread(const long ctrl)
    {
        ACE_Guard<ACE_Thread_Mutex> lck(m_lock);

        const ACE_thread_t tid = ACE_Thread::self();
        ThreadControllersTable::iterator findIt = m_threadControllersTable.find(tid);
        if (findIt == m_threadControllersTable.end())  //need to insert the thread id into the map
        {
            //controller id list added will be empty to start off with.
            findIt = m_threadControllersTable.insert(std::make_pair(tid,ControllerInfoList())).first;
        }

        findIt->second.push_back(ControllerInfo(ctrl));

        //            std::wcout << "ControllerTable::SetThread() - ctrl: " << ctrl << ". tid: " << tid << std::endl;
    }

    long
    ControllerTable::GetFirstControllerInThread() const
    {
        ACE_Guard<ACE_Thread_Mutex> lck(m_lock);
        ThreadControllersTable::const_iterator findIt = m_threadControllersTable.find(ACE_Thread::self());
        if (findIt == m_threadControllersTable.end() || findIt->second.empty())
        {
            throw Safir::Dob::NotOpenException(L"Failed to find unnamed connection in this thread",__WFILE__,__LINE__);
        }
        else
        {
            return findIt->second.front().m_ctrl;
        }
    }

    long
    ControllerTable::GetNamedControllerInThread(const std::string & connectionNameCommonPart,
                                                const std::string & connectionNameInstancePart) const
    {
        ACE_Guard<ACE_Thread_Mutex> lck(m_lock);

        ThreadControllersTable::const_iterator findIt = m_threadControllersTable.find(ACE_Thread::self());
        if (findIt != m_threadControllersTable.end()) //if it was not found we will go into the bit below
        {
            for (ControllerInfoList::const_iterator it = findIt->second.begin();
                 it != findIt->second.end(); ++it)
            {
                ControllerConstPtr ctrlPtr = GetControllerInternal(it->m_ctrl);
                if (ctrlPtr != NULL && ctrlPtr->NameIsEqual(connectionNameCommonPart,connectionNameInstancePart))
                {
                    return it->m_ctrl;
                }
            }
        }

        std::wostringstream msg;
        msg << "Failed to find named connection in this thread (" << connectionNameCommonPart.c_str() << ", " << connectionNameInstancePart.c_str() << ")";
        throw Safir::Dob::NotOpenException(msg.str() ,__WFILE__,__LINE__);
    }

    void
    ControllerTable::UnsetThread(const long ctrl, const bool checkThread)
    {
        ACE_Guard<ACE_Thread_Mutex> lck(m_lock);

        ThreadControllersTable::iterator findIt = m_threadControllersTable.find(ACE_Thread::self());
        if (findIt != m_threadControllersTable.end())
        {
            for (ControllerInfoList::iterator it = findIt->second.begin();
                 it != findIt->second.end(); ++it)
            {
                if (ctrl == it->m_ctrl)
                {
                    findIt->second.erase(it);
                    return;
                }
            }
        }

        std::wostringstream ostr;
        try
        {
            ostr << "You are trying to Close a connection from a different thread than it was Opened in." << std::endl
                 << "This could either be because you're not calling Close from the right thread, or that you "
                 << "are not calling Close at all, so that it gets called from the destructor, which can get "
                 << "called from a different thread." <<std::endl
                 << "Getting connection name (this may fail if you've done something really wrong): " <<std::endl;
            const char * name;
            try
            {
                ControllerConstPtr ctrlPtr = GetControllerInternal(ctrl);
                if (ctrlPtr != NULL)
                {
                    name = ctrlPtr->GetConnectionName();
                    ostr << "Got it: '" << name << "'" << std::endl;
                }
            }
            catch (const std::exception & exc)
            {
                ostr << "Failed: "<< exc.what() <<std::endl;
            }

            bool found = false;
            //try to find it anyway (we want to remove it so nothing strange happens..
            for (ThreadControllersTable::iterator it = m_threadControllersTable.begin();
                 it != m_threadControllersTable.end(); ++it)
            {
                for (ControllerInfoList::iterator it2 = it->second.begin();
                     it2 != it->second.end(); ++it2)
                {
                    if (ctrl == it2->m_ctrl)
                    {
                        ostr << "The thread id that you called from was "
                             << ACE_Thread::self()
                             << " but DOSE expected you to call it from "
                             << it->first
                             <<std::endl;

                        it->second.erase(it2);
                        found = true;
                        break;
                    }
                }
            }

            if (!found)
            {
                //oh well, the connection was not opened after all. just skip outputting
                //the error message...
                return;
            }

            if (checkThread)
            {
                std::wcout << ostr.str();
                lllout << ostr.str();
            }
        }
        catch (...)
        {
            if (checkThread)
            {
                ostr <<std::endl<< "caught exception while building error message!!!"<<std::endl;
                std::wcout << ostr.str();
                lllout << ostr.str();
            }
            throw;
        }
    }

    ControllerConstPtr ControllerTable::GetControllerInternal(const long ctrl) const
    {
        ControllerMap::const_iterator it = m_controllers.find(ctrl);

        if (it == m_controllers.end())
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException
                (std::wstring(L"Internal error! An unknown controllerId was used: ") +
                 boost::lexical_cast<std::wstring>(ctrl),__WFILE__,__LINE__);
        }

        return it->second;
    }
}
}
}
