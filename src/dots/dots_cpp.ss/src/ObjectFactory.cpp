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

#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/BlobOperations.h>
#include <ace/Guard_T.h>
#include <ace/Recursive_Thread_Mutex.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    ObjectFactory * volatile ObjectFactory::m_pInstance = NULL;

    ObjectFactory &
    ObjectFactory::Instance()
    {
        if (!m_pInstance)
        {
            static ACE_Recursive_Thread_Mutex creationLock;

            ACE_Guard<ACE_Recursive_Thread_Mutex> lck(creationLock);
            if (!m_pInstance)
            {
                m_pInstance = new ObjectFactory();
            }
        }
        return *m_pInstance;
    }

    ObjectFactory::ObjectFactory()
    {

    }

    ObjectFactory::~ObjectFactory()
    {

    }


    ObjectPtr
    ObjectFactory::CreateObject(char const * const blob) const
    {
        if (blob == NULL)
        {
            throw SoftwareViolationException(L"Cannot create object from NULL blob!",__WFILE__,__LINE__);
        }
        const TypeId typeId = BlobOperations::GetTypeId(blob);
        CallbackMap::const_iterator it = m_CallbackMap.find(typeId);
        if (it == m_CallbackMap.end())
        {
            throw IllegalValueException(L"There is no such type registered in the ObjectFactory",__WFILE__,__LINE__);
        }
        //invoke the function
        return (it->second(blob));
    }

    ObjectPtr
    ObjectFactory::CreateObject(const TypeId typeId) const
    {
        CallbackMap::const_iterator it = m_CallbackMap.find(typeId);
        if (it == m_CallbackMap.end())
        {
            throw IllegalValueException(L"There is no such type registered in the ObjectFactory",__WFILE__,__LINE__);
        }
        //invoke the function
        return (it->second(NULL));
    }
}
}
}
