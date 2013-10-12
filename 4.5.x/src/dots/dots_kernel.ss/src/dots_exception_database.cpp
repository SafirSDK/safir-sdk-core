/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
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

#include "dots_exception_database.h"
//#include "dots_basic_types.h"

#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
   
    static const char* EXCEPTION_DATABASE_NAME = "EXCEPTION_DATABASE";


    ExceptionDatabase::ExceptionDatabase(create_and_initialize_t, AllocationHelper & allocHelper)
    {
        m_exceptions = allocHelper.GetShmem()->construct<ExceptionTable>(EXCEPTION_DATABASE_NAME)
            (std::less<TypeId>(),allocHelper.GetAllocator<ExceptionTable>());
    }

    ExceptionDatabase::ExceptionDatabase(open_only_t,boost::interprocess::managed_shared_memory & shmem)
    {
        m_exceptions = shmem.find<ExceptionTable>(EXCEPTION_DATABASE_NAME).first;
        if (m_exceptions == NULL)
        {
            throw InternalException("Failed to open EXCEPTION_DATABASE",__FILE__,__LINE__);
        }
    }

    ExceptionDatabase::~ExceptionDatabase()
    {
    }

    const ExceptionDescription * ExceptionDatabase::FindException(const TypeId typeId) const
    {
        ExceptionTable::const_iterator findIt = m_exceptions->find(typeId);
        if (findIt != m_exceptions->end())
        {
            return &findIt->second;
        }
        else
        {
            return NULL;
        }
    }

    void ExceptionDatabase::GetTypeIds(TypeId * const buf,
                                       const Int32 bufSize,
                                       Int32& resultSize) const
    {
        resultSize=0;
        for (ExceptionTable::const_iterator it = m_exceptions->begin();
             it != m_exceptions->end(); ++it)
        {
            buf[resultSize] = it->first;
            ++resultSize;

            if (resultSize == bufSize)
            {
                break;
            }
        }
    }

    void ExceptionDatabase::InsertExceptions(DobExceptions exceptions, AllocationHelper& allocHelper)
    {
        int remainingTries = 100;
        while (remainingTries>0 && !exceptions.empty())
        {
            --remainingTries;
            for (DobExceptions::iterator it = exceptions.begin();
                 it != exceptions.end(); ) //no increment here!
            {
                bool removeIt = false;
                //insert base classes
                if (it->m_baseClass.empty())
                {
                    lllout << "Inserting base class " << it->m_name.c_str() << std::endl;
                    m_exceptions->insert(std::make_pair(it->m_typeId, ExceptionDescription(it->m_name,it->m_typeId,NULL,allocHelper)));
                    removeIt = true;
                }
                else 
                {
                    ExceptionTable::iterator findIt = m_exceptions->find(it->m_baseClassTypeId);
                    
                    if (findIt != m_exceptions->end())
                    {
                        const ExceptionDescriptionConstPtr base = &findIt->second;
                        lllout << "Inserting exception " << it->m_name.c_str() << " that derives from " << it->m_baseClass.c_str() << std::endl;
                        m_exceptions->insert(std::make_pair(it->m_typeId, ExceptionDescription(it->m_name,it->m_typeId,base,allocHelper)));
                        removeIt = true;
                    }
                    else
                    {
                        lllout << it->m_name.c_str() << " not inserted, since base class " << it->m_baseClass.c_str() << " not found, retrying" << std::endl;
                    }
                }
                
                if (removeIt)
                {
                    DobExceptions::iterator removeIt = it;
                    ++it;
                    exceptions.erase(removeIt);
                }
                else
                {
                    ++it;
                }


            }
        }

        if (!exceptions.empty())
        {
            std::wcout << "Some exceptions were not inserted! Base classes not found for these exceptions:" << std::endl;
            for (DobExceptions::iterator it = exceptions.begin();
                 it != exceptions.end(); ++it)
            {
                std::wcout << "  " << it->m_name.c_str() << " with baseClass " << it->m_baseClass.c_str() << std::endl;
            }

            exit(22);
        }
    }
}
}
}
}
