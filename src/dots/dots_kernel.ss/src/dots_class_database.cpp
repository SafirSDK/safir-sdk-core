/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://www.safirsdk.com)
* 
* Created by: Joel Ottosson / stjoot
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

#include "dots_class_database.h"
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include "dots_basic_types.h"
#include "dots_error_handler.h"
#include "dots_enum_database.h"

#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    static const char* CLASS_DATABASE_NAME = "CLASS_DATABASE";

    static const int CACHE_SIZE = 5;
    static const int UNUSED_CACHE_SLOT = -1;

    ClassDatabase::ClassDatabase(create_and_initialize_t, AllocationHelper & allocHelper):
        m_objectClassTypeId(DotsId_Generate64(OBJECT_CLASS))
    {
        m_classes = allocHelper.GetShmem()->construct<ClassTable>(CLASS_DATABASE_NAME)
            (std::less<TypeId>(),allocHelper.GetAllocator<ClassTable>());

        InitCache();
    }

    ClassDatabase::ClassDatabase(open_only_t, boost::interprocess::managed_shared_memory & shmem):
        m_objectClassTypeId(DotsId_Generate64(OBJECT_CLASS))
    {
        m_classes = shmem.find<ClassTable>(CLASS_DATABASE_NAME).first;
        if (m_classes == NULL)
        {
            throw InternalException("Failed to open CLASS_DATABASE",__FILE__,__LINE__);
        }
        
        InitCache();
    }

    void ClassDatabase::InitCache()
    {
        m_cachedTypeIds = CachedTypeIds(CACHE_SIZE, std::make_pair(UNUSED_CACHE_SLOT, static_cast<ClassDescription *>(NULL)));
        m_lastUsedIdx = 0;
        m_lastInsertedIdx = 0;
    }

    ClassDatabase::~ClassDatabase()
    {

    }

//disable warnings about constant conditional
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4127)
#endif

    ClassDescription * ClassDatabase::FindClass(const TypeId typeId)
    {
        // There can be a large number of classes in a system and since this method is called
        // frequently it is optimized by saving the description pointers for the last looked up
        // typeIds in a vector. Note that this vector is local to each app so the mutex will only
        // affect the threads within the app. 

        boost::lock_guard<boost::mutex> lck(m_mutex);

        // If it happens that someone calls FindClass with a type id that is the same as we
        // use to indicate an unused slot (unlikely), we skip the cache part.  
        if (typeId != UNUSED_CACHE_SLOT  && CACHE_SIZE > 0)
        {
            int currIdx = m_lastUsedIdx;
            for(;;)
            {
                if (m_cachedTypeIds[currIdx].first == typeId)
                {
                    m_lastUsedIdx = currIdx;
                    return m_cachedTypeIds[currIdx].second;
                }
                // We move backwards as this probably maximize the chance of a quck cache hit
                if (currIdx > 0)
                {
                    --currIdx;
                }
                else
                {
                    currIdx = CACHE_SIZE - 1;
                }
                if (currIdx == m_lastUsedIdx)
                {
                    // We are back where we started and the typeid isn't found
                    break;
                }
            }
        }

        // Ok, the typeId wasn't cached, we have to make a full search

        ClassTable::iterator it = m_classes->find(typeId);

        if (it != m_classes->end())
        {
            // Kick out the oldest typeid from the cache and insert this one instead
            ++m_lastInsertedIdx;
            if (m_lastInsertedIdx >= CACHE_SIZE)
            {
                m_lastInsertedIdx = 0;
            }
            m_cachedTypeIds[m_lastInsertedIdx].first = typeId;
            m_cachedTypeIds[m_lastInsertedIdx].second = &it->second;

            m_lastUsedIdx = m_lastInsertedIdx;

            return &it->second;
        }
        else
        {
            return NULL;
        }
    }

//and enable the warnings again
#if defined _MSC_VER
  #pragma warning (pop)
#endif

    const ClassDescription * ClassDatabase::FindClass(const TypeId typeId) const
    {//call the non-const version
        return const_cast<ClassDatabase*>(this)->FindClass(typeId);
    }


    bool ClassDatabase::IsOfType(const TypeId theType, const TypeId ofType) const
    {
        //Same type is always compatible
        if (theType==ofType)
        {
            return true;
        }

        //Object is not compatible with any other type but Object itself
        else if (theType==m_objectClassTypeId)
        {
            return false;
        }

        const ClassDescription * cd = FindClass(theType);
        while (cd != NULL)
        {
            if (cd->BaseClass()->GetTypeId() == ofType)
            {
                return true;
            }
            else if (cd->BaseClass()->GetTypeId() == m_objectClassTypeId)
            {
                return false;
            }

            cd = cd->BaseClass().get();
        }
        return false;
        //we can get here if the type is an enumeration or a property
    }


    void ClassDatabase::GetCompleteType(const TypeId typeId,
                                        TypeId * buf,
                                        const Int32 bufSize,
                                        Int32 & noResults) const
    {
        const ClassDescription * const cde = FindClass(typeId);
        noResults=0;
        GetCompleteTypeInternal(cde, buf, bufSize, noResults);
    }


    void ClassDatabase::GetCompleteTypeInternal(const ClassDescription * const cde,
                                                TypeId * buf,
                                                const Int32 bufSize,
                                                Int32 & noResults)
    {
        if (noResults>=bufSize || cde==NULL)
        {
            return;
        }

        buf[noResults++]=cde->GetTypeId();
        for (ClassDescendants::const_iterator it = cde->Descendants().begin();
             it != cde->Descendants().end(); ++it)
        {
            GetCompleteTypeInternal(it->get(), buf, bufSize, noResults);
        }
    }




    void ClassDatabase::GetTypeIds(TypeId * const buf,
                                   const Int32 bufSize,
                                   Int32 & resultSize) const
    {
        resultSize=0;
        for (ClassTable::const_iterator it = m_classes->begin();
             it != m_classes->end(); ++it)
        {
            buf[resultSize] = it->first;
            ++resultSize;

            if (resultSize > bufSize)
            {
                break;
            }
        }
    }


    void ClassDatabase::InsertClasses(const DobClasses & temporaryDescriptions, const EnumDatabase & enums, AllocationHelper & allocHelper)
    {
        for (size_t i=0; i<temporaryDescriptions.size(); ++i)
        {
            InsertClass(temporaryDescriptions, i, enums, allocHelper);
        }

    }


    ClassDescriptionPtr ClassDatabase::InsertClass(const DobClasses & temporaryDescriptions, const size_t which, const EnumDatabase & enums, AllocationHelper & allocHelper)
    {
        //an alias to the one we're inserting at the moment.
        const DobClass & tmpClass = temporaryDescriptions[which];

        //scope so that findIt isn't visible throughout this method
        {
            //look in the table if we've already inserted the class, in that case return it.
            ClassTable::iterator findIt = m_classes->find(tmpClass.m_typeId);
            if (findIt != m_classes->end())
            {
                return &findIt->second;
            }
        }

        lllout << "Inserting class " << tmpClass.m_name.c_str() << std::endl;


        m_classes->insert(std::make_pair(tmpClass.m_typeId,
                                         ClassDescription(tmpClass.m_name,
                                                          tmpClass.m_typeId,
                                                          tmpClass.m_baseClassTypeId,
                                                          tmpClass.m_initialSize,
                                                          tmpClass.m_thisClassSize,
                                                          tmpClass.m_noInheritedMembers,
                                                          tmpClass.m_noInheritedParameters,
                                                          static_cast<Size>(tmpClass.m_noInheritedMembers+tmpClass.m_members.size()),
                                                          static_cast<Size>(tmpClass.m_desc.size()),
                                                          static_cast<Size>(tmpClass.m_noInheritedParameters+tmpClass.m_parameters.size()),
                                                          static_cast<Size>(tmpClass.m_propertyMappings.size()),
                                                          allocHelper)));

        //from now on we want to make the changes in a reference into the table.
        //The reason for inserting already is that we want the type to be there
        //in case it is used by any of the classes that we insert recursively below.
        assert( m_classes->find(tmpClass.m_typeId) != m_classes->end());
        ClassDescription & theClass = m_classes->find(tmpClass.m_typeId)->second;

        //set base class and descendants
        if (tmpClass.m_baseClassIndex < 0) //if we're Object
        {
            theClass.SetBaseClass(NULL);
        }
        else
        {
            //note the recursion
            ClassDescriptionPtr base = InsertClass(temporaryDescriptions, tmpClass.m_baseClassIndex, enums, allocHelper);
            theClass.SetBaseClass(base);
            base->AddDescendant(&theClass);
        }

        //add members
        for (DobMembers::const_iterator it = tmpClass.m_members.begin();
             it != tmpClass.m_members.end(); ++it)
        {
            //note the recursion
            theClass.AddMember(MemberDescription(it->m_name,
                                                 it->m_type,
                                                 it->m_arrayLength,
                                                 it->m_type == ObjectMemberType ? InsertClass(temporaryDescriptions,it->m_class,enums,allocHelper) : NULL,
                                                 it->m_type == EnumerationMemberType ? enums.FindEnum(it->m_objType) : NULL,
                                                 it->m_dataLength,
                                                 allocHelper));
        }

        return &theClass;
    }

}
}
}
}

