/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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

#include "dots_property_database.h"
#include "dots_repository.h"
#include "dots_basic_types.h"

#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    static const char* PROPERTY_DATABASE_NAME = "PROPERTY_DATABASE";

    PropertyDatabase::PropertyDatabase(create_and_initialize_t, AllocationHelper & allocHelper)
    {
        m_properties = allocHelper.GetShmem()->construct<PropertyTable>(PROPERTY_DATABASE_NAME)
            (std::less<TypeId>(),allocHelper.GetAllocator<PropertyTable>());

    }


    PropertyDatabase::PropertyDatabase(open_only_t, boost::interprocess::managed_shared_memory & shmem)
    {
        m_properties = shmem.find<PropertyTable>(PROPERTY_DATABASE_NAME).first;
        if (m_properties == NULL)
        {
            throw InternalException("Failed to open PROPERTY_DATABASE",__FILE__,__LINE__);
        }
    }


    PropertyDatabase::~PropertyDatabase()
    {

    }


    const PropertyDescription *
    PropertyDatabase::FindProperty(const TypeId typeId) const
    {
        PropertyTable::const_iterator findIt = m_properties->find(typeId);
        if (findIt == m_properties->end())
        {
            return NULL;
        }
        return &findIt->second;
    }

    void
    PropertyDatabase::GetTypeIds(TypeId * const buf,
                                 const Int32 bufSize,
                                 Int32 & resultSize) const
    {
        resultSize=0;
        for (PropertyTable::const_iterator it = m_properties->begin();
             it != m_properties->end(); ++it)
        {
            buf[resultSize] = it->first;
            ++resultSize;

            if (resultSize > bufSize)
            {
                break;
            }
        }
    }



    /*


    void PropertyDatabase::Dump()
    {
        adty::HashtableIterator<PropertyDescription> pit=m_properties->GetIterator();
        while(pit.HasNext(&Repository::m_pool))
        {
            PropertyDescription& pde=pit.Next(&Repository::m_pool);
            std::wcout<<std::endl;
            std::wcout<<"property "<<pde.Name()<<std::endl;

            std::wcout<<"   typeId: "<<pde.m_typeId<<""<<std::endl;
            std::wcout<<"   Elements:"<<std::endl;
            for (int i=0; i<pde.m_members.GetSize(); i++)
            {
                MemberDescription& cel=pde.m_members.Get(&Repository::m_pool,i);
                if (cel.m_type==ObjectMemberType)
                {
                    std::wcout<<"\t"<<cel.Class()->Name();
                }
                else
                {
                    std::wcout<<"\t"<<Safir::Dob::Typesystem::Internal::BasicTypes::StringOf(cel.m_type);
                }
                std::wcout<<" "<<cel.Name()<<std::endl;
            }
        }
        }*/



    void PropertyDatabase::Insert(const DobProperty & tmpProperty,
                                  const ClassDatabase & classes,
                                  const EnumDatabase & enums,
                                  AllocationHelper & allocHelper)
                                                    //DobProperties & d, size_t i
    {
        lllout << "About to insert property " << tmpProperty.m_name.c_str() << " into the property database" << std::endl;
        m_properties->insert(std::make_pair(tmpProperty.m_typeId,
                                            PropertyDescription(tmpProperty.m_name,
                                                                tmpProperty.m_typeId,
                                                                static_cast<Size>(tmpProperty.m_members.size()),
                                                                allocHelper)));

        //from now on we want to make the changes in a reference into the table.
        assert(m_properties->find(tmpProperty.m_typeId) != m_properties->end());
        PropertyDescription & theProperty = m_properties->find(tmpProperty.m_typeId)->second;

        for (DobMembers::const_iterator it = tmpProperty.m_members.begin();
             it != tmpProperty.m_members.end(); ++it)
        {
            theProperty.AddMember(MemberDescription(it->m_name,
                                                    it->m_type,
                                                    0,
                                                    it->m_type == ObjectMemberType ? classes.FindClass(it->m_objType) : NULL,
                                                    it->m_type == EnumerationMemberType ? enums.FindEnum(it->m_objType) : NULL,
                                                    it->m_dataLength,
                                                    allocHelper));

        }
    }

}
}
}
}
