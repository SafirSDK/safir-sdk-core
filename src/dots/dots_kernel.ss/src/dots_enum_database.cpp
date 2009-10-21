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

#include "dots_enum_database.h"
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
    static const char* ENUM_DATABASE_NAME = "ENUM_DATABASE";


    EnumDatabase::EnumDatabase(create_and_initialize_t, AllocationHelper & allocHelper)
    {
        m_enums = allocHelper.GetShmem()->construct<EnumTable>(ENUM_DATABASE_NAME)
            (std::less<TypeId>(),allocHelper.GetAllocator<EnumTable>());
    }

    EnumDatabase::EnumDatabase(open_only_t,boost::interprocess::managed_shared_memory & shmem)
    {
        m_enums = shmem.find<EnumTable>(ENUM_DATABASE_NAME).first;
        if (m_enums == NULL)
        {
            throw InternalException("Failed to open ENUM_DATABASE",__FILE__,__LINE__);
        }
    }

    EnumDatabase::~EnumDatabase()
    {
    }

    const EnumDescription * EnumDatabase::FindEnum(const TypeId typeId) const
    {
        EnumTable::const_iterator findIt = m_enums->find(typeId);
        if (findIt != m_enums->end())
        {
            return &findIt->second;
        }
        else
        {
            return NULL;
        }
    }

    void EnumDatabase::GetTypeIds(TypeId * const buf,
                                  const Int32 bufSize,
                                  Int32& resultSize) const
    {
        resultSize=0;
        for (EnumTable::const_iterator it = m_enums->begin();
             it != m_enums->end(); ++it)
        {
            //ignore the checksums.
            if (it->first == it->second.Checksum())
            {
                continue;
            }

            buf[resultSize] = it->first;
            ++resultSize;

            if (resultSize == bufSize)
            {
                break;
            }
        }
    }

    void EnumDatabase::Insert(const DobEnumeration & tempEnum, AllocationHelper & allocHelper)
    {
        lllout << "About to insert enumeration " << tempEnum.m_name.c_str() << " into the enumeration database" << std::endl;
        //TODO: This will result in duplicate memory! Maybe it should be a pointer instead, so that we can get the same
        //info from two points in the table
        m_enums->insert(std::make_pair(tempEnum.m_typeId, EnumDescription(tempEnum.m_name,
                                                                          tempEnum.m_typeId,
                                                                          tempEnum.m_checkSum,
                                                                          tempEnum.m_values,
                                                                          allocHelper)));

        //insert an extra, using checksum instead of typeid
        m_enums->insert(std::make_pair(tempEnum.m_checkSum, EnumDescription(tempEnum.m_name,
                                                                            tempEnum.m_typeId,
                                                                            tempEnum.m_checkSum,
                                                                            tempEnum.m_values,
                                                                            allocHelper)));

    }
    /*
    void EnumDatabase::Dump() const
    {
        for (EnumTable::iterator it = m_enums->begin();
             it != m_enums->end(); ++it)
        {
            it->second.Dump();
            EnumDescription & ede=it->second;
            std::wcout<<"enum "<<ede.Name()<<", (typeId="<<ede.GetTypeId()<<std::endl;
            std::wcout<<"   Values: "<<std::endl;
            for (int i=0; i<ede.m_values.GetSize(); i++)
            {
                std::wcout<<"\t"<<Repository::m_pool.RebasePointer<char>(ede.m_values.Get(&Repository::m_pool, i))<<std::endl;
                }
        }
        }*/
}
}
}
}
