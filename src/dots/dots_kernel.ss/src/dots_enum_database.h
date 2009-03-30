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

#ifndef _dots_enum_database_h
#define _dots_enum_database_h

#include "dots_enum_description.h"
#include "dots_temporary_descriptions.h"
#include "dots_allocation_helper.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    typedef AllocationHelper::PairContainers<TypeId, EnumDescription>::map EnumTable;

    /**
     * Handles storage and retrieval of all enumeration types in the system.
     */
    class EnumDatabase:
        private boost::noncopyable
    {
    public:
        EnumDatabase(create_and_initialize_t, AllocationHelper & allocHelper);
        EnumDatabase(open_only_t, boost::interprocess::managed_shared_memory & shmem);

        ~EnumDatabase();

        void Insert(const DobEnumeration & tempEnum, AllocationHelper & allocHelper);

        /** Get the description of one type id */
        const EnumDescription * FindEnum(const TypeId typeId) const;

        /** Get the number of enumerations in the database */
        Int32 NumberOfEnums() const {return static_cast<Int32>(m_enums->size());}

        /**
         * Get all the type ids in the database.
         *
         * @param buf [in/out] array to put the typeIds in.
         * @param bufSize [in] size of buf. Should be the same as GetNumberOfEnums if you want all typeIds.
         * @param resultSize [out] number of typeIds written to buf.
         */
        void GetTypeIds(TypeId * const buf,
                        const Int32 bufSize,
                        Int32 & resultSize) const;

        //        void Dump() const;
    private:
        EnumTable * m_enums;
    };
}
}
}
}
#endif

