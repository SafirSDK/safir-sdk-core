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

#ifndef _dots_property_database_h
#define _dots_property_database_h

#include "dots_property_description.h"
#include "dots_temporary_descriptions.h"
#include "dots_fwd.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    typedef AllocationHelper::PairContainers<TypeId, PropertyDescription>::map PropertyTable;

    /**
     * Handles storage and retrieval of all properties in the system.
     */
    class PropertyDatabase:
        private boost::noncopyable
    {
    public:
        PropertyDatabase(create_and_initialize_t, AllocationHelper & allocHelper);
        PropertyDatabase(open_only_t, boost::interprocess::managed_shared_memory & shmem);

        ~PropertyDatabase();

        void Insert(const DobProperty & tmpProperty,
                    const ClassDatabase & classes,
                    const EnumDatabase & enums,
                    AllocationHelper & allocHelper);


        const PropertyDescription * FindProperty(const TypeId typeId) const;

        Int32 NumberOfProperties() const {return static_cast<Int32>(m_properties->size());}

        void GetTypeIds(TypeId * const buf,
                        const Int32 bufSize,
                        Int32 & resultSize) const;

        //Debug
        void Dump();

    private:
        //Private stuff
        PropertyTable* m_properties;
    };
}
}
}
}
#endif

