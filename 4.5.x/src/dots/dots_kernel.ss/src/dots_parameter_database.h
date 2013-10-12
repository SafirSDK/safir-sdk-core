/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
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

#ifndef _dots_const_database_h
#define _dots_const_database_h

#include "dots_internal_defs.h"
#include "dots_temporary_descriptions.h"
#include "dots_enum_database.h"
#include "dots_allocation_helper.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{

    /**
     * Handles storage and retrieval of all parameters in the system.
     */
    class ParameterDatabase
    {
    public:
        ParameterDatabase(create_and_initialize_t, const size_t parametersSize, AllocationHelper & allocHelper);
        ParameterDatabase(open_only_t, boost::interprocess::managed_shared_memory & shmem);

        ~ParameterDatabase();

        /**
         * Insert a parameter into the blob of parameters.
         *
         * @return Offset into the parameter blob where the inserted parameter starts.
         * @param param [in] The parameter to insert.
         * @param enumDb [in] The enumeration database, is needed if the param is an enum.
         * @param allocHelper [in,out] Allocation helper in case allocation has to be done.
         */
        const ParameterOffsetConst Insert(const DobParameter & param,
                                          const EnumDatabase & enumDb,
                                          AllocationHelper & allocHelper);

    private:
        void Ibool(const DobParameter& val, AllocationHelper & allocHelper);
        void Ienum(const DobParameter& val, const EnumDatabase & enumDb, AllocationHelper & allocHelper);
        void Iint32(const DobParameter& val, AllocationHelper & allocHelper);
        void Iint64(const DobParameter& val, AllocationHelper & allocHelper);
        void Ifloat32(const DobParameter& val, AllocationHelper & allocHelper);
        void Ifloat64(const DobParameter& val, AllocationHelper & allocHelper);
        void Itid(const DobParameter& val, AllocationHelper & allocHelper);
        void InsertHashedType(const DobParameter& val, AllocationHelper & allocHelper);
        void IentityId(const DobParameter& val, AllocationHelper & allocHelper);
        void Istring(const DobParameter& val, AllocationHelper & allocHelper);
        void Iobject(const DobParameter& val, AllocationHelper & allocHelper);
        void Ibinary(const DobParameter& val, AllocationHelper & allocHelper);

        void Error(const DobParameter& val, const size_t valueIndex, const std::string & description);

        void PrintOffset(const std::string & desc, const ParameterOffset & offset) const;


        /** The starting address of the memory block that holds all parameters. */
        void * m_parameterBlob;

        /** Points to the first free block in m_parameters. */
        ParameterOffset m_firstFree;
        //        size_t m_offset;


    };
}
}
}
}
#endif
