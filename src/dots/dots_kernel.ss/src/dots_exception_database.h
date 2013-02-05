/******************************************************************************
*
* Copyright Saab AB, 2009 (http://www.safirsdk.com)
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

#ifndef _dots_exception_database_h
#define _dots_exception_database_h

#include "dots_exception_description.h"
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
    typedef AllocationHelper::PairContainers<TypeId, ExceptionDescription>::map ExceptionTable;

    /**
     * Handles storage and retrieval of all enumeration types in the system.
     */
    class ExceptionDatabase:
        private boost::noncopyable
    {
    public:
        ExceptionDatabase(create_and_initialize_t, AllocationHelper & allocHelper);
        ExceptionDatabase(open_only_t, boost::interprocess::managed_shared_memory & shmem);

        ~ExceptionDatabase();

        void InsertExceptions(DobExceptions tempExceptions, AllocationHelper & allocHelper);

        /** Get the description of one type id */
        const ExceptionDescription * FindException(const TypeId typeId) const;

        /** Get the number of exceptions in the database*/
        Int32 NumberOfExceptions() const {return static_cast<Int32>(m_exceptions->size());}

        /**
         * Get all the type ids in the database.
         *
         * @param buf [in/out] array to put the typeIds in.
         * @param bufSize [in] size of buf. Should be the same as NumberOfExceptions if you want all typeIds.
         * @param resultSize [out] number of typeIds written to buf.
         */
        void GetTypeIds(TypeId * const buf,
                        const Int32 bufSize,
                        Int32 & resultSize) const;

    private:
        ExceptionTable * m_exceptions;
    };
}
}
}
}
#endif

