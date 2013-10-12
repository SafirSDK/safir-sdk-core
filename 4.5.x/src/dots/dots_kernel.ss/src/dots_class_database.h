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

#ifndef _dots_class_database_h
#define _dots_class_database_h

#include "dots_class_description.h"
#include "dots_fwd.h"
#include "dots_temporary_descriptions.h"
#include <boost/thread/mutex.hpp>
#include <boost/thread/locks.hpp>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    typedef AllocationHelper::PairContainers<TypeId, ClassDescription>::map ClassTable;

    /**
     * Handles all class descriptions of DOB objects.
     * Provides functions for searching for specific class definitions.
     */
    class ClassDatabase:
        private boost::noncopyable
    {
    public:
        //Constructor
        ClassDatabase(create_and_initialize_t, AllocationHelper & allocHelper);
        ClassDatabase(open_only_t, boost::interprocess::managed_shared_memory & shmem);

        ~ClassDatabase();

        /**
         * Adds all classes to the database.
         */
        void InsertClasses(const DobClasses & temporaryDescriptions,
                           const EnumDatabase & enums,
                           AllocationHelper & allocHelper);

        ClassDescription * FindClass(const TypeId typeId);
        const ClassDescription * FindClass(const TypeId typeId) const;

        Size NumberOfClasses() const {return static_cast<Size>(m_classes->size());}

        bool IsOfType(const TypeId theType, const TypeId ofType) const;

        void GetCompleteType(const TypeId typeId,
                             TypeId * buf,
                             const Int32 bufSize,
                             Int32 & noResults) const;

        void GetTypeIds(TypeId * const buf,
                        const Int32 bufSize,
                        Int32 & resultSize) const;

        //Debug
        //void Dump();


    private:
        const TypeId m_objectClassTypeId; //calculate this once and save it since it's used so often
        //        void DumpDescendants(ClassDescription* cde);

        static void GetCompleteTypeInternal(const ClassDescription * const cde,
                                            TypeId * buf,
                                            const Int32 bufSize,
                                            Int32 & noResults);

        /**
         * Add a class, its parent (recursively) and all object member classes (recursively).
         *
         * This method is called once for every temporary class description, and adds
         * the classes to the database if they have not already been added.
         */
        ClassDescriptionPtr InsertClass(const DobClasses & temporaryDescriptions, const size_t which, const EnumDatabase & enums, AllocationHelper & allocHelper);

        ClassTable * m_classes;

        void InitCache();

        boost::mutex        m_mutex;
        typedef std::pair<TypeId, ClassDescription*>    Item;
        typedef std::vector<Item>                       CachedTypeIds;

        CachedTypeIds   m_cachedTypeIds;
        int             m_lastUsedIdx;
        int             m_lastInsertedIdx;
    };
}
}
}
}
#endif
