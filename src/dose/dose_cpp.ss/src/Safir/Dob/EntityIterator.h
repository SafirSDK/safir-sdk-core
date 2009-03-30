/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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

#ifndef _SAFIR_DOB_ENTITYITERATOR_H
#define _SAFIR_DOB_ENTITYITERATOR_H

#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/EntityProxy.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <boost/iterator/iterator_facade.hpp>

namespace Safir
{
namespace Dob
{
//#ifdef _MSC_VER
//#pragma warning(push)
//#pragma warning(disable: 4275)
//#endif

    /**
     * A STL conformant forward iterator used to traverse entity instances.
     */
    class EntityIterator :
        public boost::iterator_facade
        <
             EntityIterator,
             const EntityProxy,
             boost::single_pass_traversal_tag
        >
    {
    public:
        /** Constructs an "end" iterator
         */
        DOSE_CPP_API EntityIterator();

        /** Copy Constructor. */
        DOSE_CPP_API EntityIterator(const EntityIterator & other);

        /** Destructor.
         */
        DOSE_CPP_API ~EntityIterator();

        /** Copy assignment operator.*/
        DOSE_CPP_API EntityIterator & operator=(const EntityIterator& other);

        /**
         * The preincrement operator.
         *
         * Usage: ++it.
         *
         * Note that this iterator does not support postincrement, i.e. it++.
         * This using declaration is needed due to the disabling of the postincrement below.
         */
        using boost::iterator_facade<EntityIterator,
                                     const EntityProxy,
                                     boost::single_pass_traversal_tag>::operator++;
    private:
        friend class boost::iterator_core_access;
        friend class ConnectionBase;

        //Disable postincrement iterator
        //Use preincrement, i.e. ++it, instead.
        const EntityIterator operator++(int);

        DOSE_CPP_API EntityIterator(const long ctrl,
                                    const Typesystem::TypeId typeId,
                                    const bool includeSubclasses);

        DOSE_CPP_API const EntityProxy& dereference() const;
        DOSE_CPP_API void increment();
        DOSE_CPP_API bool equal(const EntityIterator& other) const;

        long m_ctrl;
        Safir::Dob::Typesystem::Int32 m_iteratorId;

        mutable boost::shared_ptr<EntityProxy> m_dereferenced;
    };
//#ifdef _MSC_VER
//#pragma warning(pop)
//#endif

}
}

#endif
