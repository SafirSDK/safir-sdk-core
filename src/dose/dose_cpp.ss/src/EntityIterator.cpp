/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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
#include <Safir/Dob/EntityIterator.h>

#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Dob/Internal/Interface.h>
#include "EntityProxyImpl.h"

namespace Safir
{
namespace Dob
{
    EntityIterator::EntityIterator():
        m_ctrl(-1),
        m_iteratorId(-1)
    {

    }

    EntityIterator::EntityIterator(const long ctrl,
                                   const Typesystem::TypeId typeId,
                                   const bool includeSubclasses):
        m_ctrl(ctrl),
        m_iteratorId(-1)
    {
        bool success;
        bool end;
        DoseC_EntityIteratorCreate(m_ctrl, typeId, includeSubclasses, m_iteratorId, end, success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }

        if (end)
        {
            *this = EntityIterator();
        }
    }


    EntityIterator::EntityIterator(const EntityIterator & other):
        m_ctrl(other.m_ctrl),
        m_iteratorId(-1)
    {
        if (other.m_iteratorId != -1)
        {
            bool success;
            DoseC_EntityIteratorCopy(m_ctrl, other.m_iteratorId, m_iteratorId, success);

            if (!success)
            {
                Typesystem::LibraryExceptions::Instance().Throw();
            }
        }
    }

    EntityIterator::~EntityIterator()
    {
        if (m_iteratorId != -1)
        {
            DoseC_EntityIteratorDestroy(m_ctrl, m_iteratorId);
            m_iteratorId = -1;
        }
    }

    EntityIterator & EntityIterator::operator=(const EntityIterator& other)
    {
        m_dereferenced.reset();

        if (m_iteratorId != -1)
        {
            DoseC_EntityIteratorDestroy(m_ctrl, m_iteratorId);
            m_iteratorId = -1;
        }
        m_ctrl = other.m_ctrl;
        if (other.m_iteratorId != -1)
        {
            bool success;
            DoseC_EntityIteratorCopy(m_ctrl, other.m_iteratorId, m_iteratorId, success);

            if (!success)
            {
                Typesystem::LibraryExceptions::Instance().Throw();
            }
        }
        return *this;
    }


    const EntityProxy& EntityIterator::dereference() const
    {
        if (m_iteratorId == -1)
        {
            throw Typesystem::SoftwareViolationException(L"This iterator appears to be at 'end', so dont try to dereference it!", __WFILE__,__LINE__);
        }

        if (m_dereferenced == NULL)
        {
            bool success;
            const char* entityBlob;
            const char* entityState;
            DoseC_EntityIteratorDereference(m_ctrl, m_iteratorId, entityBlob, entityState, success);

            if (!success)
            {
                Typesystem::LibraryExceptions::Instance().Throw();
            }
            m_dereferenced.reset(new EntityProxy
                (new Internal::EntityProxyImpl(entityBlob,
                                               entityState,
                                               NULL,
                                               NULL,
                                               true,  //addReference
                                               false))); //timestampDiff
        }

        return *m_dereferenced;
    }

    void EntityIterator::increment()
    {
        if (m_iteratorId == -1)
        {
            throw Typesystem::SoftwareViolationException(L"Cannot increment an 'end' iterator", __WFILE__,__LINE__);
        }

        m_dereferenced.reset();

        bool success;
        bool end;
        DoseC_EntityIteratorIncrement(m_ctrl, m_iteratorId, end, success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }

        if (end)
        {
            *this = EntityIterator();
        }
    }

    bool EntityIterator::equal(const EntityIterator& other) const
    {
        if (m_iteratorId == -1 && other.m_iteratorId == -1)
        {
            return true;
        }
        else if (m_iteratorId == -1 || other.m_iteratorId == -1)
        {
            return false;
        }

        if (m_ctrl != other.m_ctrl)
        {
            throw Typesystem::SoftwareViolationException(L"Cannot compare iterators from different connections!", __WFILE__,__LINE__);
        }

        bool success;
        bool equal;
        DoseC_EntityIteratorEqual(m_ctrl, m_iteratorId, other.m_iteratorId, equal, success);

        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
        return equal;
    }

}
}
