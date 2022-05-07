/******************************************************************************
*
* Copyright Saab AB, 2022 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#pragma once

#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/EntityProxy.h>
#include <Safir/Dob/Typesystem/Defs.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_CPP_API EntityIteratorImpl
    {
    public:
        EntityIteratorImpl();
        EntityIteratorImpl(const long ctrl,
                           const Typesystem::TypeId typeId,
                           const bool includeSubclasses);

        EntityIteratorImpl(const EntityIteratorImpl & other);
        ~EntityIteratorImpl();
        EntityIteratorImpl & operator=(const EntityIteratorImpl& other);



        const EntityProxy& dereference() const;
        void increment();
        bool equal(const EntityIteratorImpl& other) const;

    private:
        long m_ctrl;
        Safir::Dob::Typesystem::Int32 m_iteratorId;

        mutable std::shared_ptr<EntityProxy> m_dereferenced;
    };
}
}
}



