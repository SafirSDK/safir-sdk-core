/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
#ifndef __DOSE_INJECTION_KIND_TABLE_H__
#define __DOSE_INJECTION_KIND_TABLE_H__

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/InjectionKind.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API InjectionKindTable:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    private:
        //This is to make sure that only Instance can call the constructor even though the constructor
        //itself has to be public (limitation of boost::interprocess)
        struct private_constructor_t {};
    public:
        static void Initialize();

        static InjectionKindTable& Instance();

        InjectionKind::Enumeration GetInjectionKind(const Typesystem::TypeId typeId) const;

        /** Return true if injectionKind is None. */
        bool IsNone(const Typesystem::TypeId typeId) const;

        /** Return true if injectionKind is SynchronousPermanent. */
        bool IsSynchronousPermanent(const Typesystem::TypeId typeId) const;

        /** Return true if injectionKind is Injectable. */
        bool IsInjectable(const Typesystem::TypeId typeId) const;

        //The constructor and destructor have to be public for the boost::interprocess internals to be able to call
        //them, but we can make the constructor "fake-private" by making it require a private type as argument.
        explicit InjectionKindTable(private_constructor_t);
    private:
        typedef PairContainers<Typesystem::TypeId, InjectionKind::Enumeration>::map InjectionKinds;

        InjectionKinds m_injectionKinds;

        static InjectionKindTable* m_instance;
    };
}
}
}

#endif

