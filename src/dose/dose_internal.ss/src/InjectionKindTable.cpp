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
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/InjectionProperty.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/InjectionOverrideProperty.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    InjectionKindTable* InjectionKindTable::m_instance = NULL;

    InjectionKindTable& InjectionKindTable::Instance()
    {
        ENSURE(m_instance != NULL, << "InjectionKindTable::Instance was called before Initialize!!!");
        return *m_instance;
    }

    void InjectionKindTable::Initialize()
    {
        auto shmName = "INJECTIONKIND_TABLE" + Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix();

        m_instance = GetSharedMemory().find_or_construct<InjectionKindTable>(shmName.c_str())(private_constructor_t());
    }

    InjectionKind::Enumeration ReadInjectionKind(const Typesystem::TypeId typeId)
    {
        bool isInherited,hasProperty;

        Dob::Typesystem::Operations::HasProperty(typeId,Dob::InjectionOverrideProperty::ClassTypeId, hasProperty, isInherited);

        if (hasProperty && !isInherited) //Make sure we do not get an inherited override!
        {
            try
            {
                //unfortunately we have to create dummy object here to be able to read the property, even
                //though it is a constant
                Dob::Typesystem::ObjectPtr obj = Typesystem::ObjectFactory::Instance().CreateObject(typeId);
                return Dob::InjectionOverrideProperty::GetInjection(obj);
            }
            catch (const std::exception & exc)
            {
                std::wostringstream ostr;
                ostr << "Failed to read Property member 'Injection' of property "
                     << Dob::Typesystem::Operations::GetName(Dob::InjectionOverrideProperty::ClassTypeId)
                     << " for class "
                     << Dob::Typesystem::Operations::GetName(typeId)
                     << ". Got exception " << exc.what();
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
            }
        }
        else if (Dob::Typesystem::Operations::HasProperty(typeId,Dob::InjectionProperty::ClassTypeId))
        {
            try
            {
                Dob::Typesystem::ObjectPtr obj = Typesystem::ObjectFactory::Instance().CreateObject(typeId);
                return Dob::InjectionProperty::GetInjection(obj);
            }
            catch (const std::exception & exc)
            {
                std::wostringstream ostr;
                ostr << "Failed to read Property member 'Injection' of property "
                     << Dob::Typesystem::Operations::GetName(Dob::InjectionProperty::ClassTypeId)
                     << " for class "
                     << Dob::Typesystem::Operations::GetName(typeId)
                     << ". Got exception " << exc.what();
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
            }
        }


        return InjectionKind::None;
    }

    InjectionKindTable::InjectionKindTable(private_constructor_t)
    {
        Dob::Typesystem::TypeIdVector typeIds = Safir::Dob::Typesystem::Operations::GetAllTypeIds();

        for (Dob::Typesystem::TypeIdVector::iterator it = typeIds.begin();
             it != typeIds.end();
             ++it)
        {
            if (Dob::Typesystem::Operations::IsOfType(*it,Safir::Dob::Entity::ClassTypeId))
            {
                const InjectionKind::Enumeration kind = ReadInjectionKind(*it);
                if (kind != InjectionKind::None)
                {
                    lllout << "Injection " << InjectionKind::ToString(kind) << " for type " <<
                        Safir::Dob::Typesystem::Operations::GetName(*it) << std::endl;
                    m_injectionKinds.insert(std::make_pair(*it,kind));
                }
            }
        }
    }


    InjectionKind::Enumeration InjectionKindTable::GetInjectionKind(const Typesystem::TypeId typeId) const
    {
        const InjectionKinds::const_iterator findIt = m_injectionKinds.find(typeId);
        if (findIt == m_injectionKinds.end())
        {
            return InjectionKind::None;
        }
        else
        {
            return findIt->second;
        }
    }

    bool InjectionKindTable::IsNone(const Typesystem::TypeId typeId) const
    {
        return GetInjectionKind(typeId) == InjectionKind::None;

    }

    bool InjectionKindTable::IsSynchronousPermanent(const Typesystem::TypeId typeId) const
    {
        return GetInjectionKind(typeId) == InjectionKind::SynchronousPermanent;
    }

    bool InjectionKindTable::IsInjectable(const Typesystem::TypeId typeId) const
    {
        return GetInjectionKind(typeId) == InjectionKind::Injectable;
    }

}
}
}
