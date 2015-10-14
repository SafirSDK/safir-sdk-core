/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safirsdkcore.com)
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
#include <Safir/Dob/Internal/ContextSharedTable.h>
#include <Safir/Dob/ContextSharedProperty.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/ContextSharedOverrideProperty.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    ContextSharedTable* ContextSharedTable::m_instance = NULL;

    ContextSharedTable& ContextSharedTable::Instance()
    {
        ENSURE(m_instance != NULL, << "ContextSharedTable::Instance was called before Initialize!!!");
        return *m_instance;
    }

    void ContextSharedTable::Initialize()
    {
        m_instance = GetSharedMemory().find_or_construct<ContextSharedTable>("CONTEXTSHARED_TABLE")(private_constructor_t());
    }

    bool ReadIsContextShared(const Typesystem::TypeId typeId)
    {
        bool isInherited,hasProperty;

        Dob::Typesystem::Operations::HasProperty(typeId,Dob::ContextSharedOverrideProperty::ClassTypeId, hasProperty, isInherited);

        if (hasProperty && !isInherited) //Make sure we do not get an inherited override!
        {
            try
            {
                //unfortunately we have to create dummy object here to be able to read the property, even
                //though it is a constant
                Dob::Typesystem::ObjectPtr obj = Typesystem::ObjectFactory::Instance().CreateObject(typeId);
                return Dob::ContextSharedOverrideProperty::GetContextShared(obj);
            }
            catch (const std::exception & exc)
            {
                std::wostringstream ostr;
                ostr << "Failed to read Property member 'ContextShared' of property "
                     << Dob::Typesystem::Operations::GetName(Dob::ContextSharedOverrideProperty::ClassTypeId)
                     << " for class "
                     << Dob::Typesystem::Operations::GetName(typeId)
                     << ". Got exception " << exc.what();
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
            }
        }
        else if (Dob::Typesystem::Operations::HasProperty(typeId,Dob::ContextSharedProperty::ClassTypeId))
        {
            try
            {
                Dob::Typesystem::ObjectPtr obj = Typesystem::ObjectFactory::Instance().CreateObject(typeId);
                return Dob::ContextSharedProperty::GetContextShared(obj);
            }
            catch (const std::exception & exc)
            {
                std::wostringstream ostr;
                ostr << "Failed to read Property member 'ContextShared' of property "
                     << Dob::Typesystem::Operations::GetName(Dob::ContextSharedProperty::ClassTypeId)
                     << " for class "
                     << Dob::Typesystem::Operations::GetName(typeId)
                     << ". Got exception " << exc.what();
                throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
            }
        }

        return false;

    }

    ContextSharedTable::ContextSharedTable(private_constructor_t)
    {
        Dob::Typesystem::TypeIdVector typeIds = Safir::Dob::Typesystem::Operations::GetAllTypeIds();

        for (Dob::Typesystem::TypeIdVector::iterator it = typeIds.begin();
             it != typeIds.end();
             ++it)
        {
            if (Dob::Typesystem::Operations::IsOfType(*it,Safir::Dob::Entity::ClassTypeId) ||
                Dob::Typesystem::Operations::IsOfType(*it,Safir::Dob::Service::ClassTypeId) ||
                Dob::Typesystem::Operations::IsOfType(*it,Safir::Dob::Message::ClassTypeId))
            {
                if (ReadIsContextShared(*it))
                {
                    lllout << "Type " << Safir::Dob::Typesystem::Operations::GetName(*it)
                        << " is ContextShared." << std::endl;
                    m_contextShared.insert(*it);
                }
            }
        }
    }

    bool ContextSharedTable::IsContextShared(const Typesystem::TypeId typeId) const
    {
        const ContextShared::const_iterator findIt = m_contextShared.find(typeId);

        return findIt != m_contextShared.end();
    }
}
}
}
