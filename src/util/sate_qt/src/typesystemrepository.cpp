/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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
#include "typesystemrepository.h"
#include <Safir/Dob/Typesystem/Object.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/Response.h>
#include <Safir/Dob/Parametrization.h>
#include <Safir/Dob/Item.h>
#include <Safir/Dob/Struct.h>
#include <Safir/Dob/Typesystem/Members.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeUtilities.h>
#include <QDebug>

namespace
{
    void SetDobBaseClass(TypesystemRepository::DobClass* type, TypesystemRepository::DobBaseClass b)
    {
        type->dobBaseClass = b;
        for (auto c : type->children)
        {
            SetDobBaseClass(const_cast<TypesystemRepository::DobClass*>(c), b);
        }
    }

    void SetupMembers(TypesystemRepository::DobClass* type, int numInheritedMembers)
    {
        using namespace Safir::Dob::Typesystem::Members;
        type->totalNumberOfMembers = static_cast<size_t>(GetNumberOfMembers(type->typeId));
        for (auto m = numInheritedMembers; m < type->totalNumberOfMembers; ++m)
        {
            const char* name;
            TypesystemRepository::DobMember member;
            GetInfo(type->typeId, m, member.memberType, member.keyType, name, member.memberTypeId, member.keyTypeId, member.stringLength, member.collectionType, member.arrayLength);
            member.name = QString(name);
            type->members.push_back(member);
        }

        for (auto child : type->children)
        {
            SetupMembers(const_cast<TypesystemRepository::DobClass*>(child), type->totalNumberOfMembers);
        }
    }

    template<class C>
    void PrintHierarchy(const C* c, const QString& indent)
    {
        qDebug() << indent + c->name;
        for (auto child : c->children)
        {
            PrintHierarchy(child, indent + " ");
        }
    }
}

TypesystemRepository::TypesystemRepository()
{
    auto typeIds = Safir::Dob::Typesystem::Operations::GetClassTree(Safir::Dob::Typesystem::Object::ClassTypeId);

    // First insert all types in map
    for (auto t : typeIds)
    {
        auto it = m_classes.insert({t, std::make_unique<TypesystemRepository::DobClass>(t, QString::fromStdWString(Safir::Dob::Typesystem::Operations::GetName(t)))});
        auto cls = it.first->second.get();
        auto ns = CreateNamespace(cls->name);
        if (ns != nullptr)
        {
            ns->units.push_back(cls);
            cls->namespaze = ns;
        }
    }

    // ...then set-up parent-child relationships
    for (auto t : typeIds)
    {
        if (t != Safir::Dob::Typesystem::Object::ClassTypeId)
        {
            auto type = m_classes[t].get();
            auto parent = m_classes[Safir::Dob::Typesystem::Operations::GetParentType(t)].get();
            type->parent = parent;
            parent->children.push_back(type);
        }
    }

    // set category
    SetDobBaseClass(m_classes[Safir::Dob::Entity::ClassTypeId].get(), Entity);
    SetDobBaseClass(m_classes[Safir::Dob::Message::ClassTypeId].get(), Message);
    SetDobBaseClass(m_classes[Safir::Dob::Service::ClassTypeId].get(), Service);
    SetDobBaseClass(m_classes[Safir::Dob::Response::ClassTypeId].get(), Response);
    SetDobBaseClass(m_classes[Safir::Dob::Parametrization::ClassTypeId].get(), Parametrization);
    SetDobBaseClass(m_classes[Safir::Dob::Item::ClassTypeId].get(), Item);
    SetDobBaseClass(m_classes[Safir::Dob::Struct::ClassTypeId].get(), Struct);

    // setup enum types
    for (int64_t t : Safir::Dob::Typesystem::Operations::GetAllTypeIds())
    {
        if (Safir::Dob::Typesystem::Operations::IsEnumeration(t))
        {
            auto it = m_enums.insert({ t, std::make_unique<TypesystemRepository::DobEnum>(t, QString::fromStdWString(Safir::Dob::Typesystem::Operations::GetName(t))) });
            auto enumPtr = it.first->second.get();
            for (auto ev = 0; ev < Safir::Dob::Typesystem::Operations::GetNumberOfEnumerationValues(t); ++ev)
            {
                enumPtr->values.push_back(QString::fromStdWString(Safir::Dob::Typesystem::Operations::GetEnumerationValueName(t, ev)));
            }
            auto ns = CreateNamespace(enumPtr->name);
            if (ns != nullptr)
            {
                ns->units.push_back(enumPtr);
                enumPtr->namespaze = ns;
            }

            m_enumsSorted.push_back(enumPtr);
        }
    }

    // setup members
    SetupMembers(m_classes.at(Safir::Dob::Typesystem::Object::ClassTypeId).get(), 0);

    // setup root namespaces
    for (const auto& np : m_namespaces)
    {
        if (np.second->parent == nullptr)
        {
            m_rootNamespaces.push_back(np.second.get());
        }
    }

    // Sort result
    SortClasses(const_cast<DobClass*>(GetRootObject())->children);
    SortNamespaces(m_rootNamespaces);
    std::sort(m_enumsSorted.begin(), m_enumsSorted.end(), [](const DobEnum* a, const DobEnum* b){ return a->name < b->name;});
}

const TypesystemRepository& TypesystemRepository::Instance()
{
    static TypesystemRepository typeRepository;
    return typeRepository;
}

const TypesystemRepository::DobClass* TypesystemRepository::GetRootObject() const
{
    return GetClass(Safir::Dob::Typesystem::Object::ClassTypeId);
}

const TypesystemRepository::DobClass* TypesystemRepository::GetClass(int64_t typeId) const
{
    auto it = m_classes.find(typeId);
    return it != m_classes.end() ? it->second.get() : nullptr;
}

const TypesystemRepository::DobClass* TypesystemRepository::GetClass(const QString& name) const
{
    return GetClass(Safir::Dob::Typesystem::Operations::GetTypeId(name.toStdWString()));
}

const TypesystemRepository::DobEnum* TypesystemRepository::GetEnum(int64_t typeId) const
{
    auto it = m_enums.find(typeId);
    return it != m_enums.end() ? it->second.get() : nullptr;
}

const std::vector<const TypesystemRepository::DobEnum*>& TypesystemRepository::EnumsSorted() const
{
    return m_enumsSorted;
}

const TypesystemRepository::DobEnum* TypesystemRepository::GetEnum(const QString& name) const
{
    return GetEnum(Safir::Dob::Typesystem::Operations::GetTypeId(name.toStdWString()));
}

const std::vector<const TypesystemRepository::DobNamespace*>& TypesystemRepository::GetRootNamespaces() const
{
    return m_rootNamespaces;
}

const TypesystemRepository::DobNamespace* TypesystemRepository::GetNamespace(const QString& fullname) const
{
    auto it = m_namespaces.find(fullname);
    return it != m_namespaces.end() ? it->second.get() : nullptr;
}

TypesystemRepository::DobNamespace* TypesystemRepository::CreateNamespace(const QString& unitName)
{
    // The namespace we are looking for
    TypesystemRepository::DobNamespace* parentNamespace = nullptr;

    // Remove class name to a clean namespace string
    auto i = unitName.lastIndexOf(".");
    auto namespaceName = i > -1 ? unitName.mid(0, i) : QString("Safir.Dob.Typesystem");

    // Check if namespace already exists
    auto alreadyExistIt = m_namespaces.find(namespaceName);
    if (alreadyExistIt != m_namespaces.end())
    {
        parentNamespace = alreadyExistIt->second.get();
    }
    else
    {
        // Namespace doesn't exist, create all namespaces that are missing.
        auto namespaceList = namespaceName.split(".");
        for (int l = 0; l < namespaceList.length(); ++l)
        {
            auto fullName = namespaceList.mid(0, l + 1).join(".");
            auto it = m_namespaces.find(fullName);
            TypesystemRepository::DobNamespace* currentNs = nullptr;
            if (it != m_namespaces.end())
            {
                // already exists
                currentNs = it->second.get();
            }
            else
            {
                // new namespace that must be created
                auto newIt = m_namespaces.insert({fullName, std::make_unique<TypesystemRepository::DobNamespace>(fullName)});
                currentNs = newIt.first->second.get();

                if (parentNamespace != nullptr)
                {
                    currentNs->parent = parentNamespace;
                    parentNamespace->children.push_back(currentNs);
                }
            }

            parentNamespace = currentNs;
        }
    }

    return parentNamespace;
}

QString TypesystemRepository::GetTypeName(Safir::Dob::Typesystem::MemberType mt, int64_t typeId) const
{
    switch (mt)
    {
    case ObjectMemberType:
        return m_classes.at(typeId)->name;
    case EnumerationMemberType:
        return m_enums.at(typeId)->name;
    default:
        return QString(Safir::Dob::Typesystem::ToolSupport::TypeUtilities::GetTypeName(mt));
    }
}

void TypesystemRepository::DebugPrintClasses() const
{
    qDebug() << "Classes";
    qDebug() << "-------";
    PrintHierarchy(GetRootObject(), "");
}

void TypesystemRepository::DebugPrintNamespaces() const
{
    qDebug() << "Namespaces";
    qDebug() << "----------";
    for (auto ns : GetRootNamespaces())
    {
        PrintHierarchy(ns, "");
    }
}

void TypesystemRepository::SortClasses(std::vector<const DobClass*>& cls)
{
    std::sort(cls.begin(), cls.end(), [](const DobClass* a, const DobClass* b){ return a->name < b->name;});

    for (auto child : cls)
    {
        SortClasses(const_cast<DobClass*>(child)->children);
    }
}

void TypesystemRepository::SortNamespaces(std::vector<const DobNamespace*>& ns)
{
    std::sort(ns.begin(), ns.end(), [](const DobNamespace* a, const DobNamespace* b){ return a->name < b->name;});

    for (auto subNamespace : ns)
    {
        auto sn = const_cast<DobNamespace*>(subNamespace);
        std::sort(sn->units.begin(), sn->units.end(), [](const DobUnit* a, const DobUnit* b){ return a->name < b->name;});
        SortNamespaces(sn->children);
    }
}
