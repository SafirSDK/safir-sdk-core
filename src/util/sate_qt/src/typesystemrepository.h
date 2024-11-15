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
#pragma once

#include <cstdint>
#include <vector>
#include <memory>
#include <map>
#include <QString>
#include <QList>
#include <Safir/Dob/Typesystem/Operations.h>

class TypesystemRepository
{
public:

    enum DobCategory { Class, Member, Enum, Namespace };
    enum DobBaseClass { Entity, Message, Service, Response, Parametrization, Item, Struct, Object };

    static const int DobBaseClassRole = 1000;
    static const int DobTypeIdRole = 1001;

    class DobUnit
    {
    public:
        DobCategory category;
        QString name;
        DobUnit(DobCategory c) : category(c) {}
        DobUnit(DobCategory c, const QString& n) : category(c), name(n) {}
    };

    class DobNamespace; // Forward declare


    class DobEnum : public DobUnit
    {
    public:
        int64_t typeId = -1;
        std::vector<QString> values;
        const DobNamespace* namespaze = nullptr;
        DobEnum() : DobUnit(Enum) {}
        DobEnum(int64_t t, const QString& n) : DobUnit(Enum, n), typeId(t) {}
        QString ToString() const
        {
            QString s = name + " {\n";
            for (const auto& v : values)
            {
                s += v + ", ";
            }
            s += "\n}";
            return s;
        }
    };

    class DobMember : public DobUnit
    {
    public:
        Safir::Dob::Typesystem::MemberType memberType = Int32MemberType;
        Safir::Dob::Typesystem::MemberType keyType = Int32MemberType;
        int64_t memberTypeId = -1;
        int64_t keyTypeId = -1;
        Safir::Dob::Typesystem::CollectionType collectionType = SingleValueCollectionType;
        int stringLength = -1;
        int arrayLength = -1;
        DobMember() : DobUnit(Member) {}
    };



    class DobClass : public DobUnit
    {
    public:
        DobBaseClass dobBaseClass = Object;
        int64_t typeId = -1;
        std::vector<DobMember> members;
        int totalNumberOfMembers = 0;

        const DobMember& GetMember(int index) const
        {
            auto start = totalNumberOfMembers - static_cast<int>(members.size());
            if (index < start)
            {
                return parent->GetMember(index);
            }
            return members[static_cast<size_t>(index - start)];
        }

        const DobClass* parent = nullptr;
        std::vector<const DobClass*> children;
        const DobNamespace* namespaze = nullptr;

        DobClass() : DobUnit(Class) {}
        DobClass(int64_t t, const QString& n) : DobUnit(Class, n), typeId(t) {}
    };

    class DobNamespace : public DobUnit
    {
    public:
        QString fullName;
        const DobNamespace* parent = nullptr;
        std::vector<const DobNamespace*> children;
        std::vector<const DobUnit*> units;

        DobNamespace() : DobUnit(Namespace) {}
        DobNamespace(const QString& full) : DobUnit(Namespace, full.split(".").last()), fullName(full) {}
    };

    static const TypesystemRepository& Instance();

    const DobClass* GetRootObject() const;
    const DobClass* GetClass(int64_t typeId) const;
    const DobClass* GetClass(const QString& name) const;

    const DobEnum* GetEnum(int64_t typeId) const;
    const DobEnum* GetEnum(const QString& name) const;
    const std::vector<const DobEnum*>& EnumsSorted() const;

    const std::vector<const DobNamespace*>& GetRootNamespaces() const;
    const DobNamespace* GetNamespace(const QString& fullname) const;

    QString GetTypeName(Safir::Dob::Typesystem::MemberType mt, int64_t typeId) const;

    void DebugPrintClasses() const;
    void DebugPrintNamespaces() const;

private:
    TypesystemRepository();
    TypesystemRepository(const TypesystemRepository&) = delete;
    TypesystemRepository(TypesystemRepository&&) = delete;

    std::map<int64_t, std::unique_ptr<DobClass>> m_classes;
    std::map<int64_t, std::unique_ptr<DobEnum>> m_enums;
    std::map<QString, std::unique_ptr<DobNamespace>> m_namespaces;
    std::vector<const DobNamespace*> m_rootNamespaces;
    std::vector<const DobEnum*> m_enumsSorted;

    void SortClasses(std::vector<const DobClass*>& cls);
    void SortNamespaces(std::vector<const DobNamespace*>& ns);

    DobNamespace* CreateNamespace(const QString& unitName);
};
