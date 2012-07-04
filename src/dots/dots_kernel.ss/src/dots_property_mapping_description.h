/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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

#ifndef _dots_property_mapping_description_h
#define _dots_property_mapping_description_h

#include "dots_internal_defs.h"
#include "dots_parameter_description.h"
#include <boost/static_assert.hpp>
#include "dots_fwd.h"
#include "dots_allocation_helper.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{

    struct MemberReferenceElement
    {
        MemberIndex m_classMember;
        ArrayIndex m_index; //-1 means "point to whole array"

        MemberReferenceElement(MemberIndex member, ArrayIndex index):m_classMember(member),m_index(index) {}
        //        MemberReferenceElement():m_classMember(-1),m_index(-1) {}
    };


    typedef AllocationHelper::Containers<MemberReferenceElement>::vector ClassMemberReference;
    typedef boost::interprocess::offset_ptr<ClassMemberReference> ClassMemberReferencePtr;

    //Type definitions for use in class PropertyMappingDescription

    class MemberMapping
    {
    public:

        //create a MappedToNull mapping
        MemberMapping();

        //create a MappedToMember mapping
        MemberMapping(const Size depth, AllocationHelper & allocHelper);

        //create a MappedToParameter mapping
        MemberMapping(const ParameterDescription & parameter, AllocationHelper & allocHelper);

        MemberMapping(const MemberMapping& other);
        MemberMapping& operator=(const MemberMapping& other);


        DotsC_PropertyMappingKind GetMappingKind() const {return m_mappingKind;}

        void AddMemberReferenceLevel(const MemberIndex memberIndex, const ArrayIndex arrayIndex);
        const ClassMemberReference * GetMemberReference() const;

        const ParameterDescription * GetParameter() const;

    private:
        DotsC_PropertyMappingKind m_mappingKind;
        AllocationHelper::SmartPointers<ClassMemberReference>::shared_ptr m_classMemberReference;
        AllocationHelper::SmartPointers<ParameterDescription>::shared_ptr m_parameter;
    };


    typedef AllocationHelper::Containers<MemberMapping>::vector MemberMappingVector;

    //The class PropertyMappingDescription
    class PropertyMappingDescription
    {
    public:
        PropertyMappingDescription(const Size noMembers, //used to reserve space in shmem
                                   const PropertyDescriptionConstPtr & property,
                                   AllocationHelper & allocHelper);

        PropertyMappingDescription(const PropertyMappingDescription& other);
        PropertyMappingDescription& operator=(const PropertyMappingDescription& other);

        ~PropertyMappingDescription();

        const PropertyDescriptionConstPtr & Property() const {return m_property;}
        //        const MemberMappingVector & Mappings() const {return m_mappings;}

        MemberMapping & AddMapping(const MemberMapping & memberMapping) {m_mappings.push_back(memberMapping); return m_mappings.back();}
        const MemberMapping * GetMemberMapping(const Size index) const {return &m_mappings[index];}

        Size NumberOfMappings() const {return static_cast<Size>(m_mappings.size());}

    private:
        PropertyDescriptionConstPtr m_property;
        MemberMappingVector m_mappings;
    };

    //Collection type for PropertyMappingDescription
    typedef AllocationHelper::Containers<PropertyMappingDescription>::vector PropertyMappingVector;
}
}
}
}
#endif
