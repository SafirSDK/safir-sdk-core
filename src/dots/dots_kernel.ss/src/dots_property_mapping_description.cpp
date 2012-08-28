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

#include "dots_property_mapping_description.h"
#include "dots_repository.h"


namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    namespace //check size of memberreferenceelement (we return a vector of them as an int[] to the applications)
    {
        BOOST_STATIC_ASSERT(sizeof(MemberReferenceElement) == 2*(sizeof(DotsC_Int32)));
    }



    PropertyMappingDescription::PropertyMappingDescription(const Size noMembers, //used to reserve space in shmem
                                                           const PropertyDescriptionConstPtr & property,
                                                           AllocationHelper & allocHelper):
        m_property(property),
        m_mappings(allocHelper.GetAllocator<MemberMapping>())
    {
        m_mappings.reserve(noMembers);
    }

    PropertyMappingDescription::~PropertyMappingDescription()
    {

    }

    PropertyMappingDescription::PropertyMappingDescription(const PropertyMappingDescription& other):
        m_property(other.m_property),
        m_mappings(other.m_mappings)
    {
    }

    PropertyMappingDescription& PropertyMappingDescription::operator=(const PropertyMappingDescription& other)
    {
        m_property = other.m_property;
        m_mappings = other.m_mappings;
        return *this;
    }

    MemberMapping::MemberMapping():
        m_mappingKind(MappedToNull),
        m_classMemberReference(),
        m_parameter()
    {
    }

    MemberMapping::MemberMapping(const Size depth,
                                 AllocationHelper & allocHelper):
        m_mappingKind(MappedToMember),
        m_parameter()
    {
        //allocate an anonymous instance of a ClassMemberReference in shared memory
        m_classMemberReference = boost::interprocess::make_managed_shared_ptr
            (allocHelper.GetShmem()->construct<ClassMemberReference>(boost::interprocess::anonymous_instance)
             (allocHelper.GetAllocator<MemberReferenceElement>()),
             *allocHelper.GetShmem());
        m_classMemberReference->reserve(depth);
    }

    MemberMapping::MemberMapping(const MemberMapping& other):
        m_mappingKind(other.m_mappingKind),
        m_classMemberReference(other.m_classMemberReference),
        m_parameter(other.m_parameter)
    {
    }

    MemberMapping& MemberMapping::operator=(const MemberMapping& other)
    {
        m_mappingKind = other.m_mappingKind;
        m_classMemberReference = other.m_classMemberReference;
        m_parameter = other.m_parameter;
        return *this;
    }



    MemberMapping::MemberMapping(const ParameterDescription & parameter,
                                 AllocationHelper & allocHelper):
        m_mappingKind(MappedToParameter),
        m_classMemberReference()
    {
        m_parameter = boost::interprocess::make_managed_shared_ptr
            (allocHelper.GetShmem()->construct<ParameterDescription>(boost::interprocess::anonymous_instance)
             (parameter),
             *allocHelper.GetShmem());
    }


    void MemberMapping::AddMemberReferenceLevel(const MemberIndex memberIndex, const ArrayIndex arrayIndex)
    {
        ENSURE(m_mappingKind == MappedToMember, << "Someone called AddMemberReferenceLevel on a MemberMapping of kind " << m_mappingKind);
        m_classMemberReference->push_back(MemberReferenceElement(memberIndex,arrayIndex));
    }


    const ClassMemberReference * MemberMapping::GetMemberReference() const
    {
        ENSURE(m_mappingKind == MappedToMember, << "Someone called GetMemberReference on a MemberMapping of kind " << m_mappingKind);
        return m_classMemberReference.get().get();
    }

    const ParameterDescription * MemberMapping::GetParameter() const
    {
        ENSURE(m_mappingKind == MappedToParameter, << "Someone called GetParameter on a MemberMapping of kind " << m_mappingKind);
        return m_parameter.get().get();
    }

}
}
}
}
