/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
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

#ifndef _dots_class_description_h
#define _dots_class_description_h

#include "dots_internal_defs.h"
#include "dots_member_description.h"
#include "dots_parameter_description.h"
#include "dots_property_mapping_description.h"
#include "dots_allocation_helper.h"
#include "dots_fwd.h"
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    class ClassDescription;
    typedef AllocationHelper::Containers<ClassDescriptionConstPtr>::vector ClassDescendants;

    /**
     * Contains information about a Dob-class specified in xml-files.
     */
    class ClassDescription
    {
    public:
        ClassDescription(const std::string & name,
                         const TypeId typeId,
                         const TypeId baseClassTypeId,
                         const Size initialSize,
                         const unsigned int thisClassSize,
                         const Size noInheritedMembers,
                         const Size noInheritedParameters,
                         const Size noMembers,
                         const Size noDescendants,
                         const Size noParameters,
                         const Size noProperties,
                         AllocationHelper & allocHelper);

        ~ClassDescription();

        const char * Name() const {return m_name.c_str();}

        TypeId GetTypeId() const {return m_typeId;}

        void SetBaseClass(const ClassDescriptionConstPtr & base) {m_baseClass = base;}
        const ClassDescriptionConstPtr BaseClass() const {return m_baseClass;}

        void AddDescendant(const ClassDescriptionConstPtr & descendant) 
        {
            for(ClassDescendants::iterator it = m_descendants.begin();
                it != m_descendants.end(); ++it)
            {
                if ((*it)->GetTypeId() > descendant->GetTypeId())
                {
                    m_descendants.insert(it,descendant);
                    return;
                }
            }

            //nope, insert it last.
            m_descendants.push_back(descendant);
        }
        const ClassDescendants & Descendants() const {return m_descendants;}

        //-------------------
        // Member functions

        void AddMember(const MemberDescription & member) {m_members.push_back(member);}
        const MemberDescription * GetMember(const MemberIndex member) const;
        const MemberDescription * GetOwnMember(const MemberIndex member) const;

        MemberIndex GetMemberIndexFromName(const std::string & name) const; //return -1 on failure

        Size NumberOfOwnMembers() const {return static_cast<Size>(m_members.size());}
        Size NumberOfInheritedMembers() const {return m_noInheritedMembers;}
        Size NumberOfMembers() const {return NumberOfInheritedMembers() + NumberOfOwnMembers();}


        //-------------------
        // Parameter functions

        void AddParameter(const ParameterDescription & parameter) {m_parameters.push_back(parameter);}
        const ParameterDescription * GetParameter(const ParameterIndex parameter) const;
        const ParameterDescription * GetOwnParameter(const ParameterIndex parameter) const;

        Size NumberOfOwnParameters() const {return static_cast<Size>(m_parameters.size());}
        Size NumberOfInheritedParameters() const {return m_noInheritedParameters;}
        Size NumberOfParameters() const {return NumberOfInheritedParameters() + NumberOfOwnParameters();}

        ParameterIndex GetParameterIndexFromName(const std::string & name) const; //return -1 on failure

        //-------------------
        // PropertyMapping functions

        void AddPropertyMapping(const PropertyMappingDescription & propertyMapping) {m_propertyMappings.push_back(propertyMapping);}
        //        const PropertyMappingDescription * GetPropertyMapping(const Size index) const;

        //returns NULL if property does not exist in class tree.
        const PropertyMappingDescription * FindPropertyMapping(const TypeId propertyType, bool & isInherited) const;

        Size NumberOfPropertyMappings() const {return static_cast<Size>(m_propertyMappings.size());}

        //-------------------
        // Blob size functions

        /** The size of the class without any dynamic members set. */
        Size InitialSize() const {return m_initialSize;}

        /** The size of only the members of this class, no inherited members.
         * Object (the base class provides the header size)
         */
        Size OwnSize() const {return m_thisClassSize;}


    private:
        ShmString m_name; //including namespace ex: MyNamespace1.MyNamespace2.MyClass
        ClassDescriptionConstPtr m_baseClass;

        TypeId m_typeId;
        Size m_initialSize; //size of data part in the blob
        unsigned int m_thisClassSize; //size of only the members in this class (object will get the header size)

        Size m_noInheritedMembers;
        MemberVector m_members;
        ClassDescendants m_descendants;
        Size m_noInheritedParameters;
        ParameterVector m_parameters;
        PropertyMappingVector m_propertyMappings;
    };
}
}
}
}
#endif
