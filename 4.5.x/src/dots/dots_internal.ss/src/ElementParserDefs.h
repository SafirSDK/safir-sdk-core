/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
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
#ifndef __DOTS_INTERNAL_ELEMENT_PARSER_DEFS_H__
#define __DOTS_INTERNAL_ELEMENT_PARSER_DEFS_H__

#include "ElementParserBase.h"

// This file defines the schema for dou- and dom-files.
// It defines wich combinations of elements that are valid, and it specifies if elements are mandatory or
// optional, how many times it may occur and so on.

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{    
    namespace
    {
        //DOU file elements
        typedef Element<ElementNames::Classsummary, OptionalOne> Classsummary;
        typedef Element<ElementNames::ClassName, One> ClassName;
        typedef Element<ElementNames::BaseClass, One > BaseClass;

        typedef Element<ElementNames::Exceptionsummary, OptionalOne> Exceptionsummary;
        typedef Element<ElementNames::ExceptionName, One> ExceptionName;
        typedef Element<ElementNames::ExceptionBase, One > ExceptionBase;

        typedef Element<ElementNames::Enumerationsummary, OptionalOne> Enumerationsummary;
        typedef Element<ElementNames::EnumerationName, One> EnumerationName;
        typedef Element<ElementNames::EnumerationValue, AtLeastOne> EnumerationValue;
        typedef Element<ElementNames::EnumerationValues, One, boost::mpl::vector<EnumerationValue> > EnumerationValues;

        typedef Element<ElementNames::Propertysummary, OptionalOne> Propertysummary;
        typedef Element<ElementNames::PropertyName, One> PropertyName;
        typedef Element<ElementNames::PropertyMembersummary, OptionalOne> PropertyMembersummary;
        typedef Element<ElementNames::PropertyMemberName, One> PropertyMemberName;
        typedef Element<ElementNames::PropertyMemberisArray, OptionalOne> PropertyMemberisArray;
        typedef Element<ElementNames::PropertyMemberType, One> PropertyMemberType;
        typedef Element<ElementNames::PropertyMember, AtLeastOne, boost::mpl::vector<PropertyMembersummary, PropertyMemberName, PropertyMemberType, PropertyMemberisArray> > PropertyMember;
        typedef Element<ElementNames::PropertyMembers, OptionalOne, boost::mpl::vector<PropertyMember> > PropertyMembers;

        typedef Element<ElementNames::ArraySize, OptionalOne > ArraySize;
        typedef Element<ElementNames::ArraySizeRef, OptionalOne, boost::mpl::vector<IgnoreAny> > ArraySizeRef;

        typedef Element<ElementNames::MaxLength, OptionalOne > MaxLength;
        typedef Element<ElementNames::MaxLengthRef, OptionalOne, boost::mpl::vector<IgnoreAny> > MaxLengthRef;

        typedef Element<ElementNames::Membersummary, OptionalOne> Membersummary;
        typedef Element<ElementNames::MemberName, One> MemberName;
        typedef Element<ElementNames::MemberType, One > MemberType;
        typedef Element<ElementNames::Member, AtLeastOne, boost::mpl::vector<Membersummary, MemberName, MemberType, ELEMENT_CHOICE_2(ArraySize, ArraySizeRef, OptionalOne), ELEMENT_CHOICE_2(MaxLength, MaxLengthRef, OptionalOne) > > Member;
        typedef Element<ElementNames::ClassMembers, OptionalOne, boost::mpl::vector<Member> > Members;

        typedef Element<ElementNames::Parametersummary, OptionalOne> Parametersummary;
        typedef Element<ElementNames::ParameterName, One> ParameterName;
        typedef Element<ElementNames::ParameterType, One > ParameterType;
        typedef Element<ElementNames::ParameterValue, One> ParameterValue;
        typedef Element<ElementNames::ParameterValueRef, One, boost::mpl::vector<IgnoreAny> > ParameterValueRef;
        typedef Element<ElementNames::ParameterEntityId, One, boost::mpl::vector< Ignore<ElementNames::ClassName>, Ignore<ElementNames::InstanceId> > > ParameterEntityId;
        typedef Element<ElementNames::Any, One, boost::mpl::vector<IgnoreAny>, ParseAlgorithm<ElementNames::ParameterObject>, AnyMatcher > ParameterObject;
        typedef Element<ElementNames::ParameterArrayElement, AtLeastOne, boost::mpl::vector< ELEMENT_CHOICE_4(ParameterValue, ParameterValueRef, ParameterEntityId, ParameterObject, OptionalOne), Ignore<ElementNames::ParameterArrayIndex> > > ParameterArrayElement;
        typedef Element<ElementNames::ParameterArrayElements, One, boost::mpl::vector<ParameterArrayElement> > ParameterArrayElements;
        typedef Element<ElementNames::Parameter, AtLeastOne, boost::mpl::vector<ELEMENT_CHOICE_5(ParameterValue, ParameterEntityId, ParameterValueRef, ParameterArrayElements, ParameterObject, One), Parametersummary, ParameterName, ParameterType > > Parameter;
        typedef Element<ElementNames::Parameters, OptionalOne, boost::mpl::vector<Parameter> > Parameters;

        typedef Element<ElementNames::CreateRoutinesummary, OptionalOne> CreateRoutinesummary;
        typedef Element<ElementNames::CreateRoutineValueMember, One> CreateRoutineValueMember;
        typedef Element<ElementNames::CreateRoutineValueParameter, One, boost::mpl::vector<IgnoreAny> > CreateRoutineValueParameter;
        typedef Element<ElementNames::CreateRoutineValueValue, One> CreateRoutineValueValue;
        typedef Element<ElementNames::CreateRoutineValueEntityId, One, boost::mpl::vector< Ignore<ElementNames::ClassName>, Ignore<ElementNames::InstanceId> > > CreateRoutineValueEntityId;
        typedef Element<ElementNames::Any, One, boost::mpl::vector<IgnoreAny>, ParseAlgorithm<ElementNames::CreateRoutineValueObject>, AnyMatcher > CreateRoutineValueObject;
        typedef Element<ElementNames::CreateRoutineValue, AtLeastOne, boost::mpl::vector<ELEMENT_CHOICE_4(CreateRoutineValueParameter, CreateRoutineValueValue, CreateRoutineValueEntityId, CreateRoutineValueObject, One), CreateRoutineValueMember > > CreateRoutineValue;
        typedef Element<ElementNames::CreateRoutineValues, OptionalOne, boost::mpl::vector<CreateRoutineValue> > CreateRoutineValues;
        typedef Element<ElementNames::CreateRoutineName, One> CreateRoutineName;
        typedef Element<ElementNames::CreateRoutineMemberName, AtLeastOne > CreateRoutineMemberName;
        typedef Element<ElementNames::CreateRoutineParameterList, One, boost::mpl::vector<CreateRoutineMemberName> > CreateRoutineParameters;
        typedef Element<ElementNames::CreateRoutine, AtLeastOne, boost::mpl::vector<CreateRoutinesummary, CreateRoutineName, CreateRoutineParameters, CreateRoutineValues> > CreateRoutine;
        typedef Element<ElementNames::CreateRoutines, OptionalOne, boost::mpl::vector<CreateRoutine> > CreateRoutines;

        //DOM file elements
        typedef Element<ElementNames::PropertyMappingsummary, OptionalOne> PropertyMappingsummary;
        typedef Element<ElementNames::MappedProperty, One> MappedProperty;
        typedef Element<ElementNames::MappedClass, One> MappedClass;
        typedef Element<ElementNames::MapPropertyMember, One> MapPropertyMember;
        typedef Element<ElementNames::MapValue, One> MapValue;
        typedef Element<ElementNames::MapValueRef, One, boost::mpl::vector<IgnoreAny> > MapValueRef;
        typedef Element<ElementNames::MapEntityId, One, boost::mpl::vector< Ignore<ElementNames::ClassName>, Ignore<ElementNames::InstanceId> > > MapEntityId;
        typedef Element<ElementNames::Any, One, boost::mpl::vector<IgnoreAny>, ParseAlgorithm<ElementNames::MapObject>, AnyMatcher > MapObject;

        typedef Element<ElementNames::ClassMemberReference, One, boost::mpl::vector<IgnoreAny> > ClassMemberReference;
        typedef Element<ElementNames::MemberMapping, AtLeastOne, boost::mpl::vector< ELEMENT_CHOICE_5(MapValue, MapValueRef, MapEntityId, ClassMemberReference, MapObject, OptionalOne), MapPropertyMember > > MemberMapping;
        typedef Element<ElementNames::MemberMappings, OptionalOne, boost::mpl::vector<MemberMapping> > MemberMappings;

        //Top level
        typedef Element<ElementNames::Class, OptionalOne, boost::mpl::vector<Ignore<ElementNames::XmlAttribute>, Classsummary, ClassName, BaseClass, Members, CreateRoutines, Parameters> > ClassParser;
        typedef Element<ElementNames::Enumeration, OptionalOne, boost::mpl::vector<Ignore<ElementNames::XmlAttribute>, Enumerationsummary, EnumerationName, EnumerationValues> > EnumerationParser;
        typedef Element<ElementNames::Exception, OptionalOne, boost::mpl::vector<Ignore<ElementNames::XmlAttribute>, Exceptionsummary, ExceptionName, ExceptionBase> > ExceptionParser;
        typedef Element<ElementNames::Property, OptionalOne, boost::mpl::vector<Ignore<ElementNames::XmlAttribute>, Propertysummary, PropertyName, PropertyMembers> > PropertyParser;
        typedef Element<ElementNames::PropertyMapping, OptionalOne, boost::mpl::vector<Ignore<ElementNames::XmlAttribute>, PropertyMappingsummary, MappedProperty, MappedClass, MemberMappings> > PropertyMappingParser;
    }

    //This is the top level parser, the only one used directly on a dou or dom-file.
    typedef ELEMENT_CHOICE_4(ClassParser, EnumerationParser, ExceptionParser, PropertyParser, OptionalOne) DouParser;
    typedef PropertyMappingParser DomParser;
}
}
}
}

#endif
