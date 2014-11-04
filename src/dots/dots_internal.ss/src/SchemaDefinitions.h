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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#ifndef __DOTS_INTERNAL_ELEMENT_PARSER_DEFS_H__
#define __DOTS_INTERNAL_ELEMENT_PARSER_DEFS_H__

#include "ElementParserBase.h"

/**
 * This file defines the schema for dou- and dom-files.
 * It defines wich combinations of elements that are valid, and it specifies if elements are mandatory or
 * optional, how many times it may occur and so on.
 */
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
    namespace Schema
    {
        //DOU file elements
        typedef Element<Elements::Classsummary, OptionalOne> Classsummary;
        typedef Element<Elements::ClassName, One> ClassName;
        typedef Element<Elements::BaseClass, One > BaseClass;

        typedef Element<Elements::Exceptionsummary, OptionalOne> Exceptionsummary;
        typedef Element<Elements::ExceptionName, One> ExceptionName;
        typedef Element<Elements::ExceptionBase, One > ExceptionBase;

        typedef Element<Elements::Enumerationsummary, OptionalOne> Enumerationsummary;
        typedef Element<Elements::EnumerationName, One> EnumerationName;
        typedef Element<Elements::EnumerationValue, AtLeastOne> EnumerationValue;
        typedef Element<Elements::EnumerationValues, One, boost::mpl::vector<EnumerationValue> > EnumerationValues;

        typedef Element<Elements::Propertysummary, OptionalOne> Propertysummary;
        typedef Element<Elements::PropertyName, One> PropertyName;
        typedef Element<Elements::PropertyMembersummary, OptionalOne> PropertyMembersummary;
        typedef Element<Elements::PropertyMemberName, One> PropertyMemberName;
        typedef Element<Elements::PropertyMemberisArray, OptionalOne> PropertyMemberisArray;
        typedef Element<Elements::PropertyMemberIsSequence, OptionalOne> PropertyMemberIsSequence;
        typedef Element<Elements::PropertyMemberIsDictionary, OptionalOne> PropertyMemberIsDictionary;
        typedef Element<Elements::PropertyMemberType, One> PropertyMemberType;
        typedef Element<Elements::PropertyMember, AtLeastOne, boost::mpl::vector<PropertyMembersummary, PropertyMemberName, PropertyMemberType, ELEMENT_CHOICE_3(PropertyMemberisArray, PropertyMemberIsSequence, PropertyMemberIsDictionary, OptionalOne) > > PropertyMember;
        typedef Element<Elements::PropertyMembers, OptionalOne, boost::mpl::vector<PropertyMember> > PropertyMembers;

        typedef Element<Elements::ArraySize, OptionalOne > ArraySize;
        typedef Element<Elements::ArraySizeRef, OptionalOne, boost::mpl::vector<IgnoreAny> > ArraySizeRef;
        typedef Element<Elements::Dictionary, OptionalOne, boost::mpl::vector<Ignore<Elements::XmlAttribute> > > Dictionary;
        typedef Element<Elements::Sequence, OptionalOne > Sequence;

        typedef Element<Elements::MaxLength, OptionalOne > MaxLength;
        typedef Element<Elements::MaxLengthRef, OptionalOne, boost::mpl::vector<IgnoreAny> > MaxLengthRef;

        typedef Element<Elements::Membersummary, OptionalOne> Membersummary;
        typedef Element<Elements::MemberName, One> MemberName;
        typedef Element<Elements::MemberType, One > MemberType;
        typedef Element<Elements::Member, AtLeastOne, boost::mpl::vector<Membersummary, MemberName, MemberType, ELEMENT_CHOICE_4(ArraySize, ArraySizeRef, Dictionary, Sequence, OptionalOne), ELEMENT_CHOICE_2(MaxLength, MaxLengthRef, OptionalOne) > > Member;
        typedef Element<Elements::ClassMembers, OptionalOne, boost::mpl::vector<Member> > Members;

        typedef Element<Elements::Parametersummary, OptionalOne> Parametersummary;
        typedef Element<Elements::ParameterName, One> ParameterName;
        typedef Element<Elements::ParameterType, One > ParameterType;
        typedef Element<Elements::ParameterValue, One> ParameterValue;
        typedef Element<Elements::ParameterValueRef, One, boost::mpl::vector<IgnoreAny> > ParameterValueRef;
        typedef Element<Elements::ParameterEntityId, One, boost::mpl::vector< Ignore<Elements::ClassName>, Ignore<Elements::InstanceId> > > ParameterEntityId;
        typedef Element<Elements::ParameterObjectDeprecated, One, boost::mpl::vector<IgnoreAny> > ParameterObjectDeprecated;
        typedef Element<Elements::Any, One, boost::mpl::vector<IgnoreAny>, ParseAlgorithm<Elements::ParameterObject>, AnyMatcher > ParameterObject;
        typedef Element<Elements::ParameterValue, OptionalMany > ParameterValueCollection;
        typedef Element<Elements::ParameterEntityId, OptionalMany, boost::mpl::vector< Ignore<Elements::ClassName>, Ignore<Elements::InstanceId> > > ParameterEntityIdCollection;
        typedef Element<Elements::ParameterValueRef, OptionalMany, boost::mpl::vector<IgnoreAny> > ParameterValueRefCollection;
        typedef Element<Elements::ParameterObjectDeprecated, OptionalMany, boost::mpl::vector<IgnoreAny> > ParameterObjectDeprecatedCollection;
        typedef Element<Elements::Any, OptionalMany, boost::mpl::vector<IgnoreAny>, ParseAlgorithm<Elements::ParameterObject>, AnyMatcher > ParameterObjectCollection;
        typedef Element<Elements::ParameterArray, One, boost::mpl::vector<ParameterObjectCollection, ParameterValueRefCollection, ELEMENT_CHOICE_3(ParameterValueCollection, ParameterEntityIdCollection, ParameterObjectDeprecatedCollection, OptionalOne) > > ParameterArray;
        typedef Element<Elements::ParameterArrayElement, AtLeastOne, boost::mpl::vector< ELEMENT_CHOICE_5(ParameterValue, ParameterValueRef, ParameterEntityId, ParameterObjectDeprecated, ParameterObject, OptionalOne), Ignore<Elements::ParameterArrayIndex> > > ParameterArrayElement;
        typedef Element<Elements::ParameterArrayElements, One, boost::mpl::vector<ParameterArrayElement> > ParameterArrayElements;
        typedef Element<Elements::ParameterDictionaryKey, One, boost::mpl::vector< Ignore<Elements::ClassName>, Ignore<Elements::InstanceId> > > ParameterDictionaryKey;
        typedef Element<Elements::ParameterDictionaryEntry, OptionalMany, boost::mpl::vector<ELEMENT_CHOICE_5(ParameterValue, ParameterValueRef, ParameterEntityId, ParameterObjectDeprecated, ParameterObject, One), ParameterDictionaryKey> > ParameterDictionaryEntry;
        typedef Element<Elements::ParameterSequence, One> ParameterSequence;
        //typedef Element<Elements::ParameterSequence, One, boost::mpl::vector<ParameterObjectCollection, ParameterValueRefCollection, ELEMENT_CHOICE_3(ParameterValueCollection, ParameterEntityIdCollection, ParameterObjectDeprecatedCollection, OptionalOne) > > ParameterSequence;
        typedef Element<Elements::ParameterDictionary, One, boost::mpl::vector<Ignore<Elements::XmlAttribute>, ParameterDictionaryEntry > > ParameterDictionary;
        typedef Element<Elements::Parameter, AtLeastOne, boost::mpl::vector<ELEMENT_CHOICE_9(ParameterValue, ParameterEntityId, ParameterValueRef, ParameterArray, ParameterArrayElements, ParameterSequence, ParameterDictionary, ParameterObjectDeprecated, ParameterObject, One), Parametersummary, ParameterName, ParameterType > > Parameter;
        typedef Element<Elements::Parameters, OptionalOne, boost::mpl::vector<Parameter> > Parameters;

        typedef Element<Elements::CreateRoutinesummary, OptionalOne> CreateRoutinesummary;
        typedef Element<Elements::CreateRoutineValueMember, One> CreateRoutineValueMember;
        typedef Element<Elements::CreateRoutineValueParameter, One, boost::mpl::vector<IgnoreAny> > CreateRoutineValueParameter;
        typedef Element<Elements::CreateRoutineValueValue, One> CreateRoutineValueValue;
        typedef Element<Elements::CreateRoutineValueEntityId, One, boost::mpl::vector< Ignore<Elements::ClassName>, Ignore<Elements::InstanceId> > > CreateRoutineValueEntityId;
        typedef Element<Elements::CreateRoutineValueObjectDeprecated, One, boost::mpl::vector<IgnoreAny> > CreateRoutineValueObjectDeprecated;
        typedef Element<Elements::Any, One, boost::mpl::vector<IgnoreAny>, ParseAlgorithm<Elements::CreateRoutineValueObject>, AnyMatcher > CreateRoutineValueObject;
        typedef Element<Elements::CreateRoutineValue, AtLeastOne, boost::mpl::vector<ELEMENT_CHOICE_5(CreateRoutineValueParameter, CreateRoutineValueValue, CreateRoutineValueEntityId, CreateRoutineValueObjectDeprecated, CreateRoutineValueObject, One), CreateRoutineValueMember > > CreateRoutineValue;
        typedef Element<Elements::CreateRoutineValues, OptionalOne, boost::mpl::vector<CreateRoutineValue> > CreateRoutineValues;
        typedef Element<Elements::CreateRoutineName, One> CreateRoutineName;
        typedef Element<Elements::CreateRoutineMemberName, AtLeastOne > CreateRoutineMemberName;
        typedef Element<Elements::CreateRoutineParameterList, OptionalOne, boost::mpl::vector<CreateRoutineMemberName> > CreateRoutineParameters;
        typedef Element<Elements::CreateRoutine, AtLeastOne, boost::mpl::vector<CreateRoutinesummary, CreateRoutineName, CreateRoutineParameters, CreateRoutineValues> > CreateRoutine;
        typedef Element<Elements::CreateRoutines, OptionalOne, boost::mpl::vector<CreateRoutine> > CreateRoutines;

        //DOM file elements
        typedef Element<Elements::PropertyMappingsummary, OptionalOne> PropertyMappingsummary;
        typedef Element<Elements::MappedProperty, One> MappedProperty;
        typedef Element<Elements::MappedClass, One> MappedClass;
        typedef Element<Elements::MapPropertyMember, One> MapPropertyMember;
        typedef Element<Elements::MapValue, One> MapValue;
        typedef Element<Elements::MapValueRef, One, boost::mpl::vector<IgnoreAny> > MapValueRef;
        typedef Element<Elements::MapEntityId, One, boost::mpl::vector< Ignore<Elements::ClassName>, Ignore<Elements::InstanceId> > > MapEntityId;
        typedef Element<Elements::Any, One, boost::mpl::vector<IgnoreAny>, ParseAlgorithm<Elements::MapObject>, AnyMatcher > MapObject;
        typedef Element<Elements::MapObjectDeprecated, One, boost::mpl::vector<IgnoreAny> > MapObjectDeprecated;
        typedef Element<Elements::MapValue, OptionalMany > MapValueCollection;
        typedef Element<Elements::MapEntityId, OptionalMany, boost::mpl::vector< Ignore<Elements::ClassName>, Ignore<Elements::InstanceId> > > MapEntityIdCollection;
        typedef Element<Elements::MapValueRefCollection, OptionalMany, boost::mpl::vector<IgnoreAny> > MapValueRefCollection;
        typedef Element<Elements::MapObjectDeprecated, OptionalMany, boost::mpl::vector<IgnoreAny> > MapObjectDeprecatedCollection;
        typedef Element<Elements::Any, OptionalMany, boost::mpl::vector<IgnoreAny>, ParseAlgorithm<Elements::MapObject>, AnyMatcher > MapObjectCollection;
        typedef Element<Elements::MapArray, One, boost::mpl::vector<MapObjectCollection, MapValueRefCollection, ELEMENT_CHOICE_3(MapValueCollection, MapEntityIdCollection, MapObjectDeprecatedCollection, OptionalOne) > > MapArray;
        typedef Element<Elements::MapSequence, One, boost::mpl::vector<MapObjectCollection, MapValueRefCollection, ELEMENT_CHOICE_3(MapValueCollection, MapEntityIdCollection, MapObjectDeprecatedCollection, OptionalOne) > > MapSequence;
        typedef Element<Elements::MapDictionaryKey, One, boost::mpl::vector< Ignore<Elements::ClassName>, Ignore<Elements::InstanceId> > > MapDictionaryKey;
        typedef Element<Elements::MapDictionaryEntry, OptionalMany, boost::mpl::vector<ELEMENT_CHOICE_5(MapValue, MapValueRef, MapEntityId, MapObjectDeprecated, MapObject, OptionalOne), MapDictionaryKey> > MapDictionaryEntry;
        typedef Element<Elements::MapDictionary, One, boost::mpl::vector<MapDictionaryEntry > > MapDictionary;

        typedef Element<Elements::MapArrayElement, AtLeastOne, boost::mpl::vector< ELEMENT_CHOICE_5(MapValue, MapValueRef, MapEntityId, MapObjectDeprecated, MapObject, OptionalOne), Ignore<Elements::ParameterArrayIndex> > > MapArrayElement;
        typedef Element<Elements::MapArrayElements, One, boost::mpl::vector<MapArrayElement> > MapArrayElements;
        typedef Element<Elements::ClassMemberReference, One, boost::mpl::vector<IgnoreAny> > ClassMemberReference;
        typedef Element<Elements::MemberMapping, AtLeastOne, boost::mpl::vector< ELEMENT_CHOICE_10(MapValue, MapEntityId, MapValueRef, MapArray, MapArrayElements, MapSequence, MapDictionary, ClassMemberReference, MapObjectDeprecated, MapObject, OptionalOne), MapPropertyMember > > MemberMapping;
        typedef Element<Elements::MemberMappings, OptionalOne, boost::mpl::vector<MemberMapping> > MemberMappings;

        //Top level
        typedef Element<Elements::Class, OptionalOne, boost::mpl::vector<Ignore<Elements::XmlAttribute>, Classsummary, ClassName, BaseClass, Members, CreateRoutines, Parameters> > ClassParser;
        typedef Element<Elements::Enumeration, OptionalOne, boost::mpl::vector<Ignore<Elements::XmlAttribute>, Enumerationsummary, EnumerationName, EnumerationValues> > EnumerationParser;
        typedef Element<Elements::Exception, OptionalOne, boost::mpl::vector<Ignore<Elements::XmlAttribute>, Exceptionsummary, ExceptionName, ExceptionBase> > ExceptionParser;
        typedef Element<Elements::Property, OptionalOne, boost::mpl::vector<Ignore<Elements::XmlAttribute>, Propertysummary, PropertyName, PropertyMembers> > PropertyParser;
        typedef Element<Elements::PropertyMapping, OptionalOne, boost::mpl::vector<Ignore<Elements::XmlAttribute>, PropertyMappingsummary, MappedProperty, MappedClass, MemberMappings> > PropertyMappingParser;
    }

    //This is the top level parser, the only one used directly on a dou or dom-file.
    typedef ELEMENT_CHOICE_4(Schema::ClassParser, Schema::EnumerationParser, Schema::ExceptionParser, Schema::PropertyParser, OptionalOne) DouParser;
    typedef Schema::PropertyMappingParser DomParser;
}
}
}
}

#endif
