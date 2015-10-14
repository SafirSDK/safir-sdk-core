/******************************************************************************
*
* Copyright Saab AB, 2004-2015 (http://safirsdkcore.com)
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
#ifndef __DOTS_INTERNAL_ELEMENT_NAMES_H__
#define __DOTS_INTERNAL_ELEMENT_NAMES_H__

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
    namespace Elements
    {
        struct XmlAttribute {static const std::string& Name() {static const std::string n="<xmlattr>"; return n;}};
        struct Any {static const std::string& Name() {static const std::string n="any"; return n;}};
        struct Classsummary {static const std::string& Name() {static const std::string n="summary"; return n;}};
        struct Exceptionsummary {static const std::string& Name() {static const std::string n="summary"; return n;}};
        struct Enumerationsummary {static const std::string& Name() {static const std::string n="summary"; return n;}};
        struct Propertysummary {static const std::string& Name() {static const std::string n="summary"; return n;}};
        struct Parametersummary {static const std::string& Name() {static const std::string n="summary"; return n;}};
        struct Membersummary {static const std::string& Name() {static const std::string n="summary"; return n;}};
        struct CreateRoutinesummary {static const std::string& Name() {static const std::string n="summary"; return n;}};
        struct ClassName {static const std::string& Name() {static const std::string n="name"; return n;}};
        struct ExceptionName {static const std::string& Name() {static const std::string n="name"; return n;}};
        struct EnumerationName {static const std::string& Name() {static const std::string n="name"; return n;}};
        struct PropertyName {static const std::string& Name() {static const std::string n="name"; return n;}};
        struct ParameterName {static const std::string& Name() {static const std::string n="name"; return n;}};
        struct MemberName {static const std::string& Name() {static const std::string n="name"; return n;}};
        struct CreateRoutineName {static const std::string& Name() {static const std::string n="name"; return n;}};
        struct ParameterType {static const std::string& Name() {static const std::string n="type"; return n;}};
        struct ParameterValue {static const std::string& Name() {static const std::string n="value"; return n;}};
        struct ParameterValueRef {static const std::string& Name() {static const std::string n="valueRef"; return n;}};
        struct ParameterArray {static const std::string& Name() {static const std::string n="array"; return n;}};
        struct ParameterArrayElement {static const std::string& Name() {static const std::string n="arrayElement"; return n;}};
        struct ParameterArrayElements {static const std::string& Name() {static const std::string n="arrayElements"; return n;}};
        struct ParameterSequence {static const std::string& Name() {static const std::string n="sequence"; return n;}};
        struct ParameterDictionary {static const std::string& Name() {static const std::string n="dictionary"; return n;}};
        struct ParameterDictionaryEntry {static const std::string& Name() {static const std::string n="entry"; return n;}};
        struct ParameterDictionaryKey {static const std::string& Name() {static const std::string n="key"; return n;}};
        struct ParameterObject {static const std::string& Name() {static const std::string n="object"; return n;}};
        struct ParameterEntityId {static const std::string& Name() {static const std::string n="entityId"; return n;}};
        struct ParameterArrayIndex {static const std::string& Name() {static const std::string n="index"; return n;}};
        struct ParameterValueCollection {static const std::string& Name() {static const std::string n="value"; return n;}};
        struct ParameterEntityIdCollection {static const std::string& Name() {static const std::string n="entityId"; return n;}};
        struct ParameterValueRefCollection {static const std::string& Name() {static const std::string n="valueRef"; return n;}};
        struct ParameterObjectCollection {static const std::string& Name() {static const std::string n="object"; return n;}};
        struct ArraySize {static const std::string& Name() {static const std::string n="arraySize"; return n;}};
        struct Dictionary {static const std::string& Name() {static const std::string n="dictionary"; return n;}};
        struct Sequence {static const std::string& Name() {static const std::string n="sequence"; return n;}};
        struct BaseClass {static const std::string& Name() {static const std::string n="baseClass"; return n;}};
        struct ExceptionBase {static const std::string& Name() {static const std::string n="baseClass"; return n;}};
        struct Class {static const std::string& Name() {static const std::string n="class"; return n;}};
        struct CreateRoutine {static const std::string& Name() {static const std::string n="createRoutine"; return n;}};
        struct CreateRoutines {static const std::string& Name() {static const std::string n="createRoutines"; return n;}};
        struct CreateRoutineMemberName {static const std::string& Name() {static const std::string n="member"; return n;}};
        struct CreateRoutineParameterList {static const std::string& Name() {static const std::string n="parameters"; return n;}};
        struct CreateRoutineValues {static const std::string& Name() {static const std::string n="values"; return n;}};
        struct CreateRoutineValue {static const std::string& Name() {static const std::string n="value"; return n;}};
        struct CreateRoutineValueMember {static const std::string& Name() {static const std::string n="member"; return n;}};
        struct CreateRoutineValueParameter {static const std::string& Name() {static const std::string n="parameter"; return n;}};
        struct CreateRoutineValueValue {static const std::string& Name() {static const std::string n="value"; return n;}};
        struct CreateRoutineValueEntityId {static const std::string& Name() {static const std::string n="entityId"; return n;}};
        struct CreateRoutineValueObject {static const std::string& Name() {static const std::string n="object"; return n;}};
        struct PropertyMember {static const std::string& Name() {static const std::string n="member"; return n;}};
        struct Enumeration {static const std::string& Name() {static const std::string n="enumeration"; return n;}};
        struct Exception {static const std::string& Name() {static const std::string n="exception"; return n;}};
        struct MaxLength {static const std::string& Name() {static const std::string n="maxLength"; return n;}};
        struct Member {static const std::string& Name() {static const std::string n="member"; return n;}};
        struct ClassMembers {static const std::string& Name() {static const std::string n="members"; return n;}};
        struct Parameter {static const std::string& Name() {static const std::string n="parameter"; return n;}};
        struct Parameters {static const std::string& Name() {static const std::string n="parameters"; return n;}};
        struct Property {static const std::string& Name() {static const std::string n="property"; return n;}};
        struct MemberType {static const std::string& Name() {static const std::string n="type"; return n;}};
        struct EnumerationValue {static const std::string& Name() {static const std::string n="value"; return n;}};
        struct EnumerationValues {static const std::string& Name() {static const std::string n="values"; return n;}};
        struct PropertyMembersummary {static const std::string& Name() {static const std::string n="summary"; return n;}};
        struct PropertyMemberName {static const std::string& Name() {static const std::string n="name"; return n;}};
        struct PropertyMemberType {static const std::string& Name() {static const std::string n="type"; return n;}};
        struct PropertyMembers {static const std::string& Name() {static const std::string n="members"; return n;}};
        struct PropertyMemberisArray {static const std::string& Name() {static const std::string n="array"; return n;}};
        struct PropertyMemberIsSequence {static const std::string& Name() {static const std::string n="sequence"; return n;}};
        struct PropertyMemberIsDictionary {static const std::string& Name() {static const std::string n="dictionary"; return n;}};
        struct PropertyMapping {static const std::string& Name() {static const std::string n="propertyMapping"; return n;}};
        struct PropertyMappingsummary {static const std::string& Name() {static const std::string n="summary"; return n;}};
        struct MappedProperty {static const std::string& Name() {static const std::string n="property"; return n;}};
        struct MappedClass {static const std::string& Name() {static const std::string n="class"; return n;}};
        struct MemberMappings {static const std::string& Name() {static const std::string n="memberMapping"; return n;}};
        struct MemberMapping {static const std::string& Name() {static const std::string n="member"; return n;}};
        struct MapPropertyMember {static const std::string& Name() {static const std::string n="propertyMember"; return n;}};
        struct MapValue {static const std::string& Name() {static const std::string n="value"; return n;}}; //dom
        struct MapValueRef {static const std::string& Name() {static const std::string n="valueRef"; return n;}}; //dom
        struct MapValueRefCollection {static const std::string& Name() {static const std::string n="valueRef"; return n;}}; //dom
        struct MapArray {static const std::string& Name() {static const std::string n="array"; return n;}}; //dom
        struct MapArrayElements {static const std::string& Name() {static const std::string n="arrayElements"; return n;}}; //dom
        struct MapArrayElement {static const std::string& Name() {static const std::string n="arrayElement"; return n;}}; //dom
        struct MapSequence {static const std::string& Name() {static const std::string n="sequence"; return n;}}; //dom
        struct MapDictionaryEntry {static const std::string& Name() {static const std::string n="entry"; return n;}};
        struct MapDictionaryKey {static const std::string& Name() {static const std::string n="key"; return n;}};
        struct MapDictionary {static const std::string& Name() {static const std::string n="dictionary"; return n;}}; //dom
        struct ClassMemberReference {static const std::string& Name() {static const std::string n="classMemberReference"; return n;}}; //dom
        struct ClassMemberReferenceName {static const std::string& Name() {static const std::string n="classMember"; return n;}};
        struct ClassMemberReferenceIndex {static const std::string& Name() {static const std::string n="index"; return n;}};
        struct MapEntityId {static const std::string& Name() {static const std::string n="entityId"; return n;}};
        struct MapObject {static const std::string& Name() {static const std::string n="object"; return n;}};
        struct ArraySizeRef {static const std::string& Name() {static const std::string n="arraySizeRef"; return n;}}; //member array
        struct MaxLengthRef {static const std::string& Name() {static const std::string n="maxLengthRef"; return n;}}; //member maxLength
        struct InstanceIdRef {static const std::string& Name() {static const std::string n="instanceIdRef"; return n;}};
        struct IndexRef {static const std::string& Name() {static const std::string n="indexRef"; return n;}}; //domFiles valueRef and classMemberReference
        struct ReferenceName {static const std::string& Name() {static const std::string n="name"; return n;}};
        struct ReferenceIndex {static const std::string& Name() {static const std::string n="index"; return n;}};
        struct ReferenceKey {static const std::string& Name() {static const std::string n="key"; return n;}};
        struct InstanceId {static const std::string& Name() {static const std::string n="instanceId"; return n;}}; //<entityId><name/><instanceId/>..

        //for handling of old deprecated xml format
        struct ParameterObjectDeprecated {static const std::string& Name() {static const std::string n="object"; return n;}};
        struct ParameterObjectDeprecatedCollection {static const std::string& Name() {static const std::string n="object"; return n;}};
        struct CreateRoutineValueObjectDeprecated {static const std::string& Name() {static const std::string n="object"; return n;}};
        struct MapObjectDeprecated {static const std::string& Name() {static const std::string n="object"; return n;}};
    }
}
}
}
}

#endif
