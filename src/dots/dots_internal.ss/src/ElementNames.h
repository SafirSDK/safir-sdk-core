/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://www.safirsdk.com)
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
#ifndef __DOTS_INTERNAL_ELEMENT_NAMES_H__
#define __DOTS_INTERNAL_ELEMENT_NAMES_H__

#include <map>


//---------------------------------------------------
// Defines all xml-elements used in dou/dom-syntax.
// The class is a singleton.
//---------------------------------------------------
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    class ElementNames
    {
    public:
        enum Element
        {
            //--- Class ---
            Class,
            Classsummary,
            ClassName,
            BaseClass,
            ClassMembers,
            Parameters,
            CreateRoutines,

            Member,
            Membersummary,
            MemberName,
            MemberType,
            ArraySize,
            ArraySizeRef,
            MaxLength,
            MaxLengthRef,

            CreateRoutine,
            CreateRoutinesummary,
            CreateRoutineName,
            CreateRoutineParameterList,
            CreateRoutineMemberName,
            CreateRoutineValues,
            CreateRoutineValue,
            CreateRoutineValueMember,
            CreateRoutineValueParameter,
            CreateRoutineValueValue,
            CreateRoutineValueEntityId,
            CreateRoutineValueObject,

            Parameter,
            Parametersummary,
            ParameterName,
            ParameterType,
            ParameterValue,
            ParameterValueRef,            
            ParameterArrayElement,
            ParameterArrayElements,
            ParameterArrayIndex,
            ParameterObject,
            ParameterEntityId,
            InstanceIdRef,
            InstanceId,
            
            //--- Enum ---
            Enumeration,
            Enumerationsummary,
            EnumerationName,
            EnumerationValues,
            EnumerationValue,

            //--- Exception ---
            Exception,
            Exceptionsummary,
            ExceptionName,
            ExceptionBase,
            

            //--- Property ---
            Property,
            Propertysummary,
            PropertyName,
            PropertyMembers,

            PropertyMember,
            PropertyMembersummary,
            PropertyMemberName,
            PropertyMemberType,
            PropertyMemberisArray,

            //--- PropertyMapping ---
            PropertyMapping,
            PropertyMappingsummary,
            MappedProperty,
            MappedClass,
            MemberMappings,
            MemberMapping,
            MapPropertyMember,
            MapValue,
            MapValueRef,
            MapEntityId,
            MapObject,
            ClassMemberReference,
            ClassMemberReferenceName,
            ClassMemberReferenceIndex,

            ReferenceName,
            ReferenceIndex,

            /*Index,
            IndexRef,
            IndexRefName,*/

            /*
            Changed,
            TypeId,
            Error,*/

            //This one is a placeholder for all xml attributes and xml elements. I.e not a normal element.
            XmlAttribute,
            XmlComment,
            Any
        };

        static ElementNames& Instance()
        {
            static ElementNames inst;
            return inst;
        }

        const std::string& String(int id) {return m_map[id];}

    private:
        std::map<int, std::string> m_map;
        ElementNames() : m_map()
        {
            m_map[XmlAttribute]="<xmlattr>";
            m_map[XmlComment]="<xmlcomment>";
            m_map[Any]="any";

            m_map[Classsummary]="summary";
            m_map[Exceptionsummary]="summary";
            m_map[Enumerationsummary]="summary";
            m_map[Propertysummary]="summary";
            m_map[Parametersummary]="summary";
            m_map[Membersummary]="summary";
            m_map[CreateRoutinesummary]="summary";
            m_map[ClassName]="name";
            m_map[ExceptionName]="name";
            m_map[EnumerationName]="name";
            m_map[PropertyName]="name";
            m_map[ParameterName]="name";
            m_map[MemberName]="name";
            m_map[CreateRoutineName]="name";
            m_map[ParameterType]="type";
            m_map[ParameterValue]="value";
            m_map[ParameterValueRef]="valueRef";            
            m_map[ParameterArrayElement]="arrayElement";
            m_map[ParameterArrayElements]="arrayElements";
            m_map[ParameterObject]="object";
            m_map[ParameterEntityId]="entityId";
            m_map[ParameterArrayIndex]="index";

            m_map[ArraySize]="arraySize";
            m_map[BaseClass]="baseClass";
            m_map[ExceptionBase]="baseClass";
            m_map[Class]="class";
            m_map[CreateRoutine]="createRoutine";
            m_map[CreateRoutines]="createRoutines";
            m_map[CreateRoutineMemberName]="member";
            m_map[CreateRoutineParameterList]="parameters";
            m_map[CreateRoutineValues]="values";
            m_map[CreateRoutineValue]="value";
            m_map[CreateRoutineValueMember]="member";
            m_map[CreateRoutineValueParameter]="parameter";
            m_map[CreateRoutineValueValue]="value";
            m_map[CreateRoutineValueEntityId]="entityId";
            m_map[CreateRoutineValueObject]="object";

            m_map[PropertyMember]="member";
            m_map[Enumeration]="enumeration";
            m_map[Exception]="exception";
            m_map[MaxLength]="maxLength";
            m_map[Member]="member";
            m_map[ClassMembers]="members";
            m_map[Parameter]="parameter";
            m_map[Parameters]="parameters";
            m_map[Property]="property";
            m_map[MemberType]="type";
            m_map[EnumerationValue]="value";
            m_map[EnumerationValues]="values";
            m_map[PropertyMembersummary]="summary";
            m_map[PropertyMemberName]="name";
            m_map[PropertyMemberType]="type";
            m_map[PropertyMembers]="members";
            m_map[PropertyMemberisArray]="array";

            m_map[PropertyMapping]="propertyMapping";
            m_map[PropertyMappingsummary]="summary";
            m_map[MappedProperty]="property";
            m_map[MappedClass]="class";
            m_map[MemberMappings]="memberMapping";
            m_map[MemberMapping]="member";
            m_map[MapPropertyMember]="propertyMember";
            m_map[MapValue]="value"; //dom
            m_map[MapValueRef]="valueRef"; //dom
            m_map[ClassMemberReference]="classMemberReference"; //dom
            m_map[ClassMemberReferenceName]="classMember";
            m_map[ClassMemberReferenceIndex]="index";
            m_map[MapEntityId]="entityId";
            m_map[MapObject]="object";

            m_map[ArraySizeRef]="arraySizeRef"; //member array
            m_map[MaxLengthRef]="maxLengthRef"; //member maxLength
            m_map[InstanceIdRef]="instanceIdRef"; //??????
            
            m_map[ReferenceName]="name";
            m_map[ReferenceIndex]="index";

            //m_map[Index]="index";
            //m_map[IndexRef]="indexRef"; //???????? troligen dom mot vrde i array
                        
            m_map[InstanceId]="instanceId"; //<entityId><name/><instanceId/>..

            /*m_map[TypeId]="typeId";
            
            m_map[Error]="error";
            m_map[Changed]="changed";
            m_map[Array]="array";*/
        }
    };
}
}
}
}

#endif
