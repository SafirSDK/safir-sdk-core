/******************************************************************************
*
* Copyright Saab AB, 2004-2012 (http://www.safirsdk.com)
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
#ifndef __DOTS_ELEMENT_NAMES_H__
#define __DOTS_ELEMENT_NAMES_H__

#include <map>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Parser
{
    class ElementNames
    {
    public:
        enum Element
        {
            //--- Class ---
            Class,
            ClassSummary,
            ClassName,
            BaseClass,
            ClassMembers,
            Parameters,
            CreateRoutines,

            Member,
            MemberSummary,
            MemberName,
            MemberType,
            ArraySize,
            ArraySizeRef,
            ArraySizeRefName,
            MaxLength,
            MaxLengthRef,
            MaxLengthRefName,

            CreateRoutine,
            CreateRoutineSummary,
            CreateRoutineName,
            CreateRoutineParameterList,
            CreateRoutineMemberName,
            CreateRoutineValues,
            CreateRoutineValue,
            CreateRoutineValueMember,
            CreateRoutineValueParameter,

            Parameter,
            ParameterSummary,
            ParameterName,
            ParameterType,
            ParameterValue,
            ParameterArrayElement,
            ParameterArrayElements,
            Object,
            EntityId,
            InstanceIdRef,
            InstanceId,
            
            //--- Enum ---
            Enumeration,
            EnumerationSummary,
            EnumerationName,
            EnumerationValues,
            EnumerationValue,

            //--- Exception ---
            Exception,
            ExceptionSummary,
            ExceptionName,
            ExceptionBase,
            

            //--- Property ---
            Property,
            PropertySummary,
            PropertyName,
            PropertyMembers,

            PropertyMember,
            PropertyMemberSummary,
            PropertyMemberName,
            PropertyMemberType,

            //--- PropertyMapping ---
            PropertyMapping,
            PropertyMappingSummary,
            MappedProperty,
            MappedClass,
            MemberMappings,
            MemberMapping,
            MapPropertyMember,
            MapValue,
            MapValueRef,
            MapValueRefName,
            ClassMemberReference,
            MapClassMember,

            Index,
            IndexRef,
            IndexRefName,

            /*Array,
            Changed,
            TypeId,
            Error,*/

            //This one is a placeholder for all xml attributes and xml elements. I.e not a normal element.
            XmlAttribute,
            XmlComment
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

            m_map[ClassSummary]="summary";
            m_map[ExceptionSummary]="summary";
            m_map[EnumerationSummary]="summary";
            m_map[PropertySummary]="summary";
            m_map[ParameterSummary]="summary";
            m_map[MemberSummary]="summary";
            m_map[CreateRoutineSummary]="summary";
            m_map[ClassName]="name";
            m_map[ExceptionName]="name";
            m_map[EnumerationName]="name";
            m_map[PropertyName]="name";
            m_map[ParameterName]="name";
            m_map[MemberName]="name";
            m_map[CreateRoutineName]="name";
            m_map[ParameterType]="type";
            m_map[ParameterValue]="value";
            m_map[ParameterArrayElement]="arrayElement";
            m_map[ParameterArrayElements]="arrayElements";
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

            m_map[PropertyMember]="member";
            m_map[Enumeration]="enumeration";
            m_map[Exception]="exception";
            m_map[Index]="index";
            m_map[MaxLength]="maxLength";
            m_map[Member]="member";
            m_map[ClassMembers]="members";
            m_map[Parameter]="parameter";
            m_map[Parameters]="parameters";
            m_map[Property]="property";
            m_map[MemberType]="type";
            m_map[EnumerationValue]="value";
            m_map[EnumerationValues]="values";
            m_map[PropertyMemberSummary]="summary";
            m_map[PropertyMemberName]="name";
            m_map[PropertyMemberType]="type";
            m_map[PropertyMembers] = "members";
            
            m_map[PropertyMapping]="propertyMapping";
            m_map[PropertyMappingSummary]="summary";
            m_map[MappedProperty]="property";
            m_map[MappedClass]="class";
            m_map[MemberMappings]="memberMapping";
            m_map[MemberMapping]="member";
            m_map[MapPropertyMember]="propertyMember";
            m_map[MapValue]="value"; //dom
            m_map[MapValueRef]="valueRef"; //dom
            m_map[MapValueRefName]="name"; //dom
            m_map[ClassMemberReference]="classMemberReference"; //dom
            m_map[MapClassMember]="classMember";

            m_map[ArraySizeRef]="arraySizeRef"; //member array 
            m_map[ArraySizeRefName]="name";   //member arraySizeRef
            m_map[MaxLengthRef]="maxLengthRef"; //member maxLength
            m_map[MaxLengthRefName]="name";   //memeber maxLengthRef
            m_map[InstanceIdRef]="instanceIdRef"; //??????
            m_map[IndexRef]="indexRef"; //???????? troligen dom mot värde i array
            
            m_map[Object]="object";
            m_map[EntityId]="entityId";
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
