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
#ifndef __DOTS_ELEMENT_PARSER_DEFS_H__
#define __DOTS_ELEMENT_PARSER_DEFS_H__

#include "ElementParserBase.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Parser
{
    namespace InternalDefs
    {
        namespace SubParsers
        {
            typedef Element<ElementNames::ClassSummary, OptionalOne> ClassSummary;
            typedef Element<ElementNames::ClassName, One> ClassName;
            typedef Element<ElementNames::BaseClass, One > BaseClass;

            typedef Element<ElementNames::PropertyMappingSummary, OptionalOne> PropertyMappingSummary;

            typedef Element<ElementNames::ExceptionSummary, OptionalOne> ExceptionSummary;
            typedef Element<ElementNames::ExceptionName, One> ExceptionName;
            typedef Element<ElementNames::ExceptionBase, One > ExceptionBase;

            typedef Element<ElementNames::EnumerationSummary, OptionalOne> EnumerationSummary;
            typedef Element<ElementNames::EnumerationName, One> EnumerationName;
            typedef Element<ElementNames::EnumerationValue, AtLeastOne> EnumerationValue;
            typedef Element<ElementNames::EnumerationValues, One, boost::mpl::vector<EnumerationValue> > EnumerationValues;
            
            typedef Element<ElementNames::PropertySummary, OptionalOne> PropertySummary;
            typedef Element<ElementNames::PropertyName, One> PropertyName;
            typedef Element<ElementNames::PropertyMemberSummary, OptionalOne> PropertyMemberSummary;
            typedef Element<ElementNames::PropertyMemberName, One> PropertyMemberName;
            typedef Element<ElementNames::PropertyMemberType, One> PropertyMemberType;

            typedef Element<ElementNames::ArraySize, OptionalOne > ArraySize;
            typedef Element<ElementNames::ArraySizeRefName, One > ArraySizeRefName;
            typedef Element<ElementNames::ArraySizeRef, OptionalOne, boost::mpl::vector<ArraySizeRefName> > ArraySizeRef;

            typedef Element<ElementNames::MaxLength, OptionalOne > MaxLength;
            typedef Element<ElementNames::MaxLengthRefName, One > MaxLengthRefName;
            typedef Element<ElementNames::MaxLengthRef, OptionalOne, boost::mpl::vector<MaxLengthRefName> > MaxLengthRef;
            
            typedef Element<ElementNames::MemberSummary, OptionalOne> MemberSummary;
            typedef Element<ElementNames::MemberName, One> MemberName;
            typedef Element<ElementNames::MemberType, One > MemberType;
            typedef Element<ElementNames::Member, AtLeastOne, boost::mpl::vector<MemberSummary, MemberName, MemberType, Choice<ArraySize, ArraySizeRef>, Choice<MaxLength, MaxLengthRef> > > Member;
            typedef Element<ElementNames::ClassMembers, OptionalOne, boost::mpl::vector<Member> > Members;

            typedef Element<ElementNames::ParameterSummary, OptionalOne> ParameterSummary;
            typedef Element<ElementNames::ParameterName, One> ParameterName;
            typedef Element<ElementNames::ParameterType, One > ParameterType;
            typedef Element<ElementNames::ParameterValue, One> ParameterValue;
            typedef Element<ElementNames::EntityId, One, boost::mpl::vector< Ignore<ElementNames::ClassName>, Ignore<ElementNames::InstanceId> > > ParameterEntityId;
            typedef Ignore<ElementNames::Object> ParameterObject;
            typedef Element<ElementNames::ParameterArrayElement, AtLeastOne, boost::mpl::vector< ELEMENT_CHOICE_3(ParameterValue, ParameterEntityId, ParameterObject) >, ObjectParameterHandler> ParameterArrayElement;
            typedef Element<ElementNames::ParameterArrayElements, One, boost::mpl::vector<ParameterArrayElement> > ParameterArrayElements;
            typedef Element<ElementNames::Parameter, AtLeastOne, boost::mpl::vector<ParameterSummary, ParameterName, ParameterType, ELEMENT_CHOICE_4(ParameterValue, ParameterEntityId, ParameterObject, ParameterArrayElements) > > Parameter;
            typedef Element<ElementNames::Parameters, OptionalOne, boost::mpl::vector<Parameter> > Parameters;

            typedef Element<ElementNames::CreateRoutineSummary, OptionalOne> CreateRoutineSummary;
            typedef Element<ElementNames::CreateRoutineValueMember, One> CreateRoutineValueMember;
            typedef Element<ElementNames::CreateRoutineValueParameter, One> CreateRoutineValueParameter;
            typedef Element<ElementNames::CreateRoutineValue, AtLeastOne, boost::mpl::vector<CreateRoutineValueMember, CreateRoutineValueParameter> > CreateRoutineValue;
            typedef Element<ElementNames::CreateRoutineValues, OptionalOne, boost::mpl::vector<CreateRoutineValue> > CreateRoutineValues;
            typedef Element<ElementNames::CreateRoutineName, One> CreateRoutineName;
            typedef Element<ElementNames::CreateRoutineMemberName, AtLeastOne > CreateRoutineMember;
            typedef Element<ElementNames::CreateRoutineParameterList, One, boost::mpl::vector<CreateRoutineMember> > CreateRoutineParameters;
            typedef Element<ElementNames::CreateRoutine, AtLeastOne, boost::mpl::vector<CreateRoutineSummary, CreateRoutineName, CreateRoutineParameters, CreateRoutineValues> > CreateRoutine;
            typedef Element<ElementNames::CreateRoutines, One, boost::mpl::vector<CreateRoutine> > CreateRoutines;

            //Top level
            typedef Element<ElementNames::Class, One, boost::mpl::vector<Ignore<ElementNames::XmlAttribute>, ClassSummary, ClassName, BaseClass, Members, CreateRoutines, Parameters> > ClassParser;
            typedef Element<ElementNames::Enumeration, One, boost::mpl::vector<Ignore<ElementNames::XmlAttribute>, EnumerationSummary, EnumerationName, EnumerationValues> > EnumerationParser;
            typedef Element<ElementNames::Exception, One, boost::mpl::vector<Ignore<ElementNames::XmlAttribute>, ExceptionSummary, ExceptionName, ExceptionBase> > ExceptionParser;
            typedef Element<ElementNames::Property, One, boost::mpl::vector<Ignore<ElementNames::XmlAttribute>, PropertySummary, ExceptionName, ExceptionBase> > PropertyParser;
        }
    }

    using namespace InternalDefs::SubParsers;
    typedef ELEMENT_CHOICE_3(ClassParser, EnumerationParser, ExceptionParser) DobUnitParser;
}
}
}
}

#endif
