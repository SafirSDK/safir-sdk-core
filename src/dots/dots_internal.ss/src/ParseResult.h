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
#ifndef __DOTS_PARSE_RESULT_H__
#define __DOTS_PARSE_RESULT_H__

#include <string>
#include <vector>
#include <boost/shared_ptr.hpp>
#include <Safir/Dob/Typesystem/Internal/KernelDefs.h>
#include <Safir/Dob/Typesystem/Internal/ParseError.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    typedef std::vector<std::string> StringVector;
    typedef DotsC_TypeId TypeId;
    typedef DotsC_MemberType MemberType;
    typedef DotsC_PropertyMappingKind MappingKind;



    //---------------------------------------------------
    // Sub units - these are part of the top level units
    //---------------------------------------------------
    /**
     * Definition of a value. Used in parameters, createRoutines and propertyMappings.
     */
    struct ValueDefinition
    {
        bool isNull;
        std::string stringVal;
        union
        {
            DotsC_Int32 int32Val;
            DotsC_Int64 int64Val;
            DotsC_Float32 float32Val;
            DotsC_Float64 float64Val;
            DotsC_EntityId entityIdVal;
            bool boolVal;
        };

        ValueDefinition() : isNull(true){}
        explicit ValueDefinition(const std::string& strVal) : isNull(false), stringVal(strVal) {}
    };
    typedef std::vector<ValueDefinition> ParameterValues;

    /**
     * Definition of a parameter.
     */
    struct ParameterDefinition
    {
        std::string summary;
        std::string name;
        std::string typeName;
        MemberType memberType;
        bool isArray;
        bool hidden;   //Some parameters are derived from propertyMapping values. The parser will automatically generate a
                        //hidden parameter for those values. All explicitly declared parameters will have hidden=false.

        //Values will contain the parameter value as a string.
        //- If type is 'String' the value can be grabbed as is.
        //- For all numerical types, i.e Int32, Int64, Float64, SI-types, it is guaranteed safe to cast the string to corresponding
        //  DotsC_type specified in Safir/Dob/Typesystem/Internal/KernelDefs.h by using boost::lecical_cast<DotsC_T, std::string>(Values[i]);
        //- Objects are still xml-serialized since dots_parser knows noting about blobs.
        //- InstanceId can be either a its string representation or Int64 value, anything goes. Must be handled by user. For example by
        //  using boost::lexical_cast<DotsC_Int64, std::string> and catch boost::bad_lexical_cast.
        //- EntityId are returned as a string on on format "typeName, instanceId". Example "Safir.Dob.Entity, 123"
        //StringVector values; //if not isArray is false, Values contains only one value
        ParameterValues values;

        ParameterDefinition() : isArray(false), hidden(false) {}
    };
    typedef std::vector<ParameterDefinition> ParameterDefinitions;

    /**
     * Definition of a class or property member.
     */
    struct MemberDefinition
    {
        std::string summary;
        std::string name;
        std::string typeName;
        MemberType memberType;
        bool isArray;
        int arraySize; //If isArray and arraySize<0 its an dynamic array.
        int maxLength; //Max string length. Only applicable if typeName is 'String'.

        MemberDefinition() : isArray(false), arraySize(0), maxLength(0) {}
    };
    typedef std::vector<MemberDefinition> MemberDefinitions;

    /**
     * Definition of a mapping between a property member and a parameter value,
     * class member, or null.
     */
    typedef std::pair<std::string, int> MemberReference; //pair<Member, Index> or pair<Parameter, Index>
    typedef std::vector<MemberReference> MemberReferenceVector;
    struct MappedMemberDefinition
    {        
        std::string name; //Name of the member.
        MappingKind kind;

        //if Kind=MappedToMember -> pair<MemberName, index>
        //if Kind=MappedToParameter -> pair<Parameter, index>
        MemberReferenceVector memberReferences;
    };
    typedef std::vector<MappedMemberDefinition> MappedMemberDefinitions;

    /**
     * Definition of a create routine, similair to a constructor.
     */
    typedef std::pair<std::string, MemberReference> MemberValue; //pair <memberName, pair<Parameter,Index> >
    typedef std::vector<MemberValue> MemberValueVector;
    struct CreateRoutineDefinition
    {
        std::string summary;
        std::string name;
        StringVector parameters;
        MemberValueVector memberValues;
    };
    typedef std::vector<CreateRoutineDefinition> CreateRoutineDefinitions;

    //-------------------------------------------------
    // Top level units
    //-------------------------------------------------
    /**
     * Definition of a class.
     */
    struct ClassDefinition
    {
        std::string summary;
        std::string fileName;
        std::string name;
        std::string baseClass;
        ParameterDefinitions parameters;
        MemberDefinitions members;
        CreateRoutineDefinitions createRoutines;
    };
    typedef std::vector<ClassDefinition> ClassDefinitions;

    /**
     * Definition of an exception type.
     */
    struct ExceptionDefinition
    {
        std::string summary;
        std::string fileName;
        std::string name;
        std::string baseClass;
    };
    typedef std::vector<ExceptionDefinition> ExceptionDefinitions;

    /**
     * Definition of an enumeration type.
     */
    struct EnumerationDefinition
    {
        std::string summary;
        std::string fileName;
        std::string name;
        StringVector enumerationValues;
    };
    typedef std::vector<EnumerationDefinition> EnumerationDefinitions;

    /**
     * Definition of a property.
     */
    struct PropertyDefinition
    {
        std::string summary;
        std::string fileName;
        std::string name;
        MemberDefinitions members;
    };
    typedef std::vector<PropertyDefinition> PropertyDefinitions;

    /**
     * Definition of a property mapping.
     */
    struct PropertyMappingDefinition
    {
        std::string summary;
        std::string fileName;
        std::string className;
        std::string propertyName;
        MappedMemberDefinitions mappedMembers;
    };
    typedef std::vector<PropertyMappingDefinition> PropertyMappingDefinitions;

    //--------------------------------------------------------------------

    /**
     * Container class for at complete parse result.
     * This is the result type returned from TypeDefinitionParser.Parse.
     */
    struct RawParseResult
    {
        ClassDefinitions classes;
        EnumerationDefinitions enumerations;
        ExceptionDefinitions exceptions;
        PropertyDefinitions properties;
        PropertyMappingDefinitions propertyMappings;
    };
    typedef boost::shared_ptr<RawParseResult> RawParseResultPtr;
    typedef boost::shared_ptr<const RawParseResult> RawParseResultConstPtr;
}
}
}
} //end namespace Safir::Dob::Typesystem

#endif
