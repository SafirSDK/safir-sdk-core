/******************************************************************************
*
* Copyright Saab AB, 2004-2012 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / joot
*
* $HeadURL:  $
* $Revision:  $
* $LastChangedBy:  $
* $LastChangedDate: $
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

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    typedef std::vector<std::string> StringVector;

    //---------------------------------------------------
    // Sub units - these are part of the top level units
    //---------------------------------------------------
    /**
     * Definition of a parameter.
     */
    struct ParameterDefinition
    {
        std::string Summary;
        std::string Name;
        std::string TypeName;
        DotsC_MemberType MemberType;
        bool IsArray;

        //Values will contain the parameter value as a string.
        //- If type is 'String' the value can be grabbed as is.
        //- For all numerical types, i.e Int32, Int64, Float64, SI-types, it is guaranteed safe to cast the string to corresponding
        //  DotsC_type specified in Safir/Dob/Typesystem/Internal/KernelDefs.h by using boost::lecical_cast<DotsC_T, std::string>(Values[i]);
        //- Objects are still xml-serialized since dots_parser knows noting about blobs.
        //- InstanceId can be either a its string representation or Int64 value, anything goes. Must be handled by user. For example by
        //  using boost::lexical_cast<DotsC_Int64, std::string> and catch boost::bad_lexical_cast.
        //- EntityId are returned as a string on the same format as SATE is using, i.e "typeName : instanceId".
        StringVector Values; //if not IsArray is false, Values contains only one value

        ParameterDefinition() : IsArray(false) {}
    };
    typedef std::vector<ParameterDefinition> ParameterDefinitions;

    /**
     * Definition of a class or property member.
     */
    struct MemberDefinition
    {
        std::string Summary;
        std::string Name;
        std::string TypeName;
        DotsC_MemberType MemberType;
        bool IsArray;
        int ArraySize; //If IsArray and ArraySize<0 its an dynamic array.
        int MaxLength; //Max string length. Only applicable if TypeName is 'String'.

        MemberDefinition() : IsArray(false), ArraySize(0), MaxLength(0) {}
    };
    typedef std::vector<MemberDefinition> MemberDefinitions;   

    /**
     * Definition of a create routine, similair to a constructor.
     */
    typedef std::pair<std::string, std::string> MemberValue;
    typedef std::vector<MemberValue> MemberValueVector;
    struct CreateRoutineDefinition
    {   
        std::string Summary;
        std::string Name;
        StringVector Parameters;        
        MemberValueVector MemberValues;
    };
    typedef std::vector<CreateRoutineDefinition> CreateRoutineDefinitions;

    /**
     * Definition of a mapping between a property member and a parameter value,
     * class member, or null.
     */
    typedef DotsC_PropertyMappingKind MappingKind;
    typedef std::pair<std::string, int> MemberReference; //pair <Member, Index>
    typedef std::vector<MemberReference> MemberReferenceVector;
    struct MappedMemberDefinition
    {        
        std::string Name; //Name of the member.
        MappingKind Kind;
        std::string Value;
        MemberReferenceVector MemberReferences; //pair<MemberName, index>
    };
    typedef std::vector<MappedMemberDefinition> MappedMemberDefinitions;

    //-------------------------------------------------
    // Top level units
    //-------------------------------------------------
    /**
     * Definition of a class.
     */
    struct ClassDefinition
    {
        std::string Summary;
        std::string FileName;
        std::string Name;
        std::string BaseClass;
        ParameterDefinitions Parameters;
        MemberDefinitions Members;
        CreateRoutineDefinitions CreateRoutines;
    };
    typedef std::vector<ClassDefinition> ClassDefinitions;

    /**
     * Definition of an exception type.
     */
    struct ExceptionDefinition
    {
        std::string Summary;
        std::string FileName;
        std::string Name;
        std::string BaseClass;
    };
    typedef std::vector<ExceptionDefinition> ExceptionDefinitions;

    /**
     * Definition of an enumeration type.
     */
    struct EnumerationDefinition
    {
        std::string Summary;
        std::string FileName;
        std::string Name;
        StringVector EnumerationValues;
    };
    typedef std::vector<EnumerationDefinition> EnumerationDefinitions;

    /**
     * Definition of a property.
     */
    struct PropertyDefinition
    {
        std::string Summary;
        std::string FileName;
        std::string Name;
        MemberDefinitions Members;
    };
    typedef std::vector<PropertyDefinition> PropertyDefinitions;

    /**
     * Definition of a property mapping.
     */
    struct PropertyMappingDefinition
    {
        std::string Summary;
        std::string FileName;
        std::string ClassName;
        std::string PropertyName;
        MappedMemberDefinitions MappedMembers;
    };
    typedef std::vector<PropertyMappingDefinition> PropertyMappingDefinitions;

    //--------------------------------------------------------------------

    /**
     * Container class for at complete parse result.
     * This is the result type returned from TypeDefinitionParser.Parse.
     */
    struct RawParseResult
    {
        ClassDefinitions Classes;
        EnumerationDefinitions Enumerations;
        ExceptionDefinitions Exceptions;
        PropertyDefinitions Properties;
        PropertyMappingDefinitions PropertyMappings;
    };
    typedef boost::shared_ptr<RawParseResult> RawParseResultPtr;
    typedef boost::shared_ptr<const RawParseResult> RawParseResultConstPtr;
    
    /**
     * Exception used to report errors in dou- and dom- files.
     */
    class ParseError : public std::exception
    {
    public:
        ParseError(const std::string& label, const std::string& description, const std::string& file) :
          m_label(label),
              m_description(description),
              m_file(file){}

        const std::string& Label() const throw() {return m_label;}
        const std::string& Description() const throw() {return m_description;}
        const std::string& File() const throw() {return m_file;}

        virtual const char* what () const throw (){return m_description.c_str();}

    private:
        std::string m_label, m_description, m_file;
    };
}
}
}
} //end namespace Safir::Dob::Typesystem

#endif
