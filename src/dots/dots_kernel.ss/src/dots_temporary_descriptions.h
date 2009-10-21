/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://www.safirsdk.com)
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

/***************************************************************
  Purpose:      Description classes for classes, members and
                parameters used to temporary store the info
                read from xml-files before the information is
                stored in shared memory.
***************************************************************/

#ifndef _dots_temporary_descriptions_h
#define _dots_temporary_descriptions_h

#include <functional>
#include <string>
#include <set>
#include <iostream>
#include <boost/filesystem.hpp>
#include <Safir/Dob/Typesystem/Internal/Id.h>


namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    //---------------------------------------------------------------
    // Classes used for temporary storage when parsing the XML-files
    //--------------------------------------------------------------

    class DobParameter
    {
    public:
        std::string m_name;
        MemberType m_type;
        TypeId m_objType; //only if m_type is object or enum

        struct ParameterValue
        {
            bool m_isNull;
            bool m_indexFromParameter;
            int m_index;
            std::string m_indexParameter;
            bool m_valueFromParameter;
            std::string m_value;

            //For debug error outputs
            int m_lineNumber;
        };

        std::vector<ParameterValue> m_values;

        //For debug error outputs
        boost::filesystem::path m_file;
    };

    typedef std::vector<DobParameter> DobParameters;

    class DobMember
    {
    public:
        std::string m_name;         //member name
        MemberType m_type;              //member type
        Int32 m_dataLength;  //valid if m_type=string
        Int32 m_arrayLength;   //m_arrayLength=1 -> normal member, m_arrayLength>1 -> array
        bool m_strLenFromParameter;
        std::string m_strLenParameter;
        bool m_arrSizeFromParameter;
        std::string m_arrSizeParameter;
        TypeId m_objType;               //valid if m_type=object
        int m_class;                    //valid if m_type=object
        bool m_propertyIsArray;         //Only valid for properties
    };

    typedef std::vector<DobMember> DobMembers;


    namespace SimpleDOM
    {
        struct Element; //forward declaration

        typedef std::vector<Element> Tree;

        struct Element
        {
            Element(const std::string & name, int line):m_Name(name),m_Value(),m_bHaveValue(false),m_pParent(NULL),m_Children(), m_lineNumber(line) {}

            std::string m_Name; //No name means that it has not been set
            std::string m_Value; //Illegal to have both value and children
            bool m_bHaveValue;
            Element * m_pParent;
            Tree m_Children;
            int m_lineNumber;
        };

        class NameEquals: public std::binary_function<Element,std::string,bool>
        {
        public:
            bool operator() (const Element & el, const std::string & name) const {return el.m_Name == name;}
        };
    }

    namespace Temporary
    {
        struct MemberReferenceElement
        {
            std::string m_classMember;
            std::string m_index;
        };
        typedef std::vector <MemberReferenceElement> ClassMemberReference;

        struct BinaryMemberReferenceElement
        {
            MemberIndex m_classMember;
            int m_index; //-1 means "point to whole array"
        };
        typedef std::vector <BinaryMemberReferenceElement> BinaryClassMemberReference;

        inline std::ostream & operator << (std::ostream & out, const ClassMemberReference & reference)
        {
            for (ClassMemberReference::const_iterator it = reference.begin();
                 it != reference.end(); ++it)
            {
                out << it->m_classMember;
                if (!it->m_index.empty())
                {
                    out << "["
                        << it->m_index
                        << "]";
                }

                if (it+1!=reference.end())
                {
                    out << ".";
                }
            }
            return out;
        }

        enum MappingKind
        {
            NullMapping,
            ClassMemberReferenceMapping,
            ParameterMapping,
        };

        struct MappingMember
        {
            MappingMember(const std::string & propertyMemberName):
                m_propertyMemberName(propertyMemberName),
                m_kind(NullMapping),
                m_classMemberReference(),
                //                m_value(),
                m_propertyMemberIndex(-1)
            {}
                         //, m_propertyMemberIndex(0),
                         //                m_classMemberName(), m_classMemberIndex(0) {}

            std::string m_propertyMemberName;
            MappingKind m_kind;
            ClassMemberReference m_classMemberReference;
            DobParameter m_parameter;

            //filled out by the finalization:
            int m_propertyMemberIndex;
            BinaryClassMemberReference m_binaryClassMemberReference;
        };

        typedef std::vector<MappingMember> MappingMembers;

    }

    class DobPropertyMapping
    {
    public:
        DobPropertyMapping(const boost::filesystem::path & filename):
            m_propertyName(), m_propertyTypeId(0),
            m_className(),    m_classTypeId(0),
            m_memberMappingsDOM(),
            m_propertyIndex(0),
            m_classIndex(0),
            m_mappings(),
            m_filename(filename){}

        //Filled out by the parser
        std::string m_propertyName;
        TypeId m_propertyTypeId;

        std::string m_className;
        TypeId m_classTypeId;

        SimpleDOM::Tree m_memberMappingsDOM;

        //Completed by the finalize method
        int m_propertyIndex;
        int m_classIndex;


        Temporary::MappingMembers m_mappings;

        boost::filesystem::path m_filename;
    };

    typedef std::vector<DobPropertyMapping> DobPropertyMappings;

    class DobClass
    {
    public:
        TypeId m_typeId;
        std::string m_name; //including namespace ex: MyNamespace1.MyNamespace2.MyClass
        std::string m_baseClassName;
        int m_baseClassIndex;
        std::vector<int> m_desc;
        TypeId m_baseClassTypeId;
        unsigned int m_initialSize; //size of whole blob (with base classes taken into account)
        unsigned int m_thisClassSize; //size of only the members in this class (object will get the header size)
        bool m_sizeCalculated;
        unsigned int m_noInheritedMembers;
        DobMembers m_members;
        unsigned int m_noInheritedParameters;
        DobParameters m_parameters;
        bool m_complex;
        DobPropertyMappings m_propertyMappings;
    };

    typedef std::vector<DobClass> DobClasses;

    class DobProperty
    {
    public:
        TypeId m_typeId;
        std::string m_name; //including namespace ex: MyNamespace1.MyNamespace2.MyClass
        DobMembers m_members;
        bool m_complex;
    };

    typedef std::vector<DobProperty> DobProperties;

    struct DobEnumeration
    {
        TypeId m_typeId;
        TypeId m_checkSum;
        std::string m_name;
        std::vector<std::string> m_values;
    };

    typedef std::vector<DobEnumeration> DobEnumerations;

    struct DobException
    {
        DobException(const std::string& name, 
                     const std::string& baseClass):
            m_name(name),
            m_baseClass(baseClass),
            m_typeId(DotsId_Generate64(name.c_str())),
            m_baseClassTypeId(baseClass.empty()?0:DotsId_Generate64(baseClass.c_str())) {}

        bool operator<(const DobException& other) const {return m_name < other.m_name;}
        
        const std::string m_name;
        const std::string m_baseClass;
        const TypeId      m_typeId;
        const TypeId      m_baseClassTypeId;

    private:
        void operator=(const DobException&) const;
    };

    typedef std::set<DobException> DobExceptions;
}
}
}
}
#endif
