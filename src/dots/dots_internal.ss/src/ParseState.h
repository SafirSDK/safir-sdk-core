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
#ifndef __DOTS_INTERNAL_PARSESTATE_H__
#define __DOTS_INTERNAL_PARSESTATE_H__

#include <iostream>
#include <boost/noncopyable.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <Safir/Utilities/Internal/Id.h>
#include "RepositoryLocal.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{    
    //----------------------------------------------------------------------------------
    // The class ParseState keeps the current state of an ongoing parse job. Each
    // worker-thread in a ParseJob has its own ParseState.
    // This file also contains convenient methods for finding stuff in the ParseResult.
    //----------------------------------------------------------------------------------
    struct ParseState : public boost::noncopyable
    {
        //-------------------------------------------------------------------------
        // Types used for to keep references to stuff that has to be handled later
        //-------------------------------------------------------------------------
        //General reference to a member, parameter or createRoutine
        template <class SubItem>
        struct Reference
        {
            ClassDescriptionLocal* referencingClass;    //referencing class
            SubItem* referencingItem; //referencing member or parameter or createRoutine
            size_t referencingIndex; //referencing parameter index if applicable
            Reference() {}
            Reference(const ClassDescriptionLocalPtr& cl,
                      const boost::shared_ptr<SubItem>& sub,
                      size_t ix)
                :referencingClass(cl.get())
                ,referencingItem(sub.get())
                ,referencingIndex(ix)
            {
            }
        };

        //Used when some element is referencing a parameter
        template <class ReferencedSubItem>
        struct ParameterReference
        {
            Reference<ReferencedSubItem> referee;
            std::string parameterName; //referenced parameter
            std::string parameterKey; //referenced parameter key or index

            ParameterReference() {}
            ParameterReference(const ClassDescriptionLocalPtr& referencingClass,
                               const boost::shared_ptr<ReferencedSubItem>& referencingSubItem,
                               size_t referencingIndex,
                               const std::string& paramName,
                               const std::string& paramKey)
                :referee(referencingClass, referencingSubItem, referencingIndex)
                ,parameterName(paramName)
                ,parameterKey(paramKey)
            {
            }
        };

        //An object parameter that cant be verified until we got all type descriptions ready.
        //This is an optimization so we dont have to copy the entire object-XML into a string.
        //Handling serialized object parameters is very time consuming.
        struct ObjectParameter
        {            
            Reference<ParameterDescriptionLocal> referee;
            boost::property_tree::ptree* obj;
            boost::shared_ptr<boost::property_tree::ptree> propertyTree; //prevent from destruction
            bool deprecatedXmlFormat;
            ObjectParameter(){}
            ObjectParameter(const ClassDescriptionLocalPtr& class_,
                            const ParameterDescriptionLocalPtr& param,
                            size_t paramArrayIndex,
                            boost::property_tree::ptree* obj_,
                            const boost::shared_ptr<boost::property_tree::ptree>& pt)
                :referee(class_, param, paramArrayIndex)
                ,obj(obj_)
                ,propertyTree(pt)
                ,deprecatedXmlFormat(false)
            {
            }
        };

        //-------------------------------------------------------------------------
        // Fields
        //-------------------------------------------------------------------------
        mutable std::string currentPath; //current full path filename
        boost::shared_ptr<boost::property_tree::ptree> propertyTree; //the currently parsed propertyTree
        boost::shared_ptr<RepositoryLocal> repository;

        //Pointers to quickly access the unit we are currently parsing.
        ClassDescriptionLocalPtr lastInsertedClass;
        EnumDescriptionLocalPtr lastInsertedEnum;
        PropertyDescriptionLocalPtr lastInsertedProperty;
        ExceptionDescriptionLocalPtr lastInsertedException;
        PropertyMappingDescriptionLocalPtr lastInsertedPropertyMapping;
        MemberMappingLocalPtr lastInsertedMemberMapping;

        //Here we store references to stuff that must be resolved after all dou-files have been parsed
        std::vector< ParameterReference<ParameterDescriptionLocal> > paramToParamReferences;
        std::vector< ParameterReference<MemberDescriptionLocal> >  arraySizeReferences;
        std::vector< ParameterReference<MemberDescriptionLocal> > maxLengthReferences;
        std::vector< ParameterReference<CreateRoutineDescriptionLocal> > createRoutineIncompleteHiddenParameters;
        std::vector<ObjectParameter> objectParameters; //object parameters are handled when all type info is available.
        std::vector<PropertyMappingDescriptionLocalPtr> notInsertedPropertyMappings; //PropertyMappings that has not been inserted into its class
        std::vector< std::pair<ClassDescriptionLocal*, ParameterDescriptionLocalPtr> > notInsertedParameters;

        //----------------------------------------------------------------------------------
        //Constructor
        //----------------------------------------------------------------------------------
        ParseState(const boost::shared_ptr<RepositoryLocal>& rep)
            :repository(rep)
        {
        }
    };
    typedef boost::shared_ptr<ParseState> ParseStatePtr; //Ptr to ParseState
}
}
}
}

#endif
