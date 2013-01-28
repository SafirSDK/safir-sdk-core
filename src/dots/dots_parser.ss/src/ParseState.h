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
#ifndef __PARSESTATE_H__
#define __PARSESTATE_H__

#include <boost/noncopyable.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/unordered_map.hpp>
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include "ParseResult.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    //Compare a field in an object to a value. Return true if equal else false.
    template <class ObjT, class FieldT>
    bool FieldCmp(const ObjT& obj, const FieldT& val, boost::function<const FieldT&(const ObjT&)> field)
    {
        return val == field(obj);
    }

    //Get first object in vector that have specified field equal to value.
    template <class ObjT, class FieldT>
    ObjT* GetByFieldValue(std::vector<ObjT>& v, const FieldT& val, boost::function<const FieldT&(const ObjT&)> fieldToCmp)
    {
        for (typename std::vector<ObjT>::iterator it=v.begin(); it!=v.end(); ++it)
        {
            if (val==fieldToCmp(*it))
            {
                return &(*it);
            }
        }
        return NULL;
    }

    //Keeps the current state of an ongoing parse job.
    //Also offers convenient methods for finding stuff in the ParseResult.
    struct ParseState : public boost::noncopyable
    {
        struct ParameterReference
        {
            std::string fileName;
            size_t topIndex, subIndex1, subIndex2, subIndex3;
            std::string parameterName;
            int parameterIndex;

            ParameterReference(const std::string file) : fileName(file) {}
        };
        typedef std::vector<ParameterReference> ParameterReferenceVector;

        std::string currentPath;
        RawParseResultPtr result;
        ParameterReferenceVector paramToParamReferences;
        ParameterReferenceVector arraySizeReferences;
        ParameterReferenceVector maxLengthReferences;
        ParameterReferenceVector createRoutineValueReferences;

        typedef boost::unordered_map<DotsC_TypeId, size_t> IndexHashTable;
        IndexHashTable index;

        explicit ParseState(const RawParseResultPtr& r) : result(r)
        {
            result->classes.clear();
            result->enumerations.clear();
            result->exceptions.clear();
            result->properties.clear();
            result->propertyMappings.clear();

            ClassDefinition obj;
            obj.name="Object";
            obj.fileName="Object.dou";
            result->classes.push_back(obj);
            index[DotsId_Generate64(obj.name.c_str())]=0;
        }

        template <class T>
        T* GetDefinition(std::vector<T>& v, const std::string& name) const
        {
            IndexHashTable::const_iterator it = index.find(DotsId_Generate64(name.c_str()));
            if (it!=index.end())
                return &v[it->second];
            return NULL;
        }

        ClassDefinition* GetClass(const std::string& name) const {return GetDefinition(result->classes, name);}

        EnumerationDefinition* GetEnum(const std::string& name) const { return GetDefinition(result->enumerations, name);}
        PropertyDefinition* GetProperty(const std::string& name) const { return GetDefinition(result->properties, name);}
        ExceptionDefinition* GetException(const std::string& name) const { return GetDefinition(result->exceptions, name);}

        MemberDefinition* GetMember(ClassDefinition* cd, const std::string name) const
        {
            MemberDefinition* md = GetByFieldValue<MemberDefinition, std::string>(cd->members, name, boost::bind(&MemberDefinition::name,_1));
            if (!md)
            {
                ClassDefinition* base = GetClass(cd->baseClass);
                if (base)
                {
                    return GetMember(base, name);
                }

                return NULL;
            }

            return md;
        }

        ParameterDefinition* GetParameter(const std::string& name) const
        {
            size_t dotIx = name.find('#'); //if parameter we are looking for is hidden (i.e value-mapping), this is what we should find
            if (dotIx!=std::string::npos)
            {
                --dotIx;
            }
            else
            {
                //we are looking for a normal parameter.
                dotIx=name.find_last_of('.');
            }

            std::string cls, par;
            if (dotIx!=std::string::npos)
            {
                cls=name.substr(0, dotIx);
                par=name.substr(dotIx+1);

                ClassDefinition* cd = GetByFieldValue<ClassDefinition, std::string>(result->classes, cls, boost::bind(&ClassDefinition::name,_1));
                if (cd)
                {
                    ParameterDefinition* pd = GetByFieldValue<ParameterDefinition, std::string>(cd->parameters, par, boost::bind(&ParameterDefinition::name,_1));
                    return pd;
                }
            }

            return NULL;
        }
    };


}
}
}
}

#endif // __PARSESTATE_H__
