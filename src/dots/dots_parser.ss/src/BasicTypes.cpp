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
#include <boost/lexical_cast.hpp>
#include <boost/cstdint.hpp>
#include <boost/bind.hpp>
#include "BasicTypes.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    const std::string BasicTypes::ObjectName = "Object";
    const std::string BasicTypes::ExceptionName = "Exception";
    const std::string BasicTypes::FundamentalExceptionName = "FundamentalException";

    template <class T>
    bool NameComparer(const T& obj, const std::string& name)
    {
        return obj.Name==name;
    }

    template <class T> struct ValueChecker
    {
        bool operator()(const std::string& val, RawParseResultConstPtr) const
        {
            try 
            {
                boost::lexical_cast<T, std::string>(val);
            }
            catch(boost::bad_lexical_cast&) 
            {
                return false;
            } 
            return true;
        }
    };    

    template<> struct ValueChecker<std::string>
    {
        bool operator()(const std::string&, RawParseResultConstPtr) const {return true;}
    };

    template<> struct ValueChecker<bool>
    {
        bool operator()(const std::string& val, RawParseResultConstPtr) const
        {
            return val=="True" || val=="true" || val=="False" || val=="false";
        }
    };

    const BasicTypes& BasicTypes::Instance()
    {
        static BasicTypes instance;
        return instance;
    }

    BasicTypes::BasicTypes(void) : m_typeInfo()
    {        
        ValueChecker<boost::int32_t> int32Checker;
        ValueChecker<boost::int64_t> int64Checker;
        ValueChecker<float> float32Checker;
        ValueChecker<double> float64Checker;
        ValueChecker<bool> boolChecker;
        ValueChecker<std::string> noCheck;
              
        m_typeInfo.push_back( TypeInfo(EnumerationMemberType, "Enumeration", noCheck) );        
        m_typeInfo.push_back( TypeInfo(TypeIdMemberType, "TypeId", noCheck));
        m_typeInfo.push_back( TypeInfo(EntityIdMemberType, "EntityId", noCheck));
        m_typeInfo.push_back( TypeInfo(BooleanMemberType, "Boolean", boolChecker));
        m_typeInfo.push_back( TypeInfo(Int32MemberType, "Int32", int32Checker));
        m_typeInfo.push_back( TypeInfo(Int64MemberType, "Int64", int64Checker));
        m_typeInfo.push_back( TypeInfo(Float32MemberType, "Float32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Float64MemberType, "Float64", float64Checker));
        m_typeInfo.push_back( TypeInfo(InstanceIdMemberType, "InstanceId", noCheck));
        m_typeInfo.push_back( TypeInfo(ChannelIdMemberType, "ChannelId", noCheck));
        m_typeInfo.push_back( TypeInfo(HandlerIdMemberType, "HandlerId", noCheck));
        m_typeInfo.push_back( TypeInfo(StringMemberType, "String", noCheck));
        m_typeInfo.push_back( TypeInfo(ObjectMemberType, "Object", noCheck));
        m_typeInfo.push_back( TypeInfo(BinaryMemberType, "Binary", noCheck));
        m_typeInfo.push_back( TypeInfo(Ampere32MemberType, "Ampere32", float32Checker));
        m_typeInfo.push_back( TypeInfo(CubicMeter32MemberType, "CubicMeter32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Hertz32MemberType, "Hertz32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Joule32MemberType, "Joule32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Kelvin32MemberType, "Kelvin32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Kilogram32MemberType, "Kilogram32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Meter32MemberType, "Meter32", float32Checker));
        m_typeInfo.push_back( TypeInfo(MeterPerSecond32MemberType, "MeterPerSecond32", float32Checker));
        m_typeInfo.push_back( TypeInfo(MeterPerSecondSquared32MemberType, "MeterPerSecondSquared32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Newton32MemberType, "Newton32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Pascal32MemberType, "Pascal32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Radian32MemberType, "Radian32", float32Checker));
        m_typeInfo.push_back( TypeInfo(RadianPerSecond32MemberType, "RadianPerSecond32", float32Checker));
        m_typeInfo.push_back( TypeInfo(RadianPerSecondSquared32MemberType, "RadianPerSecondSquared32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Second32MemberType, "Second32", float32Checker));
        m_typeInfo.push_back( TypeInfo(SquareMeter32MemberType, "SquareMeter32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Steradian32MemberType, "Steradian32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Volt32MemberType, "Volt32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Watt32MemberType, "Watt32", float32Checker));
        m_typeInfo.push_back( TypeInfo(Ampere64MemberType, "Ampere64", float64Checker));
        m_typeInfo.push_back( TypeInfo(CubicMeter64MemberType, "CubicMeter64", float64Checker));
        m_typeInfo.push_back( TypeInfo(Hertz64MemberType, "Hertz64", float64Checker));
        m_typeInfo.push_back( TypeInfo(Joule64MemberType, "Joule64", float64Checker));
        m_typeInfo.push_back( TypeInfo(Kelvin64MemberType, "Kelvin64", float64Checker));
        m_typeInfo.push_back( TypeInfo(Kilogram64MemberType, "Kilogram64", float64Checker));
        m_typeInfo.push_back( TypeInfo(Meter64MemberType, "Meter64", float64Checker));
        m_typeInfo.push_back( TypeInfo(MeterPerSecond64MemberType, "MeterPerSecond64", float64Checker));
        m_typeInfo.push_back( TypeInfo(MeterPerSecondSquared64MemberType, "MeterPerSecondSquared64", float64Checker));
        m_typeInfo.push_back( TypeInfo(Newton64MemberType, "Newton64", float64Checker));
        m_typeInfo.push_back( TypeInfo(Pascal64MemberType, "Pascal64", float64Checker));
        m_typeInfo.push_back( TypeInfo(Radian64MemberType, "Radian64", float64Checker));
        m_typeInfo.push_back( TypeInfo(RadianPerSecond64MemberType, "RadianPerSecond64", float64Checker));
        m_typeInfo.push_back( TypeInfo(RadianPerSecondSquared64MemberType, "RadianPerSecondSquared64", float64Checker));
        m_typeInfo.push_back( TypeInfo(Second64MemberType, "Second64", float64Checker));
        m_typeInfo.push_back( TypeInfo(SquareMeter64MemberType, "SquareMeter64", float64Checker));
        m_typeInfo.push_back( TypeInfo(Steradian64MemberType, "Steradian64", float64Checker));
        m_typeInfo.push_back( TypeInfo(Volt64MemberType, "Volt64", float64Checker));
        m_typeInfo.push_back( TypeInfo(Watt64MemberType, "Watt64", float64Checker));
    }

    const BasicTypes::TypeInfo* BasicTypes::GetTypeInfo(MemberType mt) const
    {
        for (TypeInfoVector::const_iterator it=m_typeInfo.begin(); it!=m_typeInfo.end(); ++it)
        {
            if (it->Type==mt)
                return &(*it);
        }
        return NULL;
    }

    const BasicTypes::TypeInfo* BasicTypes::GetTypeInfo(const std::string& typeName) const
    {
        for (TypeInfoVector::const_iterator it=m_typeInfo.begin(); it!=m_typeInfo.end(); ++it)
        {
            if (it->Name==typeName)
                return &(*it);
        }
        return NULL;
    }

    bool BasicTypes::MemberTypeOf(const std::string& typeName, RawParseResultConstPtr res, DotsC_MemberType& memberType) const
    {
        const TypeInfo* ti = GetTypeInfo(typeName);
        if (ti!=NULL)
        {
            memberType=ti->Type;
            return true;
        }
        else if (std::find_if(res->Classes.begin(), res->Classes.end(), boost::bind(NameComparer<ClassDefinition>, _1, boost::cref(typeName))) != res->Classes.end())
        {
            //a class
            memberType=ObjectMemberType;
            return true;
        }
        else if (std::find_if(res->Enumerations.begin(), res->Enumerations.end(), boost::bind(NameComparer<EnumerationDefinition>, _1, boost::cref(typeName))) != res->Enumerations.end())
        {
            //an enum
            memberType=EnumerationMemberType;
            return true;
        }
        else
        {
            //type not found
            return false;
        }
    }

    bool BasicTypes::CanParseValue(const std::string& typeName, const std::string value, RawParseResultConstPtr res) const
    {
        const TypeInfo* ti = GetTypeInfo(typeName);
        if (ti!=NULL)
        {
            return ti->ValCheck(value, res);
        }

        EnumerationDefinitions::const_iterator enumIt = std::find_if(res->Enumerations.begin(), res->Enumerations.end(), boost::bind(NameComparer<EnumerationDefinition>, _1, boost::cref(typeName)));
        if (enumIt!=res->Enumerations.end())
        {
            StringVector::const_iterator valIt = std::find(enumIt->EnumerationValues.begin(), enumIt->EnumerationValues.end(), value);
            return valIt!=enumIt->EnumerationValues.end();            
        }

        ClassDefinitions::const_iterator classIt = std::find_if(res->Classes.begin(), res->Classes.end(), boost::bind(NameComparer<ClassDefinition>, _1, boost::cref(typeName)));
        if (classIt!=res->Classes.end())
        {
            //TODO: check xml
            return true;
        }

        return false;
    }

    Size BasicTypes::SizeOfType(MemberType type) const
    {
        switch(type)
        {
        case BooleanMemberType:
            return sizeof(bool);

        case EnumerationMemberType:
            return sizeof(EnumInternal);

        case Int32MemberType:
            return sizeof(Int32);

        case Int64MemberType:
            return sizeof(Int64);

        case TypeIdMemberType:
            return sizeof(Int64);

        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
            return sizeof(Int64); //semidynamic member!

        case EntityIdMemberType:
            return sizeof(DotsC_EntityId); //semidynamic member!

        case StringMemberType:
        case ObjectMemberType:
        case BinaryMemberType:
            return DYNAMIC_MEMBER_SIZE;

            //  32 bit floats
        case Float32MemberType:
        case Ampere32MemberType:
        case CubicMeter32MemberType:
        case Hertz32MemberType:
        case Joule32MemberType:
        case Kelvin32MemberType:
        case Kilogram32MemberType:
        case Meter32MemberType:
        case MeterPerSecond32MemberType:
        case MeterPerSecondSquared32MemberType:
        case Newton32MemberType:
        case Pascal32MemberType:
        case Radian32MemberType:
        case RadianPerSecond32MemberType:
        case RadianPerSecondSquared32MemberType:
        case Second32MemberType:
        case SquareMeter32MemberType:
        case Steradian32MemberType:
        case Volt32MemberType:
        case Watt32MemberType:
            return sizeof(Float32);

            //  64 bit floats
        case Float64MemberType:
        case Ampere64MemberType:
        case CubicMeter64MemberType:
        case Hertz64MemberType:
        case Joule64MemberType:
        case Kelvin64MemberType:
        case Kilogram64MemberType:
        case Meter64MemberType:
        case MeterPerSecond64MemberType:
        case MeterPerSecondSquared64MemberType:
        case Newton64MemberType:
        case Pascal64MemberType:
        case Radian64MemberType:
        case RadianPerSecond64MemberType:
        case RadianPerSecondSquared64MemberType:
        case Second64MemberType:
        case SquareMeter64MemberType:
        case Steradian64MemberType:
        case Volt64MemberType:
        case Watt64MemberType:
            return sizeof(Float64);
        }
        throw InternalException("Unknown type in call to Size",__FILE__,__LINE__);
    }

    const std::string& BasicTypes::StringOf(MemberType type) const
    {
        const TypeInfo* ti = GetTypeInfo(type);
        if (ti!=NULL)
        {
            return ti->Name;
        }
        
        throw InternalException("Illegal member type", __FILE__, __LINE__);
    }
  
}
}
}
} //end namespace Safir::Dob::Typesystem
