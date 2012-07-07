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
#include "TypeChecker.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Parser
{
    template <class T>
    bool NameComparer(const T& obj, const std::string& name)
    {
        return obj.Name==name;
    }

    template <class T> struct ValueChecker
    {
        bool operator()(const std::string& val, const ParseResult&) const
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
        bool operator()(const std::string&, const ParseResult&) const {return true;}
    };

    template<> struct ValueChecker<bool>
    {
        bool operator()(const std::string& val, const ParseResult&) const
        {
            return val=="True" || val=="true" || val=="False" || val=="false";
        }
    };

    const TypeChecker& TypeChecker::Instance()
    {
        static TypeChecker instance;
        return instance;
    }

    TypeChecker::TypeChecker(void) : m_basicTypes()
    {        
        ValueChecker<boost::int32_t> int32Checker;
        ValueChecker<boost::int64_t> int64Checker;
        ValueChecker<float> float32Checker;
        ValueChecker<double> float64Checker;
        ValueChecker<bool> boolChecker;
        ValueChecker<std::string> noCheck;
      
        m_basicTypes.insert(std::make_pair("Enumeration", noCheck)); //TODO: remove?
        m_basicTypes.insert(std::make_pair("TypeId", noCheck));
        m_basicTypes.insert(std::make_pair("EntityId", noCheck));

        m_basicTypes.insert(std::make_pair("Boolean", boolChecker));
        m_basicTypes.insert(std::make_pair("Int32", int32Checker));
        m_basicTypes.insert(std::make_pair("Int64", int64Checker));
        m_basicTypes.insert(std::make_pair("Float32", float32Checker));
        m_basicTypes.insert(std::make_pair("Float64", float64Checker));
        m_basicTypes.insert(std::make_pair("InstanceId", noCheck));
        m_basicTypes.insert(std::make_pair("ChannelId", noCheck));
        m_basicTypes.insert(std::make_pair("HandlerId", noCheck));
        m_basicTypes.insert(std::make_pair("String", noCheck));
        m_basicTypes.insert(std::make_pair("Object", noCheck)); //This is a class not a basic type, however it maybe almost(?) ok to let anything pass here and just ignore the content.
        m_basicTypes.insert(std::make_pair("Binary", noCheck));
        m_basicTypes.insert(std::make_pair("Ampere32", float32Checker));
        m_basicTypes.insert(std::make_pair("CubicMeter32", float32Checker));
        m_basicTypes.insert(std::make_pair("Hertz32", float32Checker));
        m_basicTypes.insert(std::make_pair("Joule32", float32Checker));
        m_basicTypes.insert(std::make_pair("Kelvin32", float32Checker));
        m_basicTypes.insert(std::make_pair("Kilogram32", float32Checker));
        m_basicTypes.insert(std::make_pair("Meter32", float32Checker));
        m_basicTypes.insert(std::make_pair("MeterPerSecond32", float32Checker));
        m_basicTypes.insert(std::make_pair("MeterPerSecondSquared32", float32Checker));
        m_basicTypes.insert(std::make_pair("Newton32", float32Checker));
        m_basicTypes.insert(std::make_pair("Pascal32", float32Checker));
        m_basicTypes.insert(std::make_pair("Radian32", float32Checker));
        m_basicTypes.insert(std::make_pair("RadianPerSecond32", float32Checker));
        m_basicTypes.insert(std::make_pair("RadianPerSecondSquared32", float32Checker));
        m_basicTypes.insert(std::make_pair("Second32", float32Checker));
        m_basicTypes.insert(std::make_pair("SquareMeter32", float32Checker));
        m_basicTypes.insert(std::make_pair("Steradian32", float32Checker));
        m_basicTypes.insert(std::make_pair("Volt32", float32Checker));
        m_basicTypes.insert(std::make_pair("Watt32", float32Checker));
        m_basicTypes.insert(std::make_pair("Ampere64", float64Checker));
        m_basicTypes.insert(std::make_pair("CubicMeter64", float64Checker));
        m_basicTypes.insert(std::make_pair("Hertz64", float64Checker));
        m_basicTypes.insert(std::make_pair("Joule64", float64Checker));
        m_basicTypes.insert(std::make_pair("Kelvin64", float64Checker));
        m_basicTypes.insert(std::make_pair("Kilogram64", float64Checker));
        m_basicTypes.insert(std::make_pair("Meter64", float64Checker));
        m_basicTypes.insert(std::make_pair("MeterPerSecond64", float64Checker));
        m_basicTypes.insert(std::make_pair("MeterPerSecondSquared64", float64Checker));
        m_basicTypes.insert(std::make_pair("Newton64", float64Checker));
        m_basicTypes.insert(std::make_pair("Pascal64", float64Checker));
        m_basicTypes.insert(std::make_pair("Radian64", float64Checker));
        m_basicTypes.insert(std::make_pair("RadianPerSecond64", float64Checker));
        m_basicTypes.insert(std::make_pair("RadianPerSecondSquared64", float64Checker));
        m_basicTypes.insert(std::make_pair("Second64", float64Checker));
        m_basicTypes.insert(std::make_pair("SquareMeter64", float64Checker));
        m_basicTypes.insert(std::make_pair("Steradian64", float64Checker));
        m_basicTypes.insert(std::make_pair("Volt64", float64Checker));
        m_basicTypes.insert(std::make_pair("Watt64", float64Checker));
    }    

    bool TypeChecker::IsType(const std::string& typeName, const ParseResult& res) const
    {        
        if (m_basicTypes.find(typeName)!=m_basicTypes.end())
        {
            //a basic type
            return true;
        }        
        else if (std::find_if(res.Classes.begin(), res.Classes.end(), boost::bind(NameComparer<ClassDefinition>, _1, boost::cref(typeName))) != res.Classes.end())
        {
            //a class
            return true;
        }
        else if (std::find_if(res.Enumerations.begin(), res.Enumerations.end(), boost::bind(NameComparer<EnumerationDefinition>, _1, boost::cref(typeName))) != res.Enumerations.end())
        {
            //an enum
            return true;
        }
        else
        {
            //type not found
            return false;
        }
    }

    bool TypeChecker::CanParseValue(const std::string& typeName, const std::string value, const ParseResult& res) const
    {
        BasicTypeMap::const_iterator basicIt = m_basicTypes.find(typeName);
        if (basicIt!=m_basicTypes.end())
        {
            return basicIt->second(value, res);
        }

        EnumerationDefinitions::const_iterator enumIt = std::find_if(res.Enumerations.begin(), res.Enumerations.end(), boost::bind(NameComparer<EnumerationDefinition>, _1, boost::cref(typeName)));
        if (enumIt!=res.Enumerations.end())
        {
            StringVector::const_iterator valIt = std::find(enumIt->EnumerationValues.begin(), enumIt->EnumerationValues.end(), value);
            return valIt!=enumIt->EnumerationValues.end();            
        }

        ClassDefinitions::const_iterator classIt = std::find_if(res.Classes.begin(), res.Classes.end(), boost::bind(NameComparer<ClassDefinition>, _1, boost::cref(typeName)));
        if (classIt!=res.Classes.end())
        {
            //TODO: check xml
            return true;
        }

        return false;
    }
  
}
}
}
} //end namespace Safir::Dob::Typesystem
