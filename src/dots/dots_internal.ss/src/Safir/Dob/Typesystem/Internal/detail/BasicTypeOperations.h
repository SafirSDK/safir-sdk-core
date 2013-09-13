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
#ifndef __DOTS_INTERNAL_DETAIL_BASIC_TYPE_OPERATIONS_H__
#define __DOTS_INTERNAL_DETAIL_BASIC_TYPE_OPERATIONS_H__

#include <string>
#include <map>
#include <vector>

#include <boost/cstdint.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <boost/noncopyable.hpp>
#include <boost/unordered_map.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/archive/iterators/base64_from_binary.hpp>
#include <boost/archive/iterators/binary_from_base64.hpp>
#include <boost/archive/iterators/transform_width.hpp>
#include <boost/archive/iterators/insert_linebreaks.hpp>
#include <boost/archive/iterators/remove_whitespace.hpp>

#include <Safir/Dob/Typesystem/Internal/Id.h>
#include <Safir/Dob/Typesystem/Internal/TypeRepository.h>
#include <Safir/Dob/Typesystem/Internal/KernelDefs2.h>
#include <Safir/Dob/Typesystem/Internal/ParseError.h>
#include <Safir/Dob/Typesystem/Internal/detail/InternalDefs.h>
#include <Safir/Dob/Typesystem/Internal/detail/classic_string_cast.h>


namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    class TypeRepository;

namespace detail
{
    class BasicTypes : public boost::noncopyable
    {
    public:

        static const BasicTypes& Instance()
        {
            static BasicTypes instance;
            return instance;
        }

        Size SizeOfType(MemberType type) const
        {
            switch(type)
            {
            case BooleanMemberType:
                return sizeof(bool);

            case EnumerationMemberType:
                return sizeof(EnumInternal);

            case Int32MemberType:
                return sizeof(DotsC_Int32);

            case Int64MemberType:
                return sizeof(DotsC_Int64);

            case TypeIdMemberType:
                return sizeof(DotsC_Int64);

            case InstanceIdMemberType:
            case ChannelIdMemberType:
            case HandlerIdMemberType:
                return sizeof(DotsC_Int64); //semidynamic member!

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
                return sizeof(DotsC_Float32);

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
                return sizeof(DotsC_Float64);
            }

            throw std::logic_error("ENSURE");
        }

        const std::string& StringOf(MemberType type) const
        {
            const std::string* s=MemberTypeToString(type);
            if (s)
            {
                return *s;
            }

            throw std::logic_error("ENSURE");
        }

        bool IsBasicType(const std::string& typeName, DotsC_MemberType& memberType) const
        {
            const DotsC_MemberType* mt=StringToMemberType(typeName);
            if (mt)
            {
                memberType=*mt;
                return (memberType!=ObjectMemberType && memberType!=EnumerationMemberType); //Object and Enum is not basic types
            }
            return false;
        }

        static std::string ToBase64(const std::string& bin)
        {
            typedef boost::archive::iterators::insert_linebreaks< boost::archive::iterators::base64_from_binary< boost::archive::iterators::transform_width<std::string::const_iterator,6,8> >, 72 > it_base64_t;
            unsigned int writePaddChars=(3-bin.size()%3)%3;
            std::string base64(it_base64_t(bin.begin()),it_base64_t(bin.end()));
            base64.append(writePaddChars,'=');
            return base64;
        }

        static bool FromBase64(std::string base64, std::string& bin)
        {
            try
            {
                typedef boost::archive::iterators::transform_width< boost::archive::iterators::binary_from_base64< boost::archive::iterators::remove_whitespace< std::string::const_iterator> >, 8, 6 > it_binary_t;
                unsigned int paddChars=count(base64.begin(), base64.end(), '=');
                std::replace(base64.begin(),base64.end(),'=','A'); // replace '=' by base64 encoding of '\0'
                bin.insert(bin.begin(), it_binary_t(base64.begin()), it_binary_t(base64.end()));
                bin.erase(bin.end()-paddChars,bin.end());  // erase pad
                return true;
            }
            catch (const std::exception&)
            {
                return false;
            }
        }

        static bool IsOfType(const TypeRepository* repository, MemberType mt, TypeId tid, MemberType ofMt, TypeId ofTid)
        {
            if (mt!=ofMt)
            {
                return false;
            }
            else if (mt==EnumerationMemberType)
            {
                return tid==ofTid;
            }
            else if (mt==ObjectMemberType)
            {
                //Check object types and handle inheritance.
                const ClassDescription* tmpClass=repository->GetClass(tid);
                while (tmpClass)
                {
                    if (tmpClass->GetTypeId()==ofTid)
                        return true;

                    tmpClass=tmpClass->GetBaseClass();
                }
                return false;
            }
            else //Basic types
            {
                return true;
            }
        }

        static bool ValidTypeId(const TypeRepository* repository, TypeId tid)
        {
            return repository->GetClass(tid)!=NULL ||
                    repository->GetEnum(tid)!=NULL ||
                    repository->GetProperty(tid)!=NULL ||
                    repository->GetException(tid)!=NULL;
        }

        //Built in object names
        static const std::string& ObjectName() {static const std::string s("Object"); return s;}
        static const std::string ExceptionName() {static const std::string s("Exception"); return s;}
        static const std::string FundamentalExceptionName() {static const std::string s("FundamentalException"); return s;}

    private:
        boost::unordered_map<std::string, DotsC_MemberType> m_nameToMt;
        boost::unordered_map<DotsC_MemberType, std::string> m_mtToName;

        const std::string* MemberTypeToString(DotsC_MemberType mt) const
        {
            boost::unordered_map<DotsC_MemberType, std::string>::const_iterator it=m_mtToName.find(mt);
            if (it!=m_mtToName.end())
            {
                return &(it->second);
            }
            return NULL;
        }

        const DotsC_MemberType* StringToMemberType(const std::string& typeName) const
        {
            boost::unordered_map<std::string, DotsC_MemberType>::const_iterator it=m_nameToMt.find(typeName);
            if (it!=m_nameToMt.end())
            {
                return &(it->second);
            }
            return NULL;
        }

        BasicTypes()
            :m_nameToMt()
            ,m_mtToName()
        {
            m_mtToName.insert(std::make_pair(EnumerationMemberType, "Enumeration"));
            m_mtToName.insert(std::make_pair(TypeIdMemberType, "TypeId"));
            m_mtToName.insert(std::make_pair(EntityIdMemberType, "EntityId"));
            m_mtToName.insert(std::make_pair(BooleanMemberType, "Boolean"));
            m_mtToName.insert(std::make_pair(Int32MemberType, "Int32"));
            m_mtToName.insert(std::make_pair(Int64MemberType, "Int64"));
            m_mtToName.insert(std::make_pair(Float32MemberType, "Float32"));
            m_mtToName.insert(std::make_pair(Float64MemberType, "Float64"));
            m_mtToName.insert(std::make_pair(InstanceIdMemberType, "InstanceId"));
            m_mtToName.insert(std::make_pair(ChannelIdMemberType, "ChannelId"));
            m_mtToName.insert(std::make_pair(HandlerIdMemberType, "HandlerId"));
            m_mtToName.insert(std::make_pair(StringMemberType, "String"));
            m_mtToName.insert(std::make_pair(ObjectMemberType, "Object"));
            m_mtToName.insert(std::make_pair(BinaryMemberType, "Binary"));
            m_mtToName.insert(std::make_pair(Ampere32MemberType, "Ampere32"));
            m_mtToName.insert(std::make_pair(CubicMeter32MemberType, "CubicMeter32"));
            m_mtToName.insert(std::make_pair(Hertz32MemberType, "Hertz32"));
            m_mtToName.insert(std::make_pair(Joule32MemberType, "Joule32"));
            m_mtToName.insert(std::make_pair(Kelvin32MemberType, "Kelvin32"));
            m_mtToName.insert(std::make_pair(Kilogram32MemberType, "Kilogram32"));
            m_mtToName.insert(std::make_pair(Meter32MemberType, "Meter32"));
            m_mtToName.insert(std::make_pair(MeterPerSecond32MemberType, "MeterPerSecond32"));
            m_mtToName.insert(std::make_pair(MeterPerSecondSquared32MemberType, "MeterPerSecondSquared32"));
            m_mtToName.insert(std::make_pair(Newton32MemberType, "Newton32"));
            m_mtToName.insert(std::make_pair(Pascal32MemberType, "Pascal32"));
            m_mtToName.insert(std::make_pair(Radian32MemberType, "Radian32"));
            m_mtToName.insert(std::make_pair(RadianPerSecond32MemberType, "RadianPerSecond32"));
            m_mtToName.insert(std::make_pair(RadianPerSecondSquared32MemberType, "RadianPerSecondSquared32"));
            m_mtToName.insert(std::make_pair(Second32MemberType, "Second32"));
            m_mtToName.insert(std::make_pair(SquareMeter32MemberType, "SquareMeter32"));
            m_mtToName.insert(std::make_pair(Steradian32MemberType, "Steradian32"));
            m_mtToName.insert(std::make_pair(Volt32MemberType, "Volt32"));
            m_mtToName.insert(std::make_pair(Watt32MemberType, "Watt32"));
            m_mtToName.insert(std::make_pair(Ampere64MemberType, "Ampere64"));
            m_mtToName.insert(std::make_pair(CubicMeter64MemberType, "CubicMeter64"));
            m_mtToName.insert(std::make_pair(Hertz64MemberType, "Hertz64"));
            m_mtToName.insert(std::make_pair(Joule64MemberType, "Joule64"));
            m_mtToName.insert(std::make_pair(Kelvin64MemberType, "Kelvin64"));
            m_mtToName.insert(std::make_pair(Kilogram64MemberType, "Kilogram64"));
            m_mtToName.insert(std::make_pair(Meter64MemberType, "Meter64"));
            m_mtToName.insert(std::make_pair(MeterPerSecond64MemberType, "MeterPerSecond64"));
            m_mtToName.insert(std::make_pair(MeterPerSecondSquared64MemberType, "MeterPerSecondSquared64"));
            m_mtToName.insert(std::make_pair(Newton64MemberType, "Newton64"));
            m_mtToName.insert(std::make_pair(Pascal64MemberType, "Pascal64"));
            m_mtToName.insert(std::make_pair(Radian64MemberType, "Radian64"));
            m_mtToName.insert(std::make_pair(RadianPerSecond64MemberType, "RadianPerSecond64"));
            m_mtToName.insert(std::make_pair(RadianPerSecondSquared64MemberType, "RadianPerSecondSquared64"));
            m_mtToName.insert(std::make_pair(Second64MemberType, "Second64"));
            m_mtToName.insert(std::make_pair(SquareMeter64MemberType, "SquareMeter64"));
            m_mtToName.insert(std::make_pair(Steradian64MemberType, "Steradian64"));
            m_mtToName.insert(std::make_pair(Volt64MemberType, "Volt64"));
            m_mtToName.insert(std::make_pair(Watt64MemberType, "Watt64"));

            m_nameToMt.insert(std::make_pair("Enumeration", EnumerationMemberType));
            m_nameToMt.insert(std::make_pair("TypeId", TypeIdMemberType));
            m_nameToMt.insert(std::make_pair("EntityId", EntityIdMemberType));
            m_nameToMt.insert(std::make_pair("Boolean", BooleanMemberType));
            m_nameToMt.insert(std::make_pair("Int32", Int32MemberType));
            m_nameToMt.insert(std::make_pair("Int64", Int64MemberType));
            m_nameToMt.insert(std::make_pair("Float32", Float32MemberType));
            m_nameToMt.insert(std::make_pair("Float64", Float64MemberType));
            m_nameToMt.insert(std::make_pair("InstanceId", InstanceIdMemberType));
            m_nameToMt.insert(std::make_pair("ChannelId", ChannelIdMemberType));
            m_nameToMt.insert(std::make_pair("HandlerId", HandlerIdMemberType));
            m_nameToMt.insert(std::make_pair("String", StringMemberType));
            m_nameToMt.insert(std::make_pair("Object", ObjectMemberType));
            m_nameToMt.insert(std::make_pair("Binary", BinaryMemberType));
            m_nameToMt.insert(std::make_pair("Ampere32", Ampere32MemberType));
            m_nameToMt.insert(std::make_pair("CubicMeter32", CubicMeter32MemberType));
            m_nameToMt.insert(std::make_pair("Hertz32", Hertz32MemberType));
            m_nameToMt.insert(std::make_pair("Joule32", Joule32MemberType));
            m_nameToMt.insert(std::make_pair("Kelvin32", Kelvin32MemberType));
            m_nameToMt.insert(std::make_pair("Kilogram32", Kilogram32MemberType));
            m_nameToMt.insert(std::make_pair("Meter32", Meter32MemberType));
            m_nameToMt.insert(std::make_pair("MeterPerSecond32", MeterPerSecond32MemberType));
            m_nameToMt.insert(std::make_pair("MeterPerSecondSquared32", MeterPerSecondSquared32MemberType));
            m_nameToMt.insert(std::make_pair("Newton32", Newton32MemberType));
            m_nameToMt.insert(std::make_pair("Pascal32", Pascal32MemberType));
            m_nameToMt.insert(std::make_pair("Radian32", Radian32MemberType));
            m_nameToMt.insert(std::make_pair("RadianPerSecond32", RadianPerSecond32MemberType));
            m_nameToMt.insert(std::make_pair("RadianPerSecondSquared32", RadianPerSecondSquared32MemberType));
            m_nameToMt.insert(std::make_pair("Second32", Second32MemberType));
            m_nameToMt.insert(std::make_pair("SquareMeter32", SquareMeter32MemberType));
            m_nameToMt.insert(std::make_pair("Steradian32", Steradian32MemberType));
            m_nameToMt.insert(std::make_pair("Volt32", Volt32MemberType));
            m_nameToMt.insert(std::make_pair("Watt32", Watt32MemberType));
            m_nameToMt.insert(std::make_pair("Ampere64", Ampere64MemberType));
            m_nameToMt.insert(std::make_pair("CubicMeter64", CubicMeter64MemberType));
            m_nameToMt.insert(std::make_pair("Hertz64", Hertz64MemberType));
            m_nameToMt.insert(std::make_pair("Joule64", Joule64MemberType));
            m_nameToMt.insert(std::make_pair("Kelvin64", Kelvin64MemberType));
            m_nameToMt.insert(std::make_pair("Kilogram64", Kilogram64MemberType));
            m_nameToMt.insert(std::make_pair("Meter64", Meter64MemberType));
            m_nameToMt.insert(std::make_pair("MeterPerSecond64", MeterPerSecond64MemberType));
            m_nameToMt.insert(std::make_pair("MeterPerSecondSquared64", MeterPerSecondSquared64MemberType));
            m_nameToMt.insert(std::make_pair("Newton64", Newton64MemberType));
            m_nameToMt.insert(std::make_pair("Pascal64", Pascal64MemberType));
            m_nameToMt.insert(std::make_pair("Radian64", Radian64MemberType));
            m_nameToMt.insert(std::make_pair("RadianPerSecond64", RadianPerSecond64MemberType));
            m_nameToMt.insert(std::make_pair("RadianPerSecondSquared64", RadianPerSecondSquared64MemberType));
            m_nameToMt.insert(std::make_pair("Second64", Second64MemberType));
            m_nameToMt.insert(std::make_pair("SquareMeter64", SquareMeter64MemberType));
            m_nameToMt.insert(std::make_pair("Steradian64", Steradian64MemberType));
            m_nameToMt.insert(std::make_pair("Volt64", Volt64MemberType));
            m_nameToMt.insert(std::make_pair("Watt64", Watt64MemberType));
        }
    };
}
}
}
}
} //end namespace Safir::Dob::Typesystem::detail

#endif


