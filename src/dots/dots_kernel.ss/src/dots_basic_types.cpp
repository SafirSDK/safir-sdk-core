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

#include "dots_basic_types.h"
#include "dots_internal_defs.h"
#include "dots_blob_layout.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    const char* BasicTypes::Boolean_Type_String                         = "Boolean";
    const char* BasicTypes::Enumeration_Type_String                     = "Enumeration";
    const char* BasicTypes::Int32_Type_String                           = "Int32";
    const char* BasicTypes::Int64_Type_String                           = "Int64";
    const char* BasicTypes::Float32_Type_String                         = "Float32";
    const char* BasicTypes::Float64_Type_String                         = "Float64";
    const char* BasicTypes::TypeId_Type_String                          = "TypeId";
    const char* BasicTypes::InstanceId_Type_String                      = "InstanceId";
    const char* BasicTypes::EntityId_Type_String                        = "EntityId";
    const char* BasicTypes::ChannelId_Type_String                       = "ChannelId";
    const char* BasicTypes::HandlerId_Type_String                       = "HandlerId";
    const char* BasicTypes::String_Type_String                          = "String";
    const char* BasicTypes::Object_Type_String                          = "Object";
    const char* BasicTypes::Binary_Type_String                          = "Binary";
    //SI32 types
    const char* BasicTypes::Ampere_Type_String                          = "Ampere32";
    const char* BasicTypes::CubicMeter_Type_String                      = "CubicMeter32";
    const char* BasicTypes::Hertz_Type_String                           = "Hertz32";
    const char* BasicTypes::Joule_Type_String                           = "Joule32";
    const char* BasicTypes::Kelvin_Type_String                          = "Kelvin32";
    const char* BasicTypes::Kilogram_Type_String                        = "Kilogram32";
    const char* BasicTypes::Meter_Type_String                           = "Meter32";
    const char* BasicTypes::MeterPerSecond_Type_String                  = "MeterPerSecond32";
    const char* BasicTypes::MeterPerSecondSquared_Type_String           = "MeterPerSecondSquared32";
    const char* BasicTypes::Newton_Type_String                          = "Newton32";
    const char* BasicTypes::Pascal_Type_String                          = "Pascal32";
    const char* BasicTypes::Radian_Type_String                          = "Radian32";
    const char* BasicTypes::RadianPerSecond_Type_String                 = "RadianPerSecond32";
    const char* BasicTypes::RadianPerSecondSquared_Type_String          = "RadianPerSecondSquared32";
    const char* BasicTypes::Second_Type_String                          = "Second32";
    const char* BasicTypes::SquareMeter_Type_String                     = "SquareMeter32";
    const char* BasicTypes::Steradian_Type_String                       = "Steradian32";
    const char* BasicTypes::Volt_Type_String                            = "Volt32";
    const char* BasicTypes::Watt_Type_String                            = "Watt32";
    //SI64 types
    const char* BasicTypes::Long_Ampere_Type_String                     = "Ampere64";
    const char* BasicTypes::Long_CubicMeter_Type_String                 = "CubicMeter64";
    const char* BasicTypes::Long_Hertz_Type_String                      = "Hertz64";
    const char* BasicTypes::Long_Joule_Type_String                      = "Joule64";
    const char* BasicTypes::Long_Kelvin_Type_String                     = "Kelvin64";
    const char* BasicTypes::Long_Kilogram_Type_String                   = "Kilogram64";
    const char* BasicTypes::Long_Meter_Type_String                      = "Meter64";
    const char* BasicTypes::Long_MeterPerSecond_Type_String             = "MeterPerSecond64";
    const char* BasicTypes::Long_MeterPerSecondSquared_Type_String      = "MeterPerSecondSquared64";
    const char* BasicTypes::Long_Newton_Type_String                     = "Newton64";
    const char* BasicTypes::Long_Pascal_Type_String                     = "Pascal64";
    const char* BasicTypes::Long_Radian_Type_String                     = "Radian64";
    const char* BasicTypes::Long_RadianPerSecond_Type_String            = "RadianPerSecond64";
    const char* BasicTypes::Long_RadianPerSecondSquared_Type_String     = "RadianPerSecondSquared64";
    const char* BasicTypes::Long_Second_Type_String                     = "Second64";
    const char* BasicTypes::Long_SquareMeter_Type_String                = "SquareMeter64";
    const char* BasicTypes::Long_Steradian_Type_String                  = "Steradian64";
    const char* BasicTypes::Long_Volt_Type_String                       = "Volt64";
    const char* BasicTypes::Long_Watt_Type_String                       = "Watt64";


    Size BasicTypes::SizeOfType(const MemberType& m)
    {
        switch(m)
        {
        case BooleanMemberType:
            return sizeof(bool) * 8;

        case EnumerationMemberType:
            return sizeof(EnumInternal) * 2;

        case Int32MemberType:
            return sizeof(Int32) * 2;

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
            return Safir::Dob::Typesystem::Internal::BlobLayout::DYNAMIC_MEMBER_SIZE;

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
            return sizeof(Float32) * 2;

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


    BasicTypes::NameToTypeTable BasicTypes::CreateNameToTypeTable()
    {
        NameToTypeTable table;

        const bool allInserted =
            table.insert(make_pair(BooleanMemberType)).second &&
            table.insert(make_pair(EnumerationMemberType)).second &&
            table.insert(make_pair(Int32MemberType)).second &&
            table.insert(make_pair(Int64MemberType)).second &&
            table.insert(make_pair(Float32MemberType)).second &&
            table.insert(make_pair(Float64MemberType)).second &&
            table.insert(make_pair(TypeIdMemberType)).second &&
            table.insert(make_pair(InstanceIdMemberType)).second &&
            table.insert(make_pair(EntityIdMemberType)).second &&
            table.insert(make_pair(ChannelIdMemberType)).second &&
            table.insert(make_pair(HandlerIdMemberType)).second &&
            table.insert(make_pair(StringMemberType)).second &&
            table.insert(make_pair(BinaryMemberType)).second &&

            //SI types
            table.insert(make_pair(Ampere32MemberType)).second &&
            table.insert(make_pair(CubicMeter32MemberType)).second &&
            table.insert(make_pair(Hertz32MemberType)).second &&
            table.insert(make_pair(Joule32MemberType)).second &&
            table.insert(make_pair(Kelvin32MemberType)).second &&
            table.insert(make_pair(Kilogram32MemberType)).second &&
            table.insert(make_pair(Meter32MemberType)).second &&
            table.insert(make_pair(MeterPerSecond32MemberType)).second &&
            table.insert(make_pair(MeterPerSecondSquared32MemberType)).second &&
            table.insert(make_pair(Newton32MemberType)).second &&
            table.insert(make_pair(Pascal32MemberType)).second &&
            table.insert(make_pair(Radian32MemberType)).second &&
            table.insert(make_pair(RadianPerSecond32MemberType)).second &&
            table.insert(make_pair(RadianPerSecondSquared32MemberType)).second &&
            table.insert(make_pair(Second32MemberType)).second &&
            table.insert(make_pair(SquareMeter32MemberType)).second &&
            table.insert(make_pair(Steradian32MemberType)).second &&
            table.insert(make_pair(Volt32MemberType)).second &&
            table.insert(make_pair(Watt32MemberType)).second &&

            //SI Longtypes
            table.insert(make_pair(Ampere64MemberType)).second &&
            table.insert(make_pair(CubicMeter64MemberType)).second &&
            table.insert(make_pair(Hertz64MemberType)).second &&
            table.insert(make_pair(Joule64MemberType)).second &&
            table.insert(make_pair(Kelvin64MemberType)).second &&
            table.insert(make_pair(Kilogram64MemberType)).second &&
            table.insert(make_pair(Meter64MemberType)).second &&
            table.insert(make_pair(MeterPerSecond64MemberType)).second &&
            table.insert(make_pair(MeterPerSecondSquared64MemberType)).second &&
            table.insert(make_pair(Newton64MemberType)).second &&
            table.insert(make_pair(Pascal64MemberType)).second &&
            table.insert(make_pair(Radian64MemberType)).second &&
            table.insert(make_pair(RadianPerSecond64MemberType)).second &&
            table.insert(make_pair(RadianPerSecondSquared64MemberType)).second &&
            table.insert(make_pair(Second64MemberType)).second &&
            table.insert(make_pair(SquareMeter64MemberType)).second &&
            table.insert(make_pair(Steradian64MemberType)).second &&
            table.insert(make_pair(Volt64MemberType)).second &&
            table.insert(make_pair(Watt64MemberType)).second;

        if (!allInserted)
        {
            throw InternalException("At least one type was not successfully inserted into NameToTypeTable",__FILE__,__LINE__);
        }
        return table;
    }

    MemberType BasicTypes::MemberTypeOf(const char* typeName)
    {
        static const NameToTypeTable table = CreateNameToTypeTable();
        NameToTypeTable::const_iterator findIt = table.find(typeName);
        if (findIt != table.end())
        {
            return findIt->second;
        }
        else
        {
            //Unknown type is supposed to be Object
            return ObjectMemberType;
        }
    }

    const char* BasicTypes::StringOf(MemberType m)
    {
        switch(m)
        {
        case BooleanMemberType:
            return Boolean_Type_String;
        case EnumerationMemberType:
            return Enumeration_Type_String;
        case Int32MemberType:
            return Int32_Type_String;
        case Int64MemberType:
            return Int64_Type_String;
        case Float32MemberType:
            return Float32_Type_String;
        case Float64MemberType:
            return Float64_Type_String;
        case TypeIdMemberType:
            return TypeId_Type_String;
        case InstanceIdMemberType:
            return InstanceId_Type_String;
        case EntityIdMemberType:
            return EntityId_Type_String;
        case ChannelIdMemberType:
            return ChannelId_Type_String;
        case HandlerIdMemberType:
            return HandlerId_Type_String;
        case StringMemberType:
            return String_Type_String;
        case ObjectMemberType:
            return Object_Type_String;
        case BinaryMemberType:
            return Binary_Type_String;
        //SI Types
        case Ampere32MemberType:
            return Ampere_Type_String;
        case CubicMeter32MemberType:
            return CubicMeter_Type_String;
        case Hertz32MemberType:
            return Hertz_Type_String;
        case Joule32MemberType:
            return Joule_Type_String;
        case Kelvin32MemberType:
            return Kelvin_Type_String;
        case Kilogram32MemberType:
            return Kilogram_Type_String;
        case Meter32MemberType:
            return Meter_Type_String;
        case MeterPerSecond32MemberType:
            return MeterPerSecond_Type_String;
        case MeterPerSecondSquared32MemberType:
            return MeterPerSecondSquared_Type_String;
        case Newton32MemberType:
            return Newton_Type_String;
        case Pascal32MemberType:
            return Pascal_Type_String;
        case Radian32MemberType:
            return Radian_Type_String;
        case RadianPerSecond32MemberType:
            return RadianPerSecond_Type_String;
        case RadianPerSecondSquared32MemberType:
            return RadianPerSecondSquared_Type_String;
        case Second32MemberType:
            return Second_Type_String;
        case SquareMeter32MemberType:
            return SquareMeter_Type_String;
        case Steradian32MemberType:
            return Steradian_Type_String;
        case Volt32MemberType:
            return Volt_Type_String;
        case Watt32MemberType:
            return Watt_Type_String;
        //SI Long Types
        case Ampere64MemberType:
            return Long_Ampere_Type_String;
        case CubicMeter64MemberType:
            return Long_CubicMeter_Type_String;
        case Hertz64MemberType:
            return Long_Hertz_Type_String;
        case Joule64MemberType:
            return Long_Joule_Type_String;
        case Kelvin64MemberType:
            return Long_Kelvin_Type_String;
        case Kilogram64MemberType:
            return Long_Kilogram_Type_String;
        case Meter64MemberType:
            return Long_Meter_Type_String;
        case MeterPerSecond64MemberType:
            return Long_MeterPerSecond_Type_String;
        case MeterPerSecondSquared64MemberType:
            return Long_MeterPerSecondSquared_Type_String;
        case Newton64MemberType:
            return Long_Newton_Type_String;
        case Pascal64MemberType:
            return Long_Pascal_Type_String;
        case Radian64MemberType:
            return Long_Radian_Type_String;
        case RadianPerSecond64MemberType:
            return Long_RadianPerSecond_Type_String;
        case RadianPerSecondSquared64MemberType:
            return Long_RadianPerSecondSquared_Type_String;
        case Second64MemberType:
            return Long_Second_Type_String;
        case SquareMeter64MemberType:
            return Long_SquareMeter_Type_String;
        case Steradian64MemberType:
            return Long_Steradian_Type_String;
        case Volt64MemberType:
            return Long_Volt_Type_String;
        case Watt64MemberType:
            return Long_Watt_Type_String;
        }
        throw InternalException("Not all types covered by BasicType::StringOf",__FILE__,__LINE__);
    }

}
}
}
}
