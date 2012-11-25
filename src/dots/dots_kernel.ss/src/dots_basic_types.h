/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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

#ifndef _dots_basic_types_h
#define _dots_basic_types_h

#include "dots_internal_defs.h"

#include <map>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Definitions of the basic types such as integers and floats.
     */
    class BasicTypes
    {
    public:
        static MemberType MemberTypeOf(const char* typeName); //returns ObjectMemberType if typeName is not found...
        static const char* StringOf(MemberType m);
        static Size SizeOfType(const MemberType& m);

    private:
        
        typedef std::map<std::string, MemberType> NameToTypeTable;
        static NameToTypeTable CreateNameToTypeTable();

        static NameToTypeTable::value_type make_pair(const MemberType m)
        {return std::make_pair(StringOf(m),m);}

        static const char* Boolean_Type_String;
        static const char* Enumeration_Type_String;
        static const char* Int32_Type_String;
        static const char* Int64_Type_String;
        static const char* Float32_Type_String;
        static const char* Float64_Type_String;
        static const char* TypeId_Type_String;
        static const char* InstanceId_Type_String;
        static const char* EntityId_Type_String;
        static const char* ChannelId_Type_String;
        static const char* HandlerId_Type_String;
        static const char* String_Type_String;
        static const char* Object_Type_String;
        static const char* Binary_Type_String;
        //SI types
        static const char* Ampere_Type_String;
        static const char* CubicMeter_Type_String;
        static const char* Hertz_Type_String;
        static const char* Joule_Type_String;
        static const char* Kelvin_Type_String;
        static const char* Kilogram_Type_String;
        static const char* Meter_Type_String;
        static const char* MeterPerSecond_Type_String;
        static const char* MeterPerSecondSquared_Type_String;
        static const char* Newton_Type_String;
        static const char* Pascal_Type_String;
        static const char* Radian_Type_String;
        static const char* RadianPerSecond_Type_String;
        static const char* RadianPerSecondSquared_Type_String;
        static const char* Second_Type_String;
        static const char* SquareMeter_Type_String;
        static const char* Steradian_Type_String;
        static const char* Volt_Type_String;
        static const char* Watt_Type_String;
        //SI Long types
        static const char* Long_Ampere_Type_String;
        static const char* Long_CubicMeter_Type_String;
        static const char* Long_Hertz_Type_String;
        static const char* Long_Joule_Type_String;
        static const char* Long_Kelvin_Type_String;
        static const char* Long_Kilogram_Type_String;
        static const char* Long_Meter_Type_String;
        static const char* Long_MeterPerSecond_Type_String;
        static const char* Long_MeterPerSecondSquared_Type_String;
        static const char* Long_Newton_Type_String;
        static const char* Long_Pascal_Type_String;
        static const char* Long_Radian_Type_String;
        static const char* Long_RadianPerSecond_Type_String;
        static const char* Long_RadianPerSecondSquared_Type_String;
        static const char* Long_Second_Type_String;
        static const char* Long_SquareMeter_Type_String;
        static const char* Long_Steradian_Type_String;
        static const char* Long_Volt_Type_String;
        static const char* Long_Watt_Type_String;

        //static const Size  m_typeSize[49];
    };
}
}
}
}
#endif
