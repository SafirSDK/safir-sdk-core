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

#ifndef _dots_xml_elements_h
#define _dots_xml_elements_h

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
namespace XmlElements
{
    //Valid elemetns
    static const char * const ARRAY                    = "array";
    static const char * const ARRAY_ELEMENT            = "arrayElement";
    static const char * const ARRAY_ELEMENTS           = "arrayElements";
    static const char * const ARRAY_SIZE               = "arraySize";
    static const char * const BASE_CLASS               = "baseClass";
    static const char * const CHANGED                  = "changed";
    static const char * const CLASS                    = "class";
    static const char * const CLASS_MEMBER             = "classMember";
    static const char * const CREATE_ROUTINE           = "createRoutine";
    static const char * const CREATE_ROUTINES          = "createRoutines";
    static const char * const MEMBER_MAPPING           = "memberMapping";
    static const char * const ENUM                     = "enumeration";
    static const char * const ERR                      = "error";
    static const char * const EXCEPTION                = "exception";
    static const char * const INDEX                    = "index";
    static const char * const INSTANCE_ID              = "instanceId";
    static const char * const LENGTH                   = "maxLength";
    static const char * const MEMBER                   = "member";
    static const char * const MEMBERS                  = "members";
    static const char * const NAME                     = "name";
    static const char * const OBJ                      = "object";
    static const char * const ENTITY_ID                = "entityId";
    static const char * const PARAMETER                = "parameter";
    static const char * const PARAMETERS               = "parameters";
    static const char * const PROPERTY                 = "property";
    static const char * const PROPERTY_MEMBER          = "propertyMember";
    static const char * const PROPERTY_MAPPING         = "propertyMapping";
    static const char * const SUMMARY                  = "summary";
    static const char * const TYPE                     = "type";
    static const char * const TYPE_ID                  = "typeId";
    static const char * const VALUE                    = "value";
    static const char * const VALUES                   = "values";
    static const char * const CLASS_MEMBER_REFERENCE   = "classMemberReference";

    //References
    static const char * const VALUE_REF                = "valueRef";
    static const char * const ARRAY_SIZE_REF           = "arraySizeRef";
    static const char * const LENGTH_REF               = "maxLengthRef";
    static const char * const INSTANCE_ID_REF          = "instanceIdRef";
    static const char * const INDEX_REF                = "indexRef";
}
}
}
}
}
#endif
