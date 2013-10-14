/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
* 
* Created by: Lars Hagström / stlrha
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

#ifndef __DOTS_FWD_H__
#define __DOTS_FWD_H__

#include <boost/interprocess/offset_ptr.hpp>
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    //    class Repository;
    //Forward declarations
    class ClassDescription;
    typedef boost::interprocess::offset_ptr<ClassDescription> ClassDescriptionPtr;
    typedef boost::interprocess::offset_ptr<const ClassDescription> ClassDescriptionConstPtr;

    class EnumDescription;
    typedef boost::interprocess::offset_ptr<const EnumDescription> EnumDescriptionConstPtr;

    class MemberDescription;
    typedef boost::interprocess::offset_ptr<const MemberDescription> MemberDescriptionConstPtr;

    class PropertyDescription;
    typedef boost::interprocess::offset_ptr<const PropertyDescription> PropertyDescriptionConstPtr;

    class ParameterDescription;
    typedef boost::interprocess::offset_ptr<const ParameterDescription> ParameterDescriptionConstPtr;

    class ExceptionDescription;
    typedef boost::interprocess::offset_ptr<const ExceptionDescription> ExceptionDescriptionConstPtr;

    class ClassDatabase;
    class EnumDatabase;

}
}
}
}
#endif
