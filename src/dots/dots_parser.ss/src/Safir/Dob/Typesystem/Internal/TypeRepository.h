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
#ifndef __DOTS_TYPE_REPOSITORY_H__
#define __DOTS_TYPE_REPOSITORY_H__

#include <vector>
#include <boost/noncopyable.hpp>
#include <Safir/Dob/Typesystem/Internal/TypeDescriptions.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    class TypeRepository : private boost::noncopyable
    {
    public:        
        //Enmerations
        virtual const EnumDescription* GetEnum(DotsC_TypeId typeId) const = 0;
        virtual size_t GetNumberOfEnums() const = 0;
        virtual void GetAllEnumTypeIds(std::vector<DotsC_TypeId>& typeIds) const = 0;

        //properties
        virtual const PropertyDescription* GetProperty(DotsC_TypeId typeId) const = 0;
        virtual size_t GetNumberOfproperties() const = 0;
        virtual void GetAllPropertyTypeIds(std::vector<DotsC_TypeId>& typeIds) const = 0;

        //classes
        virtual const ClassDescription* GetClass(DotsC_TypeId typeId) const = 0;
        virtual size_t GetNumberOfclasses() const = 0;
        virtual void GetAllClassTypeIds(std::vector<DotsC_TypeId>& typeIds) const = 0;

        //exceptions
        virtual const ExceptionDescription* GetException(DotsC_TypeId typeId) const = 0;
        virtual size_t GetNumberOfexceptions() const = 0;
        virtual void GetAllExceptionTypeIds(std::vector<DotsC_TypeId>& typeIds) const = 0;
    };


}
}
}
}

#endif
