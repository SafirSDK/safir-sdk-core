/******************************************************************************
*
* Copyright Saab AB, 2009 (http://www.safirsdk.com)
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

#ifndef _dots_exception_description_h
#define _dots_exception_description_h

#include "dots_internal_defs.h"
#include "dots_allocation_helper.h"
#include "dots_fwd.h"


namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    //typedef AllocationHelper::Containers<ShmString>::vector EnumValueVector;

    /**
     * Contains information about a Dob enum type specified in xml-files.
     */
    class ExceptionDescription
    {
    public:
        ExceptionDescription(const std::string & name,
                             const TypeId typeId,
                             const ExceptionDescriptionConstPtr baseClass,
                             AllocationHelper & allocHelper);

        ~ExceptionDescription();

        const char * Name() const {return m_name.c_str();}

        const TypeId GetTypeId() const {return m_typeId;}

        const ExceptionDescriptionConstPtr BaseClass() const {return m_baseClass;}

    private:
        void operator=(const ExceptionDescription&) const; //disable assignment operator.

        ShmString m_name;
        TypeId m_typeId;
        const ExceptionDescriptionConstPtr m_baseClass;
    };
}
}
}
}
#endif
