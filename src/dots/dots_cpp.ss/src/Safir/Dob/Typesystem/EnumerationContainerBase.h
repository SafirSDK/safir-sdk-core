/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

#ifndef __DOTS_ENUMERATION_CONTAINER_BASE_H__
#define __DOTS_ENUMERATION_CONTAINER_BASE_H__

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/ContainerBase.h>
#include <typeinfo>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    /**
     * Base class for containers of enumeration values.
     * The containers for enumerations are defined in the automatically generated code,
     * but this class defines the common functionality for them.
     * Enumeration containers really store the ordinal values (integer representation of
     * the enumeration), and this class has methods for setting and getting the ordinal.
     * The derived class (in the generated code) has methods for setting and getting the
     * value as an enumeration value.
     * Most applications should not use the GetOrdinal/SetOrdinal functions, but should
     * use the SetVal and GetVal methods defined in the derived classes.
     */
    class EnumerationContainerBase : public ContainerBase
    {
    public:
        /**
         * Default Constructor.
         * Constructs an enumeration container that is null and not changed.
         */
        EnumerationContainerBase():ContainerBase(),m_bIsNull(true),m_Value() {}

        /**
         * Set the ordinal value of the enumeration container.
         *
         * Note: Only applications that need to use "anonymous enums" should
         * use this method. All other applications should be using the SetVal method.
         *
         * @param value [in] - The new value.
         * @throws IllegalValueException The value is not in the range of the enumeration.
         */
        virtual void SetOrdinal(const EnumerationValue value) = 0;

        /**
         * Get the ordinal value of the enumeration container.
         *
         * Note: Only applications that need to use "anonymous enums" should
         * use this method. All other applications should be using the GetVal method.
         *
         * @return The ordinal value of the container.
         * @throws NullException The container is null.
         */
        virtual EnumerationValue GetOrdinal() const= 0;

        //implementation of pure virtual in ContainerBase.
        virtual bool IsNull() const {return m_bIsNull;}

        //implementation of pure virtual in ContainerBase.
        virtual void SetNull()
        {
            m_bIsNull = true;
            m_bIsChanged = true;
        }

        //implementation of pure virtual in ContainerBase.
        virtual void Copy(const ContainerBase & that)
        {
            if (this != &that)
            {
                if (typeid(*this) != typeid(that))
                {
                    throw SoftwareViolationException(L"Invalid call to Copy, containers are not of same type",__WFILE__,__LINE__);
                }
                *this = static_cast<const EnumerationContainerBase &>(that);
            }
        }
    protected:
        friend class BlobOperations;

         /**
         * Copy assignment operator.
         *
         * @param other [in] - The object to copy.
         * @return A reference to this.
         */
        EnumerationContainerBase & operator=(const EnumerationContainerBase & other)
        {ContainerBase::operator =(other); m_Value = other.m_Value; m_bIsNull = other.m_bIsNull; return *this;}

        /**
         * The null flag for the enumeration container.
         */
        bool m_bIsNull;

        /**
         * The value of the enumeration container.
         * This is the ordinal value. Needs to be cast to the correct type in the derived classes.
         */
        EnumerationValue m_Value;
    };


}
}
}
#endif

