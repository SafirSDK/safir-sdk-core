/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#ifndef __DOTS_CONTAINER_BASE_H__
#define __DOTS_CONTAINER_BASE_H__

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    namespace Internal {class BlobOperations;} //forward declaration of internal stuff

    /**
     * Base class for all Containers.
     *
     * This class contains common functionality for all Containers.
     * Basically this amounts to the interface for nullability and
     * the change flag.
     */
    class ContainerBase
    {
    public:
        /**
         * Default Constructor.
         *
         * Construct a container that is not changed.
         */
        ContainerBase(): m_bIsChanged(false) {}

        /**
         * Virtual destructor.
         *
         * Needed to ensure proper destruction of Object pointers.
         */
        virtual ~ContainerBase() {}

        /**
         * Is the container set to null?
         *
         * @return True if the container is set to null.
         */
        virtual bool IsNull() const = 0;

        /**
         * Set the container to null.
         */
        virtual void SetNull() = 0;

        /**
         * Is the change flag set on the container?
         * The change flag gets updated every time the contained value changes.
         * Note: If this is a container containing an object this call will recursively
         *       check change flags in the contained object.
         *
         * @return True if the containers change flag is set.
         */
        virtual bool IsChanged() const {return m_bIsChanged;}

        /**
         * Set the containers change flag.
         *
         * It should be fairly unusual for an application to have to use this
         * operation. There is nothing dangerous about it, but are you sure this
         * is the operation you were after?
         *
         * The change flag is how receivers of objects can work out what the
         * sender really wanted done on the object.
         *
         * Note: If this is a container containing an object this call will recursively
         *       set all the change flags in the contained object.
         * @param changed [in] - The value to set the change flag(s) to.
         */
        virtual void SetChanged(const bool changed) {m_bIsChanged = changed;}

        /**
         * Virtual assignment.
         *
         * Copy all the members from "that" into "this". Types must be the same for this to work!
         *
         * @param that [in] - The object to copy into this.
         * @throws SoftwareViolationException If the types are not of the same kind.
         */
        virtual void Copy(const ContainerBase & that) = 0;
    protected:
        /**
        * Copy assignment operator.
        *
        * Declared protected to stop incorrect non-virtual assignments (use Copy for virtual assignment).
        *
        * @param other [in] - The object to copy into this.
        * @return A reference to the assigned object.
        */
        ContainerBase & operator=(const ContainerBase & other)
        {m_bIsChanged = other.m_bIsChanged; return *this;}

        /**
         * The variable containing the change flag.
         */
        bool m_bIsChanged;
    };

    /**
     * Check if a container is changed.
     *
     * This meant for use with stuff in std::algorithms. All it does is call IsChanged on the container.
     *
     * @param container [in] - The container to check.
     * @return True if the container has changed.
     */
    static inline bool IsChanged(const ContainerBase & container) {return container.IsChanged();}

}
}
}
#endif

