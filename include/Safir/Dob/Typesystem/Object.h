/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
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

#ifndef __DOTS_OBJECT_H__
#define __DOTS_OBJECT_H__

#include <boost/shared_ptr.hpp>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/ContainerBase.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/Exceptions.h>


namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    class Object; //forward declaration for typedef below

    /** A smart pointer to an Object. */
    typedef boost::shared_ptr<Object> ObjectPtr;

    /** A smart pointer to a const Object. */
    typedef boost::shared_ptr<const Object> ObjectConstPtr;

    /**
     * The base class for all DOB objects.
     *
     * This class is the base class for all automatically generated DOB classes.
     */
    class DOTS_CPP_API Object
    {
    public:
        /**
         * Default constructor.
         */
        Object() {}

        /**
         * Virtual destructor.
         *
         * Needed to ensure proper destruction of Object pointers.
         */
        virtual ~Object() {}

        /**
         * Create a copy of the object.
         *
         * Will create a copy of the object on the heap and return a smart pointer to it.
         * Use boost::static_pointer_cast or boost::dynamic_pointer_cast to cast it
         * to the pointer type that you're after.
         *
         * @return A smart pointer to the copy of the object.
         */
        virtual Dob::Typesystem::ObjectPtr Clone() const;

        /**
         * Creates a new Object.
         *
         * Creates a new Object on the heap and returns a smart pointer to it.
         *
         * @return A smart pointer to a new object of type Object.
         */
        static Dob::Typesystem::ObjectPtr Create();


        /** The TypeId of the Object class. */
        static const Dob::Typesystem::TypeId ClassTypeId = 5955188366590963785LL;

        /**
         * Get the type id of this object.
         *
         * Gets the type id of the object. Method is virtual to ensure that the right
         * value gets returned for pointers or references.
         *
         * @note this method is overridden by all auto-generated classes.
         *
         * @return The TypeId of the object.
         */
        virtual  Dob::Typesystem::TypeId GetTypeId() const {return ClassTypeId;}

        /**
         * Check if any member of this object is changed.
         *
         * This method will recursively check if any member of the object has its change flag set.
         *
         * @note this method is overridden by all auto-generated classes.
         *
         * @return True if any member has changed.
         */
        virtual bool IsChanged();

        /**
         * Recursively set change flags in all members of this object.
         *
         * @note this method is overridden by all auto-generated classes.
         *
         * @param changed [in] - The value to set the change flags to
         */
        virtual void SetChanged(const bool changed);

        /**
         * @name Reflection part.
         * These methods allow applications to manipulate the members of objects
         * without having been compiled against it.
         * There should be no reason for most applications to use these methods.
         */
        /** @{ */

        /**
         * Get a reference to a member container from an object.
         *
         * Use the methods in Members to get member indices and array sizes for use
         * with this method.
         *
         * Note: Do not use this method unless you're very sure it is the one you need!
         *
         * @param member [in] - The index of the member to get.
         * @param index [in] - The array index of the member to get.
         * @return A reference to the member container.
         * @throws IllegalValueException If the index is not in the range of the array.
         * @throws SoftwareViolationException If the element is not an array and the index is not 0.
         */
        virtual ContainerBase & GetMember(const Dob::Typesystem::MemberIndex member,
                                          const Dob::Typesystem::ArrayIndex  index);       //MUST BE OVERRIDDEN!

        /**
         * Get a const reference to a member container from an object.
         *
         * Use the methods in Members to get member indices and array sizes for use
         * with this method.
         *
         * Note: Do not use this method unless you're very sure it is the one you need!
         *
         * @param member [in] - The index of the member to get.
         * @param index [in] - The array index of the member to get.
         * @return A const reference to the member container.
         * @throws IllegalValueException If the index is not in the range of the array.
         * @throws SoftwareViolationException If the element is not an array and the index is not 0.
         */
        virtual const ContainerBase & GetMember(const Dob::Typesystem::MemberIndex member,
                                                const Dob::Typesystem::ArrayIndex  index) const; //MUST BE OVERRIDDEN!

        /** @} */

        /**
         * @name Blob serialization/deserialization part.
         * These functions are for internal use only!
         * Their names and functionality are likely to change in the near future!
         */
        /** @{ */

        /**
         * Create an Object from a blob.
         *
         * @param handle [in] - Handle to a blobReader to deserialize.
         */
        explicit Object(Safir::Dob::Typesystem::Int64 handle);

        /**
         * Write the object to a blob.
         *
         * @param handle [in] - Handle to a blobWriter that is the destination of the serialized object.
         */
        virtual void WriteToBlob(Safir::Dob::Typesystem::Int64 handle) const;

        /** @} */
    };
}
}
}

#endif

