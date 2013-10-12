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

#ifndef __DOTS_PROPERTIES_H__
#define __DOTS_PROPERTIES_H__

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/Object.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/InstanceId.h>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    /**
     * Functions for getting property information from types and objects.
     *
     * With these operations you can get and set members on objects using properties.
     * If you need member information (names, typeids etc) from a property, use the
     * functions in Members. (For array sizes, use the method in this class though...).
    */
    class DOTS_API Properties
    {
    public:

        /**
         * Get the array size of a property member.
         *
         * @param classId [in] -  type id of a class that supports the specified property.
         * @param propertyId [in] - type id of the property
         * @param propertyMember [in] - index of the property member.
         * @return The array size of the property member.
         * @throws IllegalValueException There is no such type or member defined.
         */
        static Dob::Typesystem::Int32 GetArraySize (const Dob::Typesystem::TypeId classId,
                                                    const Dob::Typesystem::TypeId propertyId,
                                                    const Dob::Typesystem::MemberIndex propertyMember);


        /**
         * Set a property member to null.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void SetNull(Dob::Typesystem::ObjectPtr object,
                            const Dob::Typesystem::TypeId propertyId,
                            const Dob::Typesystem::MemberIndex member,
                            const Dob::Typesystem::ArrayIndex index);

        /**
         * Is the property member null.
         *
         * @param object [in] - The object to check inside.
         * @param propertyId [in] - TypeId of the property to use.
         * @param member [in] - Index of the property member.
         * @param index [in] - Array index.
         * @return True if the property member (or a parent item of it) was null.
         */
        static bool IsNull(const Dob::Typesystem::ObjectPtr object,
                           const Dob::Typesystem::TypeId propertyId,
                           const Dob::Typesystem::MemberIndex member,
                           const Dob::Typesystem::ArrayIndex index);

        /**
         * Is the property member changed.
         *
         * @param object [in] - The object to check inside.
         * @param propertyId [in] - TypeId of the property to use.
         * @param member [in] - Index of the property member.
         * @param index [in] - Array index.
         * @return True if the property member (or a parent item of it) was changed.
         */
        static bool IsChanged(const Dob::Typesystem::ObjectPtr object,
                              const Dob::Typesystem::TypeId propertyId,
                              const Dob::Typesystem::MemberIndex member,
                              const Dob::Typesystem::ArrayIndex index);

        /**
         * Is the property member read-only.
         *
         * A property member is read-only if it
         *
         *   1. is mapped to null,
         *
         *   2. is mapped to a parameter, or
         *
         *   3. the item containing the member in the object is null.
         *
         * @param object [in] - The object to check inside.
         * @param propertyId [in] - TypeId of the property to use.
         * @param member [in] - Index of the property member.
         * @param index [in] - Array index.
         * @return True if the property member is read only.
         */
        static bool IsReadOnly(const Dob::Typesystem::ObjectPtr object,
                               const Dob::Typesystem::TypeId propertyId,
                               const Dob::Typesystem::MemberIndex member,
                               const Dob::Typesystem::ArrayIndex index);

        /**
         * Set a boolean property member in the object using a property.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [in] - The value to set the member to.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void Set(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const bool value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Get a boolean property member from the object using a property.
         *
         * @param object [in] - The object to read from.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [out] - The value of the member.
         * @param member [in] - Index of the property member to read from.
         * @param index [in] - Array index.
         * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
         * @throws NullException The member is null.
         */
        static void Get(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        bool & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);



        /**
         * Set an enumeration property member in the object using a property.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [in] - The value to set the member to.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void SetEnum(Dob::Typesystem::ObjectPtr object,
                            const Dob::Typesystem::TypeId propertyId,
                            const Dob::Typesystem::EnumerationValue value,
                            const Dob::Typesystem::MemberIndex member,
                            const Dob::Typesystem::ArrayIndex index);

        /**
         * Get an enumeration property member from the object using a property.
         *
         * @param object [in] - The object to read from.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [out] - The value of the member.
         * @param member [in] - Index of the property member to read from.
         * @param index [in] - Array index.
         * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
         * @throws NullException The member is null.
         */
        static void GetEnum(const Dob::Typesystem::ObjectPtr object,
                            const Dob::Typesystem::TypeId propertyId,
                            Dob::Typesystem::EnumerationValue & value,
                            const Dob::Typesystem::MemberIndex member,
                            const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an Int32 property member in the object using a property.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [in] - The value to set the member to.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void Set(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const Dob::Typesystem::Int32 value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Get an Int32 property member from the object using a property.
         *
         * @param object [in] - The object to read from.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [out] - The value of the member.
         * @param member [in] - Index of the property member to read from.
         * @param index [in] - Array index.
         * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
         * @throws NullException The member is null.
         */
        static void Get(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        Dob::Typesystem::Int32 & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an Int64 property member in the object using a property.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [in] - The value to set the member to.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void Set(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const Dob::Typesystem::Int64 value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Get an Int64 property member from the object using a property.
         *
         * @param object [in] - The object to read from.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [out] - The value of the member.
         * @param member [in] - Index of the property member to read from.
         * @param index [in] - Array index.
         * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
         * @throws NullException The member is null.
         */
        static void Get(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        Dob::Typesystem::Int64 & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set a Float32 property member in the object using a property.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [in] - The value to set the member to.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void Set(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const Dob::Typesystem::Float32 value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Get a Float32 property member from the object using a property.
         *
         * @param object [in] - The object to read from.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [out] - The value of the member.
         * @param member [in] - Index of the property member to read from.
         * @param index [in] - Array index.
         * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
         * @throws NullException The member is null.
         */
        static void Get(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        Dob::Typesystem::Float32 & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set a Float64 property member in the object using a property.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [in] - The value to set the member to.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void Set(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const Dob::Typesystem::Float64 value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Get a Float64 property member from the object using a property.
         *
         * @param object [in] - The object to read from.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [out] - The value of the member.
         * @param member [in] - Index of the property member to read from.
         * @param index [in] - Array index.
         * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
         * @throws NullException The member is null.
         */
        static void Get(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        Dob::Typesystem::Float64 & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an InstanceId property member in the object using a property.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [in] - The value to set the member to.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void Set(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const InstanceId & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Get an InstanceId property member from the object using a property.
         *
         * @param object [in] - The object to read from.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [out] - The value of the member.
         * @param member [in] - Index of the property member to read from.
         * @param index [in] - Array index.
         * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
         * @throws NullException The member is null.
         */
        static void Get(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        InstanceId & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an EntityId property member in the object using a property.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [in] - The value to set the member to.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void Set(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const Dob::Typesystem::EntityId & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Get an EntityId property member from the object using a property.
         *
         * @param object [in] - The object to read from.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [out] - The value of the member.
         * @param member [in] - Index of the property member to read from.
         * @param index [in] - Array index.
         * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
         * @throws NullException The member is null.
         */
        static void Get(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        Dob::Typesystem::EntityId & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set a ChannelId property member in the object using a property.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [in] - The value to set the member to.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void Set(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const ChannelId & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Get a ChannelId property member from the object using a property.
         *
         * @param object [in] - The object to read from.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [out] - The value of the member.
         * @param member [in] - Index of the property member to read from.
         * @param index [in] - Array index.
         * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
         * @throws NullException The member is null.
         */
        static void Get(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        ChannelId & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set a HandlerId property member in the object using a property.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [in] - The value to set the member to.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void Set(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const HandlerId & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Get a HandlerId property member from the object using a property.
         *
         * @param object [in] - The object to read from.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [out] - The value of the member.
         * @param member [in] - Index of the property member to read from.
         * @param index [in] - Array index.
         * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
         * @throws NullException The member is null.
         */
        static void Get(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        HandlerId & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set a string property member in the object using a property.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [in] - The value to set the member to.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void Set(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const std::wstring & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Get a string property member from the object using a property.
         *
         * @param object [in] - The object to read from.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [out] - The value of the member.
         * @param member [in] - Index of the property member to read from.
         * @param index [in] - Array index.
         * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
         * @throws NullException The member is null.
         */
        static void Get(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        std::wstring & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set an object property member in the object using a property.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param ptr [in] - The object to set the member to.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void Set(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const Dob::Typesystem::ObjectPtr ptr,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Get an object property member from the object using a property.
         *
         * @param object [in] - The object to read from.
         * @param propertyId [in] - TypeId of the property to use.
         * @param ptr [out] - The value of the member.
         * @param member [in] - Index of the property member to read from.
         * @param index [in] - Array index.
         * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
         * @throws NullException The member is null.
         */
        static void Get(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        Dob::Typesystem::ObjectPtr & ptr,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);

        /**
         * Set a binary property member in the object using a property.
         *
         * @param object [in,out] - The object to modify.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [in] - The binary data to set the member to.
         * @param member [in] - Index of the property member to modify.
         * @param index [in] - Array index.
         * @throws ReadOnlyException If the property member is read-only.
         */
        static void Set(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const Dob::Typesystem::Binary & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);


        /**
         * Get a binary property member from the object using a property.
         *
         * @param object [in] - The object to read from.
         * @param propertyId [in] - TypeId of the property to use.
         * @param value [out] - The value of the member.
         * @param member [in] - Index of the property member to read from.
         * @param index [in] - Array index.
         * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
         * @throws NullException The member is null.
         */
        static void Get(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        Dob::Typesystem::Binary & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index);





    private:
        static  void DereferenceClassMemberReference(Dob::Typesystem::Object & object,
                                                     DotsC_Int32 const * const classmemberref,
                                                     const DotsC_Int32 refSize,
                                                     const Dob::Typesystem::ArrayIndex index,
                                                     Dob::Typesystem::ContainerBase * & container, //out
                                                     bool & parentIsChanged);           //out


        static  void DereferenceClassMemberReference(const Dob::Typesystem::Object & object,
                                                     DotsC_Int32 const * const classmemberref,
                                                     const DotsC_Int32 refSize,
                                                     const Dob::Typesystem::ArrayIndex index,
                                                     Dob::Typesystem::ContainerBase const * & container, //out
                                                     bool & parentIsChanged);           //out

    };
}
}
}

#endif

