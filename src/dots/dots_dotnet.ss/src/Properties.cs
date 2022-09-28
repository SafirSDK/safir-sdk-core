/* ****************************************************************************
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

using System;
using System.Runtime.InteropServices;

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// Functions for getting property information from types and objects.
    /// <para/>
    /// With these operations you can get and set members on objects using properties.
    /// If you need member information (names, typeids etc) from a property, use the
    /// functions in Members. (For array sizes, use the method in this class though...).
    /// </summary>
    public class Properties
    {
        /// <summary>
        /// Get the information of how a property member is mapped for a class.
        /// </summary>
        /// <param name="classId">Type id of a class that supports the specified property.</param>
        /// <param name="propertyId">Type id of the property</param>
        /// <param name="propertyMember">Index of the property member.</param>
        /// <returns>The PropertyMappingKind.</returns>
        /// <exception cref="IllegalValueException">There is no such type or member defined.</exception>
        public static PropertyMappingKind GetMappingKind(System.Int64 classId,
                                                         System.Int64 propertyId,
                                                         System.Int32 propertyMember)
        {
            PropertyMappingKind kind;
            byte okByte=Internal.Kernel.DotsC_GetPropertyMappingKind (classId, propertyId, propertyMember, out kind);
            if (Internal.InternalOperations.BoolOf (okByte)) {
                return kind;
            }

            throw new IllegalValueException("That object is not mapped to that property!");
        }

        /// <summary>
        /// Get the array size of a property member.
        /// </summary>
        /// <param name="classId">Type id of a class that supports the specified property.</param>
        /// <param name="propertyId">Type id of the property</param>
        /// <param name="propertyMember">Index of the property member.</param>
        /// <returns>The array size of the property member.</returns>
        /// <exception cref="IllegalValueException">There is no such type or member defined.</exception>
        public static int GetArraySize(System.Int64 classId,
                                       System.Int64 propertyId,
                                       System.Int32 propertyMember)
        {
            int result = Internal.Kernel.DotsC_GetMemberArraySizeProperty(classId, propertyId, propertyMember);
            if (result == -1)
            {
                throw new IllegalValueException("No such type or array or mapping defined");
            }
            else
            {
                return result;
            }
        }

        #region Status methods

        /// <summary>
        ///  Set a property member to null.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void SetNull(Object obj,
                                   System.Int64 propertyId,
                                   System.Int32 member,
                                   System.Int32 index)
        {
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                return;

            case PropertyMappingKind.MappedToParameter:
                throw new ReadOnlyException("Property member is mapped to parameter");

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    return; //parent is null
                }
                else
                {
                    container.SetNull();
                }
            }
            break;
            }
        }

        /// <summary>
        /// Is the property member null.
        /// </summary>
        /// <param name="obj">The object to check inside.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="member">Index of the property member.</param>
        /// <param name="index">Array index.</param>
        /// <returns>True if the property member (or a parent item of it) was null.</returns>
        public static bool IsNull(Object obj,
                                  System.Int64 propertyId,
                                  System.Int32 member,
                                  System.Int32 index)
        {
            switch (GetMappingKind(obj.GetTypeId(), propertyId, member))
            {
            case PropertyMappingKind.MappedToNull:
                return true;

            case PropertyMappingKind.MappedToParameter:
                return false;

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null || container.IsNull())
                {
                    return true; //parent or container is null
                }
                else
                {
                    return false;
                }
            }
            }
            throw new SoftwareViolationException("Coding error in Properties.IsNull");
        }

        /// <summary>
        /// Is the property member changed.
        /// </summary>
        /// <param name="obj">The object to check inside.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="member">Index of the property member.</param>
        /// <param name="index">Array index.</param>
        /// <returns>True if the property member (or a parent item of it) was changed.</returns>
        public static bool IsChanged(Object obj,
                                     System.Int64 propertyId,
                                     System.Int32 member,
                                     System.Int32 index)
        {
            switch (GetMappingKind(obj.GetTypeId(), propertyId, member))
            {
            case PropertyMappingKind.MappedToNull:
                return false;

            case PropertyMappingKind.MappedToParameter:
                return false;

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);

                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (parentIsChanged)
                {
                    return true;
                }
                if (container != null && container.IsChanged())
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            }
            throw new SoftwareViolationException("Coding error in Properties.IsChanged");
        }

        /// <summary>
        /// Is the property member read-only.
        /// <para/>
        /// A property member is read-only if it is mapped to null, is mapped to a parameter, or
        /// the item containing the member in the object is null.
        /// </summary>
        /// <param name="obj">The object to check inside.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="member">Index of the property member.</param>
        /// <param name="index">Array index.</param>
        /// <returns>True if the property member is read only.</returns>
        public static bool IsReadOnly(Object obj,
                                      System.Int64 propertyId,
                                      System.Int32 member,
                                      System.Int32 index)
        {
            switch (GetMappingKind(obj.GetTypeId(), propertyId, member))
            {
            case PropertyMappingKind.MappedToNull:
                return true;

            case PropertyMappingKind.MappedToParameter:
                return true;

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);

                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);

                if (container == null)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            }
            throw new SoftwareViolationException("Coding error in Properties.IsChanged");
        }

        #endregion

        #region Get and Set methods

        /// <summary>
        /// Set a boolean property member in the object using a property.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value to set the member to.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void Set(Object obj,
                        System.Int64 propertyId,
                        bool val,
                        System.Int32 member,
                        System.Int32 index)
        {
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new ReadOnlyException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
                throw new ReadOnlyException("Property member is mapped to parameter");

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((BooleanContainer)container).Val = val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Get a boolean property member from the object using a property.
        /// </summary>
        /// <param name="obj">The object to read from.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value of the member.</param>
        /// <param name="member">Index of the property member to read from.</param>
        /// <param name="index">Array index.</param>
        public static void Get(Object obj,
                        System.Int64 propertyId,
                        out bool val,
                        System.Int32 member,
                        System.Int32 index)
        {
            val = true;
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new NullException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
                {
                    Int64 paramClassTypeId;
                    Int32 paramIndex;
                    Int32 valueIndex;
                    Internal.Kernel.DotsC_GetPropertyParameterReference (obj.GetTypeId(), propertyId, member, index, out paramClassTypeId, out paramIndex, out valueIndex);

                    byte tmpVal;
                    Internal.Kernel.DotsC_GetBooleanParameter(paramClassTypeId, paramIndex, valueIndex, out tmpVal);
                    val = Internal.InternalOperations.BoolOf(tmpVal);
                }
                break;

            case PropertyMappingKind.MappedToMember:
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    val = ((BooleanContainer)container).Val;
                }
                break;
            }
        }

        /// <summary>
        /// Set an enumeration property member in the object using a property.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value to set the member to.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void SetEnum(Object obj,
                            System.Int64 propertyId,
                            System.Int32 val,
                            System.Int32 member,
                            System.Int32 index)
        {
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new ReadOnlyException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
                throw new ReadOnlyException("Property member is mapped to parameter");

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((EnumerationContainerBase)container).Ordinal = val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Get an enumeration property member from the object using a property.
        /// </summary>
        /// <param name="obj">The object to read from.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value of the member.</param>
        /// <param name="member">Index of the property member to read from.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The member is inaccessible. Some "parent" item is null.</exception>
        /// <exception cref="NullException">The member is null.</exception>
        public static void GetEnum(Object obj,
                            System.Int64 propertyId,
                            out System.Int32 val,
                            System.Int32 member,
                            System.Int32 index)
        {
            val = 0;
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new NullException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
            {
                Int64 paramClassTypeId;
                Int32 paramIndex;
                Int32 valueIndex;
                Internal.Kernel.DotsC_GetPropertyParameterReference (obj.GetTypeId(), propertyId, member, index, out paramClassTypeId, out paramIndex, out valueIndex);
                Internal.Kernel.DotsC_GetInt32Parameter(paramClassTypeId, paramIndex, valueIndex, KeyValMode.ValueMode, out val);
            }
            break;

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    val = ((EnumerationContainerBase)container).Ordinal;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Set an Int32 property member in the object using a property.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value to set the member to.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void Set(Object obj,
                        System.Int64 propertyId,
                        System.Int32 val,
                        System.Int32 member,
                        System.Int32 index)
        {
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new ReadOnlyException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
                throw new ReadOnlyException("Property member is mapped to parameter");

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((Int32Container)container).Val = val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Get an Int32 property member from the object using a property.
        /// </summary>
        /// <param name="obj">The object to read from.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value of the member.</param>
        /// <param name="member">Index of the property member to read from.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The member is inaccessible. Some "parent" item is null.</exception>
        /// <exception cref="NullException">The member is null.</exception>
        public static void Get(Object obj,
                 System.Int64 propertyId,
                 out System.Int32 val,
                 System.Int32 member,
                 System.Int32 index)
        {
            val = 0;
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new NullException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
            {
                Int64 paramClassTypeId;
                Int32 paramIndex;
                Int32 valueIndex;
                Internal.Kernel.DotsC_GetPropertyParameterReference (obj.GetTypeId(), propertyId, member, index, out paramClassTypeId, out paramIndex, out valueIndex);
                Internal.Kernel.DotsC_GetInt32Parameter(paramClassTypeId, paramIndex, valueIndex, KeyValMode.ValueMode, out val);
            }
            break;

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    val = ((Int32Container)container).Val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Set an Int64 property member in the object using a property.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value to set the member to.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void Set(Object obj,
                       System.Int64 propertyId,
                       System.Int64 val,
                       System.Int32 member,
                       System.Int32 index)
        {
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new ReadOnlyException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
                throw new ReadOnlyException("Property member is mapped to parameter");

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((Int64Container)container).Val = val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Get an Int64 property member from the object using a property.
        /// </summary>
        /// <param name="obj">The object to read from.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value of the member.</param>
        /// <param name="member">Index of the property member to read from.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The member is inaccessible. Some "parent" item is null.</exception>
        /// <exception cref="NullException">The member is null.</exception>
        public static void Get(Object obj,
                 System.Int64 propertyId,
                 out System.Int64 val,
                 System.Int32 member,
                 System.Int32 index)
        {
            val = 0;
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new NullException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
            {
                Int64 paramClassTypeId;
                Int32 paramIndex;
                Int32 valueIndex;
                Internal.Kernel.DotsC_GetPropertyParameterReference (obj.GetTypeId(), propertyId, member, index, out paramClassTypeId, out paramIndex, out valueIndex);
                Internal.Kernel.DotsC_GetInt64Parameter(paramClassTypeId, paramIndex, valueIndex, KeyValMode.ValueMode, out val);
            }
            break;

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    val = ((Int64Container)container).Val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Set a float property member in the object using a property.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value to set the member to.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void Set(Object obj,
                               System.Int64 propertyId,
                               float val,
                               System.Int32 member,
                               System.Int32 index)
        {
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new ReadOnlyException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
                throw new ReadOnlyException("Property member is mapped to parameter");

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((Float32Container)container).Val = val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Get a Float32 property member from the object using a property.
        /// </summary>
        /// <param name="obj">The object to read from.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value of the member.</param>
        /// <param name="member">Index of the property member to read from.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The member is inaccessible. Some "parent" item is null.</exception>
        /// <exception cref="NullException">The member is null.</exception>
        public static void Get(Object obj,
                       System.Int64 propertyId,
                       out float val,
                       System.Int32 member,
                       System.Int32 index)
        {
            val = 0;
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new NullException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
            {
                Int64 paramClassTypeId;
                Int32 paramIndex;
                Int32 valueIndex;
                Internal.Kernel.DotsC_GetPropertyParameterReference (obj.GetTypeId(), propertyId, member, index, out paramClassTypeId, out paramIndex, out valueIndex);
                Internal.Kernel.DotsC_GetFloat32Parameter(paramClassTypeId, paramIndex, valueIndex, out val);
            }
            break;

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    val = ((Float32Container)container).Val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Set a double property member in the object using a property.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value to set the member to.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void Set(Object obj,
                               System.Int64 propertyId,
                               double val,
                               System.Int32 member,
                               System.Int32 index)
        {
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new ReadOnlyException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
                throw new ReadOnlyException("Property member is mapped to parameter");

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((Float64Container)container).Val = val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Get a double property member from the object using a property.
        /// </summary>
        /// <param name="obj">The object to read from.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value of the member.</param>
        /// <param name="member">Index of the property member to read from.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The member is inaccessible. Some "parent" item is null.</exception>
        /// <exception cref="NullException">The member is null.</exception>
        public static void Get(Object obj,
                               System.Int64 propertyId,
                               out double val,
                               System.Int32 member,
                               System.Int32 index)
        {
            val = 0;
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new NullException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
            {
                Int64 paramClassTypeId;
                Int32 paramIndex;
                Int32 valueIndex;
                Internal.Kernel.DotsC_GetPropertyParameterReference (obj.GetTypeId(), propertyId, member, index, out paramClassTypeId, out paramIndex, out valueIndex);
                Internal.Kernel.DotsC_GetFloat64Parameter(paramClassTypeId, paramIndex, valueIndex, out val);
            }
            break;

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    val = ((Float64Container)container).Val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Set an InstanceId property member in the object using a property.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value to set the member to.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void Set(Object obj,
                               System.Int64 propertyId,
                               InstanceId val,
                               System.Int32 member,
                               System.Int32 index)
        {
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new ReadOnlyException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
                throw new ReadOnlyException("Property member is mapped to parameter");

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((InstanceIdContainer)container).Val = val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Get an InstanceId property member from the object using a property.
        /// </summary>
        /// <param name="obj">The object to read from.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value of the member.</param>
        /// <param name="member">Index of the property member to read from.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The member is inaccessible. Some "parent" item is null.</exception>
        /// <exception cref="NullException">The member is null.</exception>
        public static void Get(Object obj,
                               System.Int64 propertyId,
                               out InstanceId val,
                               System.Int32 member,
                               System.Int32 index)
        {
            val = null;
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new NullException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
            {
                Int64 hashVal;
                System.IntPtr strVal;

                Int64 paramClassTypeId;
                Int32 paramIndex;
                Int32 valueIndex;
                Internal.Kernel.DotsC_GetPropertyParameterReference (obj.GetTypeId(), propertyId, member, index, out paramClassTypeId, out paramIndex, out valueIndex);
                Internal.Kernel.DotsC_GetHashedIdParameter(paramClassTypeId, paramIndex, valueIndex, KeyValMode.ValueMode, out hashVal, out strVal);

                val = new InstanceId
                       (hashVal,
                        strVal != System.IntPtr.Zero ? Internal.InternalOperations.StringOf(strVal) : "");
            }
            break;

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    val = ((InstanceIdContainer)container).Val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Set an EntityId property member in the object using a property.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value to set the member to.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void Set(Object obj,
                               System.Int64 propertyId,
                               EntityId val,
                               System.Int32 member,
                               System.Int32 index)
        {
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new ReadOnlyException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
                throw new ReadOnlyException("Property member is mapped to parameter");

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((EntityIdContainer)container).Val = val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Get an EntityId property member from the object using a property.
        /// </summary>
        /// <param name="obj">The object to read from.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value of the member.</param>
        /// <param name="member">Index of the property member to read from.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The member is inaccessible. Some "parent" item is null.</exception>
        /// <exception cref="NullException">The member is null.</exception>
        public static void Get(Object obj,
                               System.Int64 propertyId,
                               out EntityId val,
                               System.Int32 member,
                               System.Int32 index)
        {
            val = null;
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new NullException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
            {
                Internal.DotsC_EntityId eid;
                System.IntPtr instanceIdStr;
                Int64 paramClassTypeId;
                Int32 paramIndex;
                Int32 valueIndex;
                Internal.Kernel.DotsC_GetPropertyParameterReference (obj.GetTypeId(), propertyId, member, index, out paramClassTypeId, out paramIndex, out valueIndex);
                Internal.Kernel.DotsC_GetEntityIdParameter(paramClassTypeId, paramIndex, valueIndex, KeyValMode.ValueMode, out eid, out instanceIdStr);

                val = new EntityId
                    (eid.TypeId,
                     new Dob.Typesystem.InstanceId
                        (eid.InstanceId,
                        instanceIdStr != System.IntPtr.Zero ? Internal.InternalOperations.StringOf(instanceIdStr) : ""));
            }
            break;

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    val = ((EntityIdContainer)container).Val;
                }
            }
            break;
            }
        }


        /// <summary>
        /// Set a ChannelId property member in the object using a property.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value to set the member to.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void Set(Object obj,
                               System.Int64 propertyId,
                               ChannelId val,
                               System.Int32 member,
                               System.Int32 index)
        {
            switch (GetMappingKind(obj.GetTypeId(), propertyId, member))
            {
                case PropertyMappingKind.MappedToNull:
                    throw new ReadOnlyException("Property member is mapped to null");

                case PropertyMappingKind.MappedToParameter:
                    throw new ReadOnlyException("Property member is mapped to parameter");

                case PropertyMappingKind.MappedToMember:
                    {
                        System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                                propertyId,
                                                                                member);
                        if (classMemberRef == null || classMemberRef.Length == 0)
                        {
                            throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                        }

                        ContainerBase container;
                        bool parentIsChanged = false;
                        DereferenceClassMemberReference(obj,
                                                        classMemberRef,
                                                        0,
                                                        index,
                                                        out container,
                                                        ref parentIsChanged);
                        if (container == null)
                        {
                            throw new ReadOnlyException("Unable to dereference property, some parent is null");
                        }
                        else
                        {
                            ((ChannelIdContainer)container).Val = val;
                        }
                    }
                    break;
            }
        }

        /// <summary>
        /// Get a ChannelId property member from the object using a property.
        /// </summary>
        /// <param name="obj">The object to read from.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value of the member.</param>
        /// <param name="member">Index of the property member to read from.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The member is inaccessible. Some "parent" item is null.</exception>
        /// <exception cref="NullException">The member is null.</exception>
        public static void Get(Object obj,
                               System.Int64 propertyId,
                               out ChannelId val,
                               System.Int32 member,
                               System.Int32 index)
        {
            val = null;
            switch (GetMappingKind(obj.GetTypeId(), propertyId, member))
            {
                case PropertyMappingKind.MappedToNull:
                    throw new NullException("Property member is mapped to null");

                case PropertyMappingKind.MappedToParameter:
                    {
                        Int64 hashVal;
                        System.IntPtr strVal;
                        Int64 paramClassTypeId;
                        Int32 paramIndex;
                        Int32 valueIndex;
                        Internal.Kernel.DotsC_GetPropertyParameterReference (obj.GetTypeId(), propertyId, member, index, out paramClassTypeId, out paramIndex, out valueIndex);
                        Internal.Kernel.DotsC_GetHashedIdParameter(paramClassTypeId, paramIndex, valueIndex, KeyValMode.ValueMode, out hashVal, out strVal);

                        val = new ChannelId(hashVal,
                                        strVal != System.IntPtr.Zero ? Internal.InternalOperations.StringOf(strVal) : "");
                    }
                    break;

                case PropertyMappingKind.MappedToMember:
                    {
                        System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                                propertyId,
                                                                                member);
                        if (classMemberRef == null || classMemberRef.Length == 0)
                        {
                            throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                        }

                        ContainerBase container;
                        bool parentIsChanged = false;
                        DereferenceClassMemberReference(obj,
                                                        classMemberRef,
                                                        0,
                                                        index,
                                                        out container,
                                                        ref parentIsChanged);
                        if (container == null)
                        {
                            throw new ReadOnlyException("Unable to dereference property, some parent is null");
                        }
                        else
                        {
                            val = ((ChannelIdContainer)container).Val;
                        }
                    }
                    break;
            }
        }

        /// <summary>
        /// Set a HandlerId property member in the object using a property.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value to set the member to.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void Set(Object obj,
                               System.Int64 propertyId,
                               HandlerId val,
                               System.Int32 member,
                               System.Int32 index)
        {
            switch (GetMappingKind(obj.GetTypeId(), propertyId, member))
            {
                case PropertyMappingKind.MappedToNull:
                    throw new ReadOnlyException("Property member is mapped to null");

                case PropertyMappingKind.MappedToParameter:
                    throw new ReadOnlyException("Property member is mapped to parameter");

                case PropertyMappingKind.MappedToMember:
                    {
                        System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                                propertyId,
                                                                                member);
                        if (classMemberRef == null || classMemberRef.Length == 0)
                        {
                            throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                        }

                        ContainerBase container;
                        bool parentIsChanged = false;
                        DereferenceClassMemberReference(obj,
                                                        classMemberRef,
                                                        0,
                                                        index,
                                                        out container,
                                                        ref parentIsChanged);
                        if (container == null)
                        {
                            throw new ReadOnlyException("Unable to dereference property, some parent is null");
                        }
                        else
                        {
                            ((HandlerIdContainer)container).Val = val;
                        }
                    }
                    break;
            }
        }

        /// <summary>
        /// Get a HandlerId property member from the object using a property.
        /// </summary>
        /// <param name="obj">The object to read from.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value of the member.</param>
        /// <param name="member">Index of the property member to read from.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The member is inaccessible. Some "parent" item is null.</exception>
        /// <exception cref="NullException">The member is null.</exception>
        public static void Get(Object obj,
                               System.Int64 propertyId,
                               out HandlerId val,
                               System.Int32 member,
                               System.Int32 index)
        {
            val = null;
            switch (GetMappingKind(obj.GetTypeId(), propertyId, member))
            {
                case PropertyMappingKind.MappedToNull:
                    throw new NullException("Property member is mapped to null");

                case PropertyMappingKind.MappedToParameter:
                    {
                        Int64 hashVal;
                        System.IntPtr strVal;
                        Int64 paramClassTypeId;
                        Int32 paramIndex;
                        Int32 valueIndex;
                        Internal.Kernel.DotsC_GetPropertyParameterReference (obj.GetTypeId(), propertyId, member, index, out paramClassTypeId, out paramIndex, out valueIndex);
                        Internal.Kernel.DotsC_GetHashedIdParameter(paramClassTypeId, paramIndex, valueIndex, KeyValMode.ValueMode, out hashVal, out strVal);

                        val = new HandlerId
                               (hashVal,
                                strVal != System.IntPtr.Zero ? Internal.InternalOperations.StringOf(strVal) : "");
                    }
                    break;

                case PropertyMappingKind.MappedToMember:
                    {
                        System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                                propertyId,
                                                                                member);
                        if (classMemberRef == null || classMemberRef.Length == 0)
                        {
                            throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                        }

                        ContainerBase container;
                        bool parentIsChanged = false;
                        DereferenceClassMemberReference(obj,
                                                        classMemberRef,
                                                        0,
                                                        index,
                                                        out container,
                                                        ref parentIsChanged);
                        if (container == null)
                        {
                            throw new ReadOnlyException("Unable to dereference property, some parent is null");
                        }
                        else
                        {
                            val = ((HandlerIdContainer)container).Val;
                        }
                    }
                    break;
            }
        }

        /// <summary>
        /// Set a string property member in the object using a property.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value to set the member to.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void Set(Object obj,
                       System.Int64 propertyId,
                       string val,
                       System.Int32 member,
                       System.Int32 index)
        {
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new ReadOnlyException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
                throw new ReadOnlyException("Property member is mapped to parameter");

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((StringContainer)container).Val = val;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Get a string property member from the object using a property.
        /// </summary>
        /// <param name="obj">The object to read from.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value of the member.</param>
        /// <param name="member">Index of the property member to read from.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The member is inaccessible. Some "parent" item is null.</exception>
        /// <exception cref="NullException">The member is null.</exception>
        public static void Get(Object obj,
                       System.Int64 propertyId,
                       out string val,
                       System.Int32 member,
                       System.Int32 index)
        {
            val = null;
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new NullException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
            {
                System.IntPtr str;

                Int64 paramClassTypeId;
                Int32 paramIndex;
                Int32 valueIndex;
                Internal.Kernel.DotsC_GetPropertyParameterReference (obj.GetTypeId(), propertyId, member, index, out paramClassTypeId, out paramIndex, out valueIndex);
                Internal.Kernel.DotsC_GetStringParameter(paramClassTypeId, paramIndex, valueIndex, KeyValMode.ValueMode, out str);

                val = Internal.InternalOperations.StringOf(str);
            }
            break;

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    val = ((StringContainer)container).Val;
                }
            }
            break;
            }
        }


        /// <summary>
        /// Set an Object property member in the object using a property.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value to set the member to.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void Set(Object obj,
                       System.Int64 propertyId,
                       Object val,
                       System.Int32 member,
                       System.Int32 index)
        {
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new ReadOnlyException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
                throw new ReadOnlyException("Property member is mapped to parameter");

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((ObjectContainerBase)container).InternalObj = val;
                    ((ObjectContainerBase)container).m_bIsChanged = true;
                }
            }
            break;
            }
        }

        /// <summary>
        /// Get an Object property member from the object using a property.
        /// </summary>
        /// <param name="obj">The object to read from.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value of the member.</param>
        /// <param name="member">Index of the property member to read from.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The member is inaccessible. Some "parent" item is null.</exception>
        /// <exception cref="NullException">The member is null.</exception>
        public static void Get(Object obj,
                       System.Int64 propertyId,
                       out Object val, //out
                       System.Int32 member,
                       System.Int32 index)
        {
            val = null;
            switch(GetMappingKind(obj.GetTypeId(),propertyId,member))
            {
            case PropertyMappingKind.MappedToNull:
                throw new NullException("Property member is mapped to null");

            case PropertyMappingKind.MappedToParameter:
            {
                System.IntPtr blob;

                Int64 paramClassTypeId;
                Int32 paramIndex;
                Int32 valueIndex;
                Internal.Kernel.DotsC_GetPropertyParameterReference (obj.GetTypeId(), propertyId, member, index, out paramClassTypeId, out paramIndex, out valueIndex);
                Internal.Kernel.DotsC_GetObjectParameter(paramClassTypeId, paramIndex, valueIndex, out blob);

                val = ObjectFactory.Instance.CreateObject(blob);
                val.SetChanged(false);
            }
            break;

            case PropertyMappingKind.MappedToMember:
            {
                System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                if (classMemberRef == null || classMemberRef.Length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                out container,
                                                ref parentIsChanged);
                if (container == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    if (((ObjectContainerBase)container).IsNull())
                    {
                        throw new NullException("Object is null");
                    }
                    else
                    {
                        val = ((ObjectContainerBase)container).InternalObj;
                    }
                }
            }
            break;
            }
        }

        /// <summary>
        /// Set a binary property member in the object using a property.
        /// </summary>
        /// <param name="obj">The object to modify.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The binary data to set the member to.</param>
        /// <param name="member">Index of the property member to modify.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The property member is read-only.</exception>
        public static void Set(Object obj,
                               System.Int64 propertyId,
                               byte[] val,
                               System.Int32 member,
                               System.Int32 index)
        {
            switch (GetMappingKind(obj.GetTypeId(), propertyId, member))
            {
                case PropertyMappingKind.MappedToNull:
                    throw new ReadOnlyException("Property member is mapped to null");

                case PropertyMappingKind.MappedToParameter:
                    throw new ReadOnlyException("Property member is mapped to parameter");

                case PropertyMappingKind.MappedToMember:
                    {
                        System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                                propertyId,
                                                                                member);
                        if (classMemberRef == null || classMemberRef.Length == 0)
                        {
                            throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                        }

                        ContainerBase container;
                        bool parentIsChanged = false;
                        DereferenceClassMemberReference(obj,
                                                        classMemberRef,
                                                        0,
                                                        index,
                                                        out container,
                                                        ref parentIsChanged);
                        if (container == null)
                        {
                            throw new ReadOnlyException("Unable to dereference property, some parent is null");
                        }
                        else
                        {
                            ((BinaryContainer)container).Val = val;
                        }
                    }
                    break;
            }
        }

        /// <summary>
        /// Get a binary property member from the object using a property.
        /// </summary>
        /// <param name="obj">The object to read from.</param>
        /// <param name="propertyId">TypeId of the property to use.</param>
        /// <param name="val">The value of the member.</param>
        /// <param name="member">Index of the property member to read from.</param>
        /// <param name="index">Array index.</param>
        /// <exception cref="ReadOnlyException">The member is inaccessible. Some "parent" item is null.</exception>
        /// <exception cref="NullException">The member is null.</exception>
        public static void Get(Object obj,
                               System.Int64 propertyId,
                               out byte[] val, //out
                               System.Int32 member,
                               System.Int32 index)
        {
            val = null;
            switch (GetMappingKind(obj.GetTypeId(), propertyId, member))
            {
                case PropertyMappingKind.MappedToNull:
                    throw new NullException("Property member is mapped to null");

                case PropertyMappingKind.MappedToParameter:
                    {
                        System.IntPtr bin;
                        int size;
                        Int64 paramClassTypeId;
                        Int32 paramIndex;
                        Int32 valueIndex;
                        Internal.Kernel.DotsC_GetPropertyParameterReference (obj.GetTypeId(), propertyId, member, index, out paramClassTypeId, out paramIndex, out valueIndex);
                        Internal.Kernel.DotsC_GetBinaryParameter(paramClassTypeId, paramIndex, valueIndex, out bin, out size);

                        val = new byte[size];
                        Marshal.Copy(bin, val, 0, size);
                    }
                    break;

                case PropertyMappingKind.MappedToMember:
                    {
                        System.Int32[] classMemberRef = GetClassMemberReference(obj.GetTypeId(),
                                                                        propertyId,
                                                                        member);
                        if (classMemberRef == null || classMemberRef.Length == 0)
                        {
                            throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                        }

                        ContainerBase container;
                        bool parentIsChanged = false;
                        DereferenceClassMemberReference(obj,
                                                        classMemberRef,
                                                        0,
                                                        index,
                                                        out container,
                                                        ref parentIsChanged);
                        if (container == null)
                        {
                            throw new ReadOnlyException("Unable to dereference property, some parent is null");
                        }
                        else
                        {
                            val = ((BinaryContainer)container).Val;
                        }
                    }
                    break;
            }
        }

        /// <summary>
        /// Get information needed to read the value of a property that is mapped to a parameter without the need of an ObjectPtr.
        /// This method is only allowed to be called if mappingKind is 'MappedToParameter'.
        /// </summary>
        /// <param name="classId">Type id of a class that supports the specified property.</param>
        /// <param name="propertyId">Type id of the property.</param>
        /// <param name="propertyMember">Index of the property member.</param>
        /// <param name="propertyIndex">Array index of the property.</param>
        /// <param name="parameterTypeId">Mapped parameter type id.</param>
        /// <param name="parameterIndex">Mapped parameter member index.</param>
        /// <param name="parameterArrayIndex">Mapped parameter array index.</param>
        /// <exception cref="IllegalValueException">If no mapping exists or not mapped to a parameter.</exception>
        public static void GetParameterReference(System.Int64 classId,
                                          System.Int64 propertyId,
                                          System.Int32 propertyMember,
                                          System.Int32 propertyIndex,
                                          out System.Int64 parameterTypeId,
                                          out System.Int32 parameterIndex,
                                          out System.Int32 parameterArrayIndex)
        {
            if (GetMappingKind(classId, propertyId, propertyMember) != PropertyMappingKind.MappedToParameter)
            {
                throw new IllegalValueException("That property member is not mapped to a parameter!");
            }

            Internal.Kernel.DotsC_GetPropertyParameterReference(classId, propertyId, propertyMember, propertyIndex, out parameterTypeId, out parameterIndex, out parameterArrayIndex);
        }


        #endregion

        #region Private methods

        private static System.Int32[] GetClassMemberReference(System.Int64 typeId,
                                                              System.Int64 propertyId,
                                                              System.Int32 member)
        {
            System.IntPtr classMemberRef;
            System.Int32 refSize;

            Internal.Kernel.DotsC_GetClassMemberReference(typeId,
                                                          propertyId,
                                                          member,
                                                          out classMemberRef,
                                                          out refSize);
            System.Int32 [] cmr = new System.Int32[refSize];
            for (int i = 0; i < refSize; ++i)
            {
                cmr[i] = Marshal.ReadInt32(classMemberRef, i*4); //4 means that there are four bytes to 32 bits
            }
            return cmr;
        }

        //if container == null then a parent was null
        private static void DereferenceClassMemberReference(Object obj,
                                                            System.Int32 [] classmemberref,
                                                            System.Int32 pos,
                                                            System.Int32 index,
                                                            out ContainerBase container,
                                                            ref bool parentIsChanged)
        {
            if (classmemberref.Length - pos > 2) //we need to recurse into child objects
            {
                ContainerBase member = obj.GetMember(classmemberref[pos],classmemberref[pos + 1]);

                if (member.IsChanged())
                {
                    parentIsChanged = true;
                }

                if (member.IsNull())
                {
                    container = null;
                }
                else
                {
                    DereferenceClassMemberReference(((ObjectContainerBase)member).InternalObj,
                                                    classmemberref,
                                                    pos + 2,
                                                    index,
                                                    out container,
                                                    ref parentIsChanged);
                }
            }
            else
            {
                if (classmemberref[pos + 1] == -1)//pointing at an array, use the index from the function call
                {
                    container = obj.GetMember(classmemberref[pos],index);
                }
                else
                {
                    if (index != 0)
                    {
                        throw new SoftwareViolationException("CMR says that the member is not an array, but I got passed an index != 0");
                    }
                    container = obj.GetMember(classmemberref[pos],classmemberref[pos+1]);
                }
            }
        }

        #endregion

    }
}
