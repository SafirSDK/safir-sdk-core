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

#include <Safir/Dob/Typesystem/Properties.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/ObjectContainer.h>
#include <Safir/Dob/Typesystem/ValueContainers.h>
#include <Safir/Dob/Typesystem/EnumerationContainerBase.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{

    Int32
    Properties::GetArraySize (const TypeId classId,
                              const TypeId propertyId,
                              const MemberIndex propertyMember)
    {
        const Int32 result = DotsC_GetMemberArraySizeProperty(classId, propertyId, propertyMember);
        if (result == -1)
        {
            throw IllegalValueException(L"No such type or array or mapping defined", __WFILE__,__LINE__);
        }
        else
        {
            return result;
        }
    }



    //if container == NULL then a parent was null
    void
    Properties::DereferenceClassMemberReference(Object & object,
                                                const DotsC_Int32 * classmemberref,
                                                const DotsC_Int32 refSize,
                                                const Dob::Typesystem::ArrayIndex index,
                                                ContainerBase * & container, //out
                                                bool & parentIsChanged)           //out
    {
        if (refSize > 2) //we need to recurse into child objects
        {
            ContainerBase & member = object.GetMember(classmemberref[0],classmemberref[1]);

            if (member.IsChanged())
            {
                parentIsChanged = true;
            }

            if (member.IsNull())
            {
                container = NULL;
            }
            else
            {
                DereferenceClassMemberReference(*static_cast<const ObjectContainerBase &>(member).GetObjectPointer().get(),
                                                classmemberref + 2,
                                                refSize - 2,
                                                index,
                                                container,
                                                parentIsChanged);
            }
        }
        else
        {
            if (classmemberref[1] == -1)//pointing at an array, use the index from the function call
            {
                container = &object.GetMember(classmemberref[0],index);
            }
            else
            {
                if (index != 0)
                {
                    throw SoftwareViolationException(L"CMR says that the member is not an array, but I got passed an index != 0",__WFILE__,__LINE__);
                }
                container = &object.GetMember(classmemberref[0],classmemberref[1]);
            }
        }
    }

    void
    Properties::DereferenceClassMemberReference(const Object & object,
                                                const DotsC_Int32 * classmemberref,
                                                const DotsC_Int32 refSize,
                                                const Dob::Typesystem::ArrayIndex index,
                                                ContainerBase const * & container, //out
                                                bool & parentIsChanged)           //out
    {
        if (refSize > 2) //we need to recurse into child objects
        {
            const ContainerBase & member = object.GetMember(classmemberref[0],classmemberref[1]);

            if (member.IsChanged())
            {
                parentIsChanged = true;
            }

            if (member.IsNull())
            {
                container = NULL;
            }
            else
            {
                DereferenceClassMemberReference(*static_cast<const ObjectContainerBase &>(member).GetObjectPointer().get(),
                                                classmemberref + 2,
                                                refSize - 2,
                                                index,
                                                container,
                                                parentIsChanged);
            }
        }
        else
        {
            if (classmemberref[1] == -1)//pointing at an array, use the index from the function call
            {
                container = &object.GetMember(classmemberref[0],index);
            }
            else
            {
                if (index != 0)
                {
                    throw SoftwareViolationException(L"CMR says that the member is not an array, but I got passed an index != 0",__WFILE__,__LINE__);
                }
                container = &object.GetMember(classmemberref[0],classmemberref[1]);
            }
        }
    }

    DotsC_PropertyMappingKind
    GetPropertyMappingKind(const Dob::Typesystem::TypeId typeId,
                           const Dob::Typesystem::TypeId propertyId,
                           const Dob::Typesystem::MemberIndex member)
    {
        DotsC_PropertyMappingKind kind;
        if (DotsC_GetPropertyMappingKind(typeId,
                                     propertyId,
                                     member,
                                     kind))
        {
            return kind;
        }

        throw IllegalValueException(L"That object is not mapped to that property!",__WFILE__,__LINE__);
    }

    void
    Properties::SetNull(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                return;
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    return; //parent is null
                }
                else
                {
                    container->SetNull();
                }
            }
            break;
        }

    }

    bool
    Properties::IsNull(const Dob::Typesystem::ObjectPtr object,
                       const Dob::Typesystem::TypeId propertyId,
                       const Dob::Typesystem::MemberIndex member,
                       const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                return true;
            }
            break;

        case MappedToParameter:
            {
                return false;
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL || container->IsNull())
                {
                    return true; //parent or container is null
                }
                else
                {
                    return false;
                }
            }
            break;
        }
        throw SoftwareViolationException(L"Coding error in Properties::IsNull",__WFILE__,__LINE__);
    }


    bool
    Properties::IsChanged(const Dob::Typesystem::ObjectPtr object,
                          const Dob::Typesystem::TypeId propertyId,
                          const Dob::Typesystem::MemberIndex member,
                          const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                return false;
            }
            break;

        case MappedToParameter:
            {
                return false;
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (parentIsChanged)
                {
                    return true;
                }
                if (container != NULL && container->IsChanged())
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            break;
        }
        throw SoftwareViolationException(L"Coding error in Properties::IsChanged",__WFILE__,__LINE__);
    }


    bool
    Properties::IsReadOnly(const Dob::Typesystem::ObjectPtr object,
                           const Dob::Typesystem::TypeId propertyId,
                           const Dob::Typesystem::MemberIndex member,
                           const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                return true;
            }
            break;

        case MappedToParameter:
            {
                return true;
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);

                if (container == NULL)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            break;
        }
        throw SoftwareViolationException(L"Coding error in Properties::IsChanged",__WFILE__,__LINE__);
    }

    //
    // bool
    //
    void
    Properties::Set(Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    const bool value,
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw ReadOnlyException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    static_cast<BooleanContainer*>(container)->SetVal(value);
                }
            }
            break;
        }
    }


    void
    Properties::Get(const Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    bool & value, //out
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw NullException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                DotsC_ParameterIndex paramIndex;
                DotsC_Int32 valueIndex;
                DotsC_GetPropertyParameterReference(object->GetTypeId(), propertyId, member, index, paramIndex, valueIndex);
                DotsC_GetBooleanParameter(object->GetTypeId(), paramIndex, valueIndex, value);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    value = static_cast<BooleanContainer const *>(container)->GetVal();
                }
            }
            break;
        }
    }

    //
    // enum
    //

    void
    Properties::SetEnum(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const Dob::Typesystem::EnumerationValue value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw ReadOnlyException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    static_cast<EnumerationContainerBase*>(container)->SetOrdinal(value);
                }
            }
            break;
        }
    }

    void
    Properties::GetEnum(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        Dob::Typesystem::EnumerationValue & value, //out
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw NullException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                DotsC_ParameterIndex paramIndex;
                DotsC_Int32 valueIndex;
                DotsC_GetPropertyParameterReference(object->GetTypeId(), propertyId, member, index, paramIndex, valueIndex);
                DotsC_GetEnumerationParameter(object->GetTypeId(), paramIndex, valueIndex, DotsC_ValueMode, value);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    value = static_cast<EnumerationContainerBase const *>(container)->GetOrdinal();
                }
            }
            break;
        }
    }

    //
    //Int32
    //
    void
    Properties::Set(Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    const Dob::Typesystem::Int32 value,
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw ReadOnlyException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    static_cast<Int32Container*>(container)->SetVal(value);
                }
            }
            break;
        }
    }


    void
    Properties::Get(const Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    Dob::Typesystem::Int32 & value, //out
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw NullException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                DotsC_ParameterIndex paramIndex;
                DotsC_Int32 valueIndex;
                DotsC_GetPropertyParameterReference(object->GetTypeId(), propertyId, member, index, paramIndex, valueIndex);
                DotsC_GetInt32Parameter(object->GetTypeId(), paramIndex, valueIndex, DotsC_ValueMode, value);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    value = static_cast<Int32Container const *>(container)->GetVal();
                }
            }
            break;
        }
    }

    //
    //Int64
    //
    void
    Properties::Set(Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    const Dob::Typesystem::Int64 value,
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw ReadOnlyException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    static_cast<Int64Container*>(container)->SetVal(value);
                }
            }
            break;
        }
    }


    void
    Properties::Get(const Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    Dob::Typesystem::Int64 & value, //out
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw NullException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                DotsC_ParameterIndex paramIndex;
                DotsC_Int32 valueIndex;
                DotsC_GetPropertyParameterReference(object->GetTypeId(), propertyId, member, index, paramIndex, valueIndex);
                DotsC_GetInt64Parameter(object->GetTypeId(), paramIndex, valueIndex, DotsC_ValueMode, value);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    value = static_cast<Int64Container const *>(container)->GetVal();
                }
            }
            break;
        }
    }



    //
    //Float32
    //
    void
    Properties::Set(Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    const Dob::Typesystem::Float32 value,
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw ReadOnlyException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    static_cast<Float32Container*>(container)->SetVal(value);
                }
            }
            break;
        }
    }


    void
    Properties::Get(const Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    Dob::Typesystem::Float32 & value, //out
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw NullException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                DotsC_ParameterIndex paramIndex;
                DotsC_Int32 valueIndex;
                DotsC_GetPropertyParameterReference(object->GetTypeId(), propertyId, member, index, paramIndex, valueIndex);
                DotsC_GetFloat32Parameter(object->GetTypeId(), paramIndex, valueIndex, value);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    value = static_cast<Float32Container const *>(container)->GetVal();
                }
            }
            break;
        }
    }

    //
    //Float64
    //
    void
    Properties::Set(Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    const Dob::Typesystem::Float64 value,
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw ReadOnlyException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    static_cast<Float64Container*>(container)->SetVal(value);
                }
            }
            break;
        }
    }


    void
    Properties::Get(const Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    Dob::Typesystem::Float64 & value, //out
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw NullException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                DotsC_ParameterIndex paramIndex;
                DotsC_Int32 valueIndex;
                DotsC_GetPropertyParameterReference(object->GetTypeId(), propertyId, member, index, paramIndex, valueIndex);
                DotsC_GetFloat64Parameter(object->GetTypeId(), paramIndex, valueIndex, value);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    value = static_cast<Float64Container const *>(container)->GetVal();
                }
            }
            break;
        }
    }


    //
    //InstanceId
    //
    void
    Properties::Set(Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    const Dob::Typesystem::InstanceId & value,
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw ReadOnlyException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    static_cast<InstanceIdContainer*>(container)->SetVal(value);
                }
            }
            break;
        }
    }


    void
    Properties::Get(const Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    Dob::Typesystem::InstanceId & value, //out
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw NullException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                Int64 hashVal;
                const char * instanceIdStr = NULL;
                DotsC_ParameterIndex paramIndex;
                DotsC_Int32 valueIndex;
                DotsC_GetPropertyParameterReference(object->GetTypeId(), propertyId, member, index, paramIndex, valueIndex);
                DotsC_GetHashedIdParameter(object->GetTypeId(), paramIndex, valueIndex, DotsC_ValueMode, hashVal, instanceIdStr);

                if (instanceIdStr == NULL)
                {
                    value = InstanceId(hashVal);
                }
                else
                {
                    value = InstanceId(hashVal,Utilities::ToWstring(instanceIdStr));
                }
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    value = static_cast<InstanceIdContainer const *>(container)->GetVal();
                }
            }
            break;
        }
    }



    //
    //EntityId
    //
    void
    Properties::Set(Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    const Dob::Typesystem::EntityId & value,
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw ReadOnlyException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    static_cast<EntityIdContainer*>(container)->SetVal(value);
                }
            }
            break;
        }
    }


    void
    Properties::Get(const Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    Dob::Typesystem::EntityId & value, //out
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw NullException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                DotsC_EntityId eid;
                const char * instanceIdStr;                
                DotsC_ParameterIndex paramIndex;
                DotsC_Int32 valueIndex;
                DotsC_GetPropertyParameterReference(object->GetTypeId(), propertyId, member, index, paramIndex, valueIndex);
                DotsC_GetEntityIdParameter(object->GetTypeId(), paramIndex, valueIndex, DotsC_ValueMode, eid, instanceIdStr);

                if (instanceIdStr == NULL)
                {
                    value = EntityId(eid.typeId,InstanceId(eid.instanceId));
                }
                else
                {
                    value = EntityId(eid.typeId,InstanceId(eid.instanceId,Utilities::ToWstring(instanceIdStr)));
                }
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    value = static_cast<EntityIdContainer const *>(container)->GetVal();
                }
            }
            break;
        }
    }


    //
    //ChannelId
    //
    void
    Properties::Set(Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    const Dob::Typesystem::ChannelId & value,
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw ReadOnlyException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    static_cast<ChannelIdContainer*>(container)->SetVal(value);
                }
            }
            break;
        }
    }


    void
    Properties::Get(const Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    Dob::Typesystem::ChannelId & value, //out
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw NullException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                Int64 hashVal;
                const char * channelIdStr = NULL;
                DotsC_ParameterIndex paramIndex;
                DotsC_Int32 valueIndex;
                DotsC_GetPropertyParameterReference(object->GetTypeId(), propertyId, member, index, paramIndex, valueIndex);
                DotsC_GetHashedIdParameter(object->GetTypeId(), paramIndex, valueIndex, DotsC_ValueMode, hashVal, channelIdStr);

                if (channelIdStr == NULL)
                {
                    value = ChannelId(hashVal);
                }
                else
                {
                    value = ChannelId(hashVal,Utilities::ToWstring(channelIdStr));
                }
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    value = static_cast<ChannelIdContainer const *>(container)->GetVal();
                }
            }
            break;
        }
    }



    //
    //HandlerId
    //
    void
    Properties::Set(Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    const Dob::Typesystem::HandlerId & value,
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw ReadOnlyException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    static_cast<HandlerIdContainer*>(container)->SetVal(value);
                }
            }
            break;
        }
    }


    void
    Properties::Get(const Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    Dob::Typesystem::HandlerId & value, //out
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw NullException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                Int64 hashVal;
                const char * handlerIdStr = NULL;
                DotsC_ParameterIndex paramIndex;
                DotsC_Int32 valueIndex;
                DotsC_GetPropertyParameterReference(object->GetTypeId(), propertyId, member, index, paramIndex, valueIndex);
                DotsC_GetHashedIdParameter(object->GetTypeId(), paramIndex, valueIndex, DotsC_ValueMode, hashVal, handlerIdStr);

                if (handlerIdStr == NULL)
                {
                    value = HandlerId(hashVal);
                }
                else
                {
                    value = HandlerId(hashVal,Utilities::ToWstring(handlerIdStr));
                }
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    value = static_cast<HandlerIdContainer const *>(container)->GetVal();
                }
            }
            break;
        }
    }




    //
    //String
    //
    void
    Properties::Set(Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    const std::wstring & value,
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw ReadOnlyException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    static_cast<StringContainer*>(container)->SetVal(value);
                }
            }
            break;
        }
    }


    void
    Properties::Get(const Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    std::wstring & value, //out
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw NullException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                const char* str;
                DotsC_ParameterIndex paramIndex;
                DotsC_Int32 valueIndex;
                DotsC_GetPropertyParameterReference(object->GetTypeId(), propertyId, member, index, paramIndex, valueIndex);
                DotsC_GetStringParameter(object->GetTypeId(), paramIndex, valueIndex, DotsC_ValueMode, str);
                value = Utilities::ToWstring(str);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    value = static_cast<StringContainer const *>(container)->GetVal();
                }
            }
            break;
        }
    }



    //
    //ObjectPtr
    //
    void
    Properties::Set(Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    const Dob::Typesystem::ObjectPtr ptr,
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw ReadOnlyException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    static_cast<ObjectContainerBase*>(container)->SetPtr(ptr);
                }
            }
            break;
        }
    }


    void
    Properties::Get(const Dob::Typesystem::ObjectPtr object,
                    const Dob::Typesystem::TypeId propertyId,
                    Dob::Typesystem::ObjectPtr & ptr, //out
                    const Dob::Typesystem::MemberIndex member,
                    const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw NullException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                const char* blob;
                DotsC_ParameterIndex paramIndex;
                DotsC_Int32 valueIndex;
                DotsC_GetPropertyParameterReference(object->GetTypeId(), propertyId, member, index, paramIndex, valueIndex);
                DotsC_GetObjectParameter(object->GetTypeId(), paramIndex, valueIndex, blob);

                ptr = ObjectFactory::Instance().CreateObject(blob);
                ptr->SetChanged(false);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    if (static_cast<ObjectContainerBase const *>(container)->IsNull())
                    {
                        throw NullException(L"Object is null",__WFILE__,__LINE__);
                    }
                    else
                    {
                        ptr = static_cast<ObjectContainerBase const *>(container)->GetObjectPointer();
                    }
                }
            }
            break;
        }
    }

    //
    // Binary
    //

    void Properties::Set(Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        const Dob::Typesystem::Binary & value,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw ReadOnlyException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                throw ReadOnlyException(L"Property member is mapped to parameter",__WFILE__,__LINE__);
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    static_cast<BinaryContainer*>(container)->SetVal(value);
                }
            }
            break;
        }
    }



    void Properties::Get(const Dob::Typesystem::ObjectPtr object,
                        const Dob::Typesystem::TypeId propertyId,
                        Dob::Typesystem::Binary & binary,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        switch(GetPropertyMappingKind(object->GetTypeId(),propertyId,member))
        {
        case MappedToNull:
            {
                throw NullException(L"Property member is mapped to null",__WFILE__,__LINE__);
            }
            break;

        case MappedToParameter:
            {
                char const * bin;
                DotsC_Int32 size;
                DotsC_ParameterIndex paramIndex;
                DotsC_Int32 valueIndex;
                DotsC_GetPropertyParameterReference(object->GetTypeId(), propertyId, member, index, paramIndex, valueIndex);
                DotsC_GetBinaryParameter(object->GetTypeId(), paramIndex, valueIndex, bin, size);

                binary.clear();
                binary.reserve(size);
                for (int i=0; i<size; i++)
                {
                    binary.push_back(bin[i]);
                }
            }
            break;

        case MappedToMember:
            {
                DotsC_Int32 const * classMemberRef = NULL;
                DotsC_Int32 refSize;

                DotsC_GetClassMemberReference(object->GetTypeId(),
                                              propertyId,
                                              member,
                                              classMemberRef,
                                              refSize);
                if (classMemberRef == NULL || refSize == 0)
                {
                    throw SoftwareViolationException(L"Failed to get class member reference from dots_kernel",__WFILE__,__LINE__);
                }

                ContainerBase const * container;
                bool parentIsChanged = false;
                DereferenceClassMemberReference(*object,
                                                classMemberRef,
                                                refSize,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container == NULL)
                {
                    throw ReadOnlyException(L"Unable to dereference property, some parent is null",__WFILE__,__LINE__);
                    return; //parent is null
                }
                else
                {
                    binary = static_cast<BinaryContainer const *>(container)->GetVal();
                }
            }
            break;
        }
    }

}
}
}
