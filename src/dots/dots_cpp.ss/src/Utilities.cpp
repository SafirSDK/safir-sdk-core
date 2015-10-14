/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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

#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Members.h>
#include <Safir/Dob/Typesystem/Object.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/ObjectContainer.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Utilities/Internal/StringEncoding.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <iostream>
#include <boost/shared_array.hpp>
#include <Safir/Dob/Typesystem/ValueContainers.h>
#include <typeinfo>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4702)
#endif

#include <boost/lexical_cast.hpp>

//and enable the warnings again
#if defined _MSC_VER
  #pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Utilities
{
    const std::string ToUtf8(const std::wstring & wstr)
    {
        return Safir::Utilities::Internal::ToUtf8(wstr);
    }

    const std::wstring ToWstring(const std::string & str)
    {
         return Safir::Utilities::Internal::ToUtf16(str);
    }



    void MergeChanges(ObjectPtr into, const ObjectPtr from)
    {
        if (from == NULL || into == NULL)
        {
            throw SoftwareViolationException(L"Objects must not be null in call to MergeChanges",__WFILE__,__LINE__);
        }

        if (from->GetTypeId() != into->GetTypeId())
        {
            throw SoftwareViolationException(L"Objects must have same TypeId for MergeChanges",__WFILE__,__LINE__);
        }

        try
        {
            const MemberIndex numMembers = Members::GetNumberOfMembers(into->GetTypeId());
            for (MemberIndex member = 0; member < numMembers; ++member)
            {
                const ArrayIndex arraySize = Members::GetArraySize(into->GetTypeId(),member);
                for (ArrayIndex index = 0; index < arraySize; ++index)
                {
                    const ContainerBase & fromContainerB = from->GetMember(member,index);
                    ContainerBase & intoContainerB = into->GetMember(member,index);

                    //is it an object member?
                    if (dynamic_cast<const ObjectContainerBase*>(&fromContainerB) != NULL)
                    {
                        const ObjectContainerBase & fromContainerOB = static_cast<const ObjectContainerBase&>(fromContainerB);
                        ObjectContainerBase & intoContainerOB = dynamic_cast<ObjectContainerBase&>(intoContainerB);

                        if (fromContainerOB.IsChangedHere()) //this specific member has changed
                        {
                            if (fromContainerOB.IsNull())
                            {
                                intoContainerOB.SetNull();
                            }
                            else
                            {
                                intoContainerOB.SetPtr(fromContainerOB.GetObjectPointer()->Clone());
                                intoContainerOB.SetChangedHere(true);
                            }
                        }
                        else if (fromContainerOB.IsChanged()) //some child has changed we need to recurse
                        {
                            //unless the type has changed or the into-member is null
                            if (intoContainerOB.IsNull() || intoContainerOB.GetObjectPointer()->GetTypeId() != fromContainerOB.GetObjectPointer()->GetTypeId())
                            {
                                //If the type is changing we write a warning
                                if (!intoContainerOB.IsNull())
                                {
                                    SEND_SYSTEM_LOG(Warning, << "The type of a member has changed without the change flag being set in 'from'.")
                                }

                                //if it was null we don't warn (even if it is a little bit suspicious to do that...)

                                intoContainerOB.SetPtr(fromContainerOB.GetObjectPointer()->Clone());
                                intoContainerOB.SetChangedHere(true);

                            }
                            else
                            {
                                //recurse
                                MergeChanges(intoContainerOB.GetObjectPointer(), fromContainerOB.GetObjectPointer());
                            }
                        }
                    }
                    else //no, normal member
                    {
                        assert(typeid(fromContainerB) == typeid(intoContainerB));
                        if (fromContainerB.IsChanged())
                        {
                            intoContainerB.Copy(fromContainerB);
                        }
                    }
                }
            }
        }
        catch (const std::bad_cast & e)
        {
            lllout << "MergeChanges failed!!!" <<std::endl;
            lllout << e.what() <<std::endl;

            throw SoftwareViolationException(L"Cast failed inside MergeChanges",__WFILE__,__LINE__);
        }
    }

    const std::string BinaryToBase64(char const * const binarySource, int sourceSize)
    {
        DotsC_Int32 resultSize=0;
        DotsC_Int32 size = DotsC_CalculateBase64BufferSize(sourceSize);
        std::vector<char> buf(size+1); //one extra for '0'
        DotsC_BinaryToBase64(&buf[0], size, binarySource, sourceSize, resultSize);
        buf.back() = '\0';
        return std::string(buf.begin(),buf.end());
    }

    const std::string BinaryToBase64(const Dob::Typesystem::Binary & bin)
    {
        return BinaryToBase64(&bin[0], static_cast<int>(bin.size()));
    }

    void Base64ToBinary(const std::string& base64, Dob::Typesystem::Binary & binary)
    {
        DotsC_Int32 resultSize=0;
        const DotsC_Int32 size = DotsC_CalculateBinaryBufferSize(static_cast<int>(base64.length()));
        binary.resize(size);
        if (size != 0)
        {
            DotsC_Base64ToBinary(&binary[0], size, base64.c_str(), static_cast<int>(base64.length()), resultSize);
        }
    }

}  //namespace Utilities


    //ToString for class EntityId, declared in EntityId.h
    const std::wstring
    EntityId::ToString() const
    {
        std::wostringstream out;

        out << "(";
        if (Operations::Exists(GetTypeId()))
        {
            out << Operations::GetName(GetTypeId());
        }
        else
        {
            out << "Unknown type: "
                << GetTypeId();
        }
        out << ", " << GetInstanceId() << ")";
        return out.str();
    }

    //ToStringNumeric for class EntityId, declared in EntityId.h
    const std::wstring
    EntityId::ToStringNumeric() const
    {
        std::wostringstream out;
        out << "("
            << m_typeId
            << ", "
            << m_instanceId.GetRawValue()
            << ")";
        return out.str();
    }

    //ToString for class InstanceId, declared in InstanceId.h
    const std::wstring
    InstanceId::ToString() const
    {
        if (m_instanceIdStr.empty())
        {
            return boost::lexical_cast<std::wstring>(m_instanceId);
        }
        else
        {
            return m_instanceIdStr;
        }
    }

    const ChannelId ChannelId::ALL_CHANNELS = ChannelId(Internal::ALL_CHANNELS_ID);

    //ToString for class ChannelId, declared in ChannelId.h
    const std::wstring
    ChannelId::ToString() const
    {
        if (!m_channelIdStr.empty())
        {
            return m_channelIdStr;
        }
        else if (m_channelId == Internal::DEFAULT_CHANNEL_ID)
        {
            return L"DEFAULT_CHANNEL";
        }
        else if (m_channelId == Internal::ALL_CHANNELS_ID)
        {
            return L"ALL_CHANNELS";
        }
        else
        {
            return boost::lexical_cast<std::wstring>(m_channelId);
        }
    }

    const HandlerId HandlerId::ALL_HANDLERS = HandlerId(Internal::ALL_HANDLERS_ID);

    //ToString for class HandlerId, declared in HandlerId.h
    const std::wstring
    HandlerId::ToString() const
    {
        if (!m_handlerIdStr.empty())
        {
            return m_handlerIdStr;
        }
        else if (m_handlerId == Internal::DEFAULT_HANDLER_ID)
        {
            return L"DEFAULT_HANDLER";
        }
        else if (m_handlerId == Internal::ALL_HANDLERS_ID)
        {
            return L"ALL_HANDLERS";
        }
        else
        {
            return boost::lexical_cast<std::wstring>(m_handlerId);
        }
    }

} //Typesystem
} //Dob
} //Safir
