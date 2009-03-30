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

#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Members.h>
#include <Safir/Dob/Typesystem/Object.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/ObjectContainer.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <iostream>
#include <boost/shared_array.hpp>
#include <Safir/Dob/Typesystem/ValueContainers.h>
#include <boost/lexical_cast.hpp>
#include <typeinfo>

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
        if (wstr.empty())
        {
            return std::string();
        }

        char *pszBuf = new char[wstr.length() * 4 + 1];
        char *psz;
        unsigned long pos;


        std::wstring::const_iterator it;
        for( it = wstr.begin(), psz = pszBuf; it != wstr.end(); ++it )
        {
            pos = *it;
            if (pos < 0x80)
            {
                *psz++ = (char) pos;
            }
            else if (pos < 0x800)
            {
                *psz++ = (char) (0xC0 + (pos >> 6));
                *psz++ = (char) (0x80 + (pos & 0x3F));
            }
            else if (pos < 0x10000)
            {
                *psz++ = (char) (0xE0 + (pos >> 12));
                *psz++ = (char) (0x80 + ((pos >> 6) & 0x3F));
                *psz++ = (char) (0x80 + (pos & 0x3F));
            }
            else
                *psz++ = '#'; // Only the BMP is supported.
        }

        std::string str( pszBuf, psz - pszBuf );

        delete [] pszBuf;
        return str;
    }

    const std::wstring ToWstring(const std::string & str)
    {
        if (str.empty())
        {
            return std::wstring();
        }

        int left = 0;
        wchar_t *pwszBuf = new wchar_t[str.length() + 1];
        wchar_t *pwsz;
        unsigned long pos;

        pwsz = pwszBuf;

        std::string::const_iterator it;
        for( it = str.begin(); it != str.end(); ++it)
        {
            pos = (unsigned char) *it;
            if ((left == 0) ^ ((pos & 0xC0) != 0x80)) // Continuation byte mismatch
            {
                left = 0;
                *pwsz++ = L'#';
            }

            if (pos < 0x80) // 7-bit ASCII
            {
                *pwsz++ = (wchar_t) pos;
            }
            else if ((pos & 0xC0) == (0x80)) // Correct continuation
            {
                left--;
                *pwsz = (*pwsz << 6) + (wchar_t) (pos & 0x3F);
                if (left == 0)
                    pwsz++;
            }
            else if ((pos & 0xE0) == (0xC0)) // First of 2
            {
                *pwsz = (wchar_t) (pos & 0x1F);
                left = 1;
            }
            else if ((pos & 0xF0) == (0xE0)) // First of 3
            {
                *pwsz = (wchar_t) (pos & 0x0F);
                left = 2;
            }
            else // Only the BMP is supported.
            {
                left = 0;
                *pwsz++ = L'#';
            }

        }

        std::wstring wstr( pwszBuf, pwsz - pwszBuf );

        delete [] pwszBuf;
        return wstr;
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
                                    std::cerr << "Warning (Contact a DOB developer if you do not understand it):" <<std::endl;
                                    std::cerr << "The type of a member has changed without the change flag being set in 'from'." << std::endl;

                                    lllout << "MergeChanges Warning: TypeId of member has changed without the change flag being set in 'from'!" <<std::endl;
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
        DotsC_Int32 size = DotsC_CalculateBinaryBufferSize(static_cast<int>(binary.size()));
        binary.resize(size);
        DotsC_Base64ToBinary(&binary[0], size, base64.c_str(), static_cast<int>(base64.length()), resultSize);
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
