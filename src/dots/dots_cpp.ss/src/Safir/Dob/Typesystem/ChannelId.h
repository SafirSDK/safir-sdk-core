/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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

#ifndef __DOTS_CHANNEL_ID_H__
#define __DOTS_CHANNEL_ID_H__

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/Internal/KernelDefs.h>
#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <string>
#include <sstream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    /**
     * Class containing the identity of a channel.
     */
    class ChannelId
    {
    public:
        /** Constant representing all channels. */
        static DOTS_API const ChannelId ALL_CHANNELS;

        /**
         * Default constructor.
         *
         * Creates a default channel id.
         */
        ChannelId():
            m_channelId(Internal::DEFAULT_CHANNEL_ID),
            m_channelIdStr(L"DEFAULT_CHANNEL")
        {}

        /**
         * Constructor.
         *
         * Creates a channel id from the given string.
         *
         * @param id [in] - String identifying the channel.
         */
        ChannelId(const std::wstring& id):
            m_channelId(Internal::Generate64BitHash(id)),
            m_channelIdStr(id)
        {}


        /**
         * Constructor.
         *
         * Creates a channel id from the given id.
         *
         * @param id [in] - Identifier identifying the channel.
         */
        explicit ChannelId(const Int64 id):
            m_channelId(id)
        {}


        /**
         * Constructor.
         *
         * Creates a channel id from the given data.
         *
         * @param id [in] - Identifier identifying the channel.
         * @param idStr [in] - String identifying the channel.
         */
        ChannelId(const Int64 id, const std::wstring & idStr):
            m_channelId(id),
            m_channelIdStr(idStr)
        {
#ifndef NDEBUG
            if (!m_channelIdStr.empty() && m_channelId != Internal::Generate64BitHash(idStr))
            {
                std::wostringstream ostr;
                ostr << "ChannelId two-argument constructor got an inconsistent id. Got ("
                    << id << ", '" << idStr << "'), but the string evaluates to " << Internal::Generate64BitHash(idStr) << ".";
                throw SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
            }
#endif
        }


        /**
         * Remove the included string from the channel id.
         *
         * This is meant to be used when this type is used as a member of a Dob object.
         * Using this call before the object gets serialized to binary or xml (i.e.
         * also before sending it anywhere) means that the string will not be included
         * when the object is sent.
         */
        void RemoveString() {m_channelIdStr.clear(); m_CachedUtf8String.clear();}

        /**
         * Equality operator.
         *
         * @param other [in] - The ChannelId to compare with.
         */
        bool operator ==(const ChannelId & other) const
        {
            return m_channelId == other.m_channelId;
        }

        /**
         * Inequality operator.
         *
         * @param other [in] - The ChannelId to compare with.
         */
        bool operator !=(const ChannelId & other) const
        {
            return !(*this==other);
        }

        /**
         * Less-than operator.
         * This is provided to allow ChannelIds to be stored in STL containers that need strict weak ordering.
         *
         * @param other [in] - The ChannelId to compare with.
         */
        bool operator < (const ChannelId & other) const
        {
            return m_channelId < other.m_channelId;
        }

        /**
         * Return a string representation of the channel id.
         */
        DOTS_API const std::wstring ToString() const;


        /** @{ */
        /** @name Internal functions. */

        typedef Int64 UnderlyingType;

        /**
         * Get the raw 64 bit integer identifier.
         *
         * @return The raw 64 bit identifier.
         */
        UnderlyingType GetRawValue() const {return m_channelId;}

        /**
         * Get the string that was used to create this id.
         * 
         * If no string was used this method returns an empty string.
         * 
         * @return The string (if any) that was used to create this id.
         */
        const std::wstring & GetRawString() const {return m_channelIdStr;}

        /**
         * Get the length of the string when converted to UTF-8 encoding.
         * Includes one byte for a null termination.
         * 
         * @return The length of the string of the id when converted to UTF-8
         */
        Int32 Utf8StringLength() const
        {
            if (m_channelIdStr.empty())
            {
                return 0;
            }

            if (m_CachedUtf8String.empty())
            {
                m_CachedUtf8String = Utilities::ToUtf8(m_channelIdStr);
            }

            return static_cast<Int32>(m_CachedUtf8String.length() + 1);
        }

        /**
         * Convert the string to UTF-8.
         * 
         * Returns an empty string if there is no string.
         * 
         * @return UTF-8 representation of the string.
         */
        const std::string & Utf8String() const
        {
            if (!m_channelIdStr.empty() && m_CachedUtf8String.empty())
            {
                m_CachedUtf8String = Utilities::ToUtf8(m_channelIdStr);
            }
            return m_CachedUtf8String;
        }

        /** @} */

    private:
        UnderlyingType m_channelId;
        std::wstring   m_channelIdStr;

        mutable std::string m_CachedUtf8String;
    };

    static inline std::wostream & operator << (std::wostream& out, const ChannelId& channelId)
    {return out << channelId.ToString();}
}
}
}
#endif

