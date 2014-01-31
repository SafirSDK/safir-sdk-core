/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

#ifndef __DOTS_INSTANCE_ID_H__
#define __DOTS_INSTANCE_ID_H__

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
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
     * Class containing the identity of an instance.
     *
     * Note: This class should be header-only, since otherwise we introduce all sorts of warnings for
     *       libraries that use this type.
     */
    class InstanceId
    {
    public:
        /**
         * Returns a random instance id.
         */
        static InstanceId GenerateRandom() {return InstanceId(Internal::GenerateRandom64Bit());}

        /**
         * Default constructor.
         *
         * Creates an unspecified instance id.
         */
        InstanceId(): m_instanceId(-1) {}

        /**
         * Constructor.
         *
         * Creates a instance id from the given string.
         *
         * @param id [in] - String identifying the instance.
         */
        InstanceId(const std::wstring& id) :
            m_instanceId(Internal::Generate64BitHash(id)),
            m_instanceIdStr(id)
        {}

        /**
         * Constructor.
         *
         * Creates an instance id using a 64 bit integer.
         *
         * @param id [in] - The 64bit integer id of the instance.
         */
        explicit InstanceId(const Dob::Typesystem::Int64 id):
            m_instanceId(id) {}


        /**
         * Constructor.
         *
         * Creates a instance id from the given data.
         *
         * @param id [in] - Identifier identifying the instance.
         * @param idStr [in] - String identifying the instance.
         */
        InstanceId(const Int64 id, const std::wstring & idStr):
            m_instanceId(id),
            m_instanceIdStr(idStr)
        {
#ifndef NDEBUG
            if (!m_instanceIdStr.empty() && m_instanceId != Internal::Generate64BitHash(idStr))
            {
                std::wostringstream ostr;
                ostr << "InstanceId two-argument constructor got an inconsistent id. Got ("
                    << id << ", '" << idStr << "'), but the string evaluates to " << Internal::Generate64BitHash(idStr) << ".";
                throw SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
            }
#endif
        }

        /**
         * Remove the included string from the instance id.
         *
         * This is meant to be used when this type is used as a member of a Dob object.
         * Using this call before the object gets serialized to binary or xml (i.e.
         * also before sending it anywhere) means that the string will not be included
         * when the object is sent.
         */
        void RemoveString() {m_instanceIdStr.clear(); m_CachedUtf8String.clear();}

        /**
         * Equality operator.
         *
         * @param other [in] - The InstanceId to compare with.
         */
        bool operator ==(const InstanceId& other) const
        {
            return m_instanceId == other.m_instanceId;
        }

        /**
         * Inequality operator.
         *
         * @param other [in] - The InstanceId to compare with.
         */
        bool operator !=(const InstanceId & other) const
        {
            return !(*this==other);
        }

        /**
         * Less-than operator.
         * This is provided to allow InstanceIds to be stored in STL containers that need strict weak ordering.
         *
         * @param other [in] - The InstanceId to compare with.
         */
        bool operator < (const InstanceId & other) const
        {
            return m_instanceId < other.m_instanceId;
        }

        /**
         * Return a string representation of the instance id.
         * If the string that created the instance id is available this is the string that will be returned,
         * otherwise it is the number that will be returned.
         *
         * The purpose of this function is for debug output and such.
         * The resulting string can *not* reliably be used in the "string constructor" for InstanceId to
         * recreate the same InstanceId.
         *
         * @return The instance id as a string.
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
        UnderlyingType GetRawValue() const {return m_instanceId;}

        /**
         * Get the string that was used to create this id.
         *
         * If no string was used this method returns an empty string.
         *
         * @return The string (if any) that was used to create this id.
         */
        const std::wstring & GetRawString() const {return m_instanceIdStr;}

        /**
         * Get the length of the string when converted to UTF-8 encoding.
         * Includes one byte for a null termination.
         *
         * @return The length of the string of the id when converted to UTF-8
         */
        Int32 Utf8StringLength() const
        {
            if (m_instanceIdStr.empty())
            {
                return 0;
            }

            if (m_CachedUtf8String.empty())
            {
                m_CachedUtf8String = Utilities::ToUtf8(m_instanceIdStr);
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
            if (!m_instanceIdStr.empty() && m_CachedUtf8String.empty())
            {
                m_CachedUtf8String = Utilities::ToUtf8(m_instanceIdStr);
            }
            return m_CachedUtf8String;
        }

        /** @} */

    private:
        UnderlyingType m_instanceId;
        std::wstring   m_instanceIdStr;

        mutable std::string m_CachedUtf8String;
    };

    static inline std::wostream & operator << (std::wostream& out, const InstanceId& instanceId)
    {return out << instanceId.ToString();}
}
}
}
#endif

