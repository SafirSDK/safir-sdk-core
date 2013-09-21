/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://www.safirsdk.com)
* 
* Created by: Joel Ottosson / stjoot
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

#ifndef _dots_internal_defs_h
#define _dots_internal_defs_h

#include <sstream>

#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/CrashReporter.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/UnorderedMap.h>
#include <Safir/Dob/Typesystem/Internal/KernelDefs.h>
#include <iostream>
#include <vector>
#include <cstring>
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
namespace Internal
{
    //Re-typedef all the DotsC_-types into this namespace
    typedef DotsC_Int32 Int32;
    typedef DotsC_Int64 Int64;
    typedef DotsC_Float32 Float32;
    typedef DotsC_Float64 Float64;
    typedef DotsC_TypeId TypeId;
    //typedef DotsC_InstanceId InstanceId;
    typedef DotsC_MemberIndex MemberIndex;
    typedef DotsC_ParameterIndex ParameterIndex;
    typedef DotsC_ArrayIndex ArrayIndex;
    typedef DotsC_EnumerationValue EnumerationValue;
    typedef DotsC_MemberType MemberType;

    //more typedefs
    typedef Int64 ChannelId;
    typedef Int64 HandlerId;

    //Files extensions
    const char * const DOU_FILE_EXTENSION                    = ".dou";
    const char * const DOM_FILE_EXTENSION                    = ".dom";

    //Preset class names
    const char * const NULL_CLASS           = "Null_Class";
    const char * const OBJECT_CLASS         = "Object";

    //--------------------------------------------------
    // Unsigned types
    //--------------------------------------------------
    typedef boost::uint32_t UInt32;
    typedef boost::uint64_t UInt64;


    //types
    typedef UInt32 Offset;
    typedef UInt32 Size;
    typedef Int32 EnumInternal;

    //----------------------------------------------------
    //string compare
    //----------------------------------------------------
    static inline bool eq(const char * const a, const char * const b)
    {
        return (strcmp(a,b)==0);
    }


    static inline bool IsInt(const char* s, bool usgn=false)
    {
        //TODO: replace with regex
        size_t i= ((s[0]=='-' && !usgn) ? 1 : 0);
        if (strlen(s)<=i)
            return false;
        for (; i<strlen(s); ++i){
            if (s[i]<'0' || s[i]>'9') return false;
        } return true;
    }

    static inline bool IsFloat(const char* s)
    {
        //TODO: replace with regex
        size_t i= (s[0]=='-' ? 1 : 0);
        if (strlen(s)<=i)
            return false;
        int c=0;
        for (; i<strlen(s); ++i){
            if (s[i]=='.') c++;
            else if (s[i]<'0' || s[i]>'9') return false;
        } return c<=1;
    }

    //-------------------------------------------------------------------------
    //Helper class for conversion between internal and external status formats
    //-------------------------------------------------------------------------
    typedef char InternalMemberStatus;
    class MemberStatusHandler
    {
    private:
        MemberStatusHandler(); //declared but not defined, since this class is not possible to instantiate
        static const InternalMemberStatus NULL_FLAG_MASK = 0x1;
        static const InternalMemberStatus CHANGE_FLAG_MASK = 0x2;
        static const InternalMemberStatus SEMIDYNAMIC_FLAG_MASK = 0x4; //means that a HashedId member or EntityId has a string.

    public:
        static const InternalMemberStatus STARTING_VALUE = NULL_FLAG_MASK;

        static void ToExternalFormat(const InternalMemberStatus internalFormat, bool& isNull, bool& isChanged)
        {
            isNull = (internalFormat & NULL_FLAG_MASK) != 0;
            isChanged = (internalFormat & CHANGE_FLAG_MASK) != 0;
        }

        static InternalMemberStatus ToInternalFormat(const bool isNull, const bool isChanged)
        {
            return (isNull?NULL_FLAG_MASK:0) | (isChanged?CHANGE_FLAG_MASK:0);
        }

        static InternalMemberStatus ToInternalFormat(const bool isNull, const bool isChanged, const bool hasDynamicPart)
        {
            return (isNull?NULL_FLAG_MASK:0) | (isChanged?CHANGE_FLAG_MASK:0) | (hasDynamicPart?SEMIDYNAMIC_FLAG_MASK:0);
        }


        static bool HasChanged(const InternalMemberStatus internalFormat)
        {
            return (internalFormat & CHANGE_FLAG_MASK) != 0;
        }

        static bool IsNull(const InternalMemberStatus internalFormat)
        {
            return (internalFormat & NULL_FLAG_MASK) != 0;
        }

        static bool ChangedOrNotNull(const InternalMemberStatus internalFormat)
        {
            return HasChanged(internalFormat) || !IsNull(internalFormat);
        }

        static void SetChanged(InternalMemberStatus & internalFormat, const bool changed)
        {
            if (changed)
            {
                internalFormat |= CHANGE_FLAG_MASK;
            }
            else
            {
                internalFormat &= (0xff ^ CHANGE_FLAG_MASK);
            }
        }

        static bool HasDynamicPart(const InternalMemberStatus internalFormat)
        {
            return (internalFormat & SEMIDYNAMIC_FLAG_MASK) != 0;
        }
    };


    inline bool operator ==(const DotsC_EntityId& left, const DotsC_EntityId& right)
    {
        return left.typeId==right.typeId && left.instanceId==right.instanceId;
    }

    inline bool operator !=(const DotsC_EntityId& left, const DotsC_EntityId& right)
    {
        return !(left==right);
    }


    class InternalException :
        public std::exception
    {
    public:
        InternalException(const std::string & description,
                          const std::string & filename,
                          const long lineNumber)
        {
            std::ostringstream msgWriter;
            msgWriter << "Description: " << description  << std::endl
                  << "Occurred at: " << filename << ": " << lineNumber;
            m_msg = msgWriter.str();
        }

        virtual ~InternalException() throw()
        {

        }

        virtual const char * what() const throw()
        {
            try
            {
                return m_msg.c_str();
            }
            catch (...)
            {
                return "Exception while creating exception message!!!";
            }

        }

    private:
        std::string m_msg;
    };

#define ENSURE(expr, comment) \
    if (Safir::Dob::Typesystem::Internal::EnsureHelper(expr)); else {std::ostringstream ostr; ostr comment; Safir::Dob::Typesystem::Internal::EnsureFailed(ostr.str());}

    static inline void EnsureFailed (const std::string & str)
    {
        std::wostringstream ostr;
        ostr << "ENSURE Failed: " << str.c_str();
        Safir::Utilities::Internal::SystemLog().Send(Safir::Utilities::Internal::SystemLog::Critical,
                                                     ostr.str());
        
        const bool success = Safir::Utilities::CrashReporter::Dump();
        
        if (!success)
        {
            Safir::Utilities::Internal::SystemLog().Send(Safir::Utilities::Internal::SystemLog::Critical,
                                                         L"ENSURE failed to generate a dump! It looks like CrashReporter is not started.");
            
        }

        throw InternalException(str, __FILE__,__LINE__);
    }

    static inline bool EnsureHelper(const bool expr)
    {
        return expr;
    }

    //hack exception to be able to internally signal parsing failures.
    class DeserializationFailure
    {

    };


    /** Tag to indicate that the memory is to be created and initialized */
    struct create_and_initialize_t {};

    /**Tag to indicate that the memory is to be opened only (it has been initialized by someone else) */
    struct open_only_t {};

    //values for the above tags.
    const create_and_initialize_t create_and_initialize = create_and_initialize_t();
    const open_only_t open_only = open_only_t();

}
}
}
}
#endif
