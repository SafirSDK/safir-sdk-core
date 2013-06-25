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

#ifndef __DOTS_INTERNAL_DEFS__
#define __DOTS_INTERNAL_DEFS__

#include <sstream>

#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <iostream>

////Make a hash_map available even though their locations are different
////call it unordered_map, as it will be called in tr1
//#if defined _MSC_VER
//    #include <hash_map>
//    #define unordered_map stdext::hash_map
//#elif defined __GNUC__
//    #include <tr1/unordered_map>
//    using std::tr1::unordered_map;
//#else
//#error We need a definition of unordered_map
//#endif

#include <Safir/Dob/Typesystem/Internal/KernelDefs2.h>
#include <vector>

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
    typedef std::vector<std::string> StringVector;

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
    typedef DotsC_PropertyMappingKind MappingKind;

    typedef boost::uint32_t Offset;
    typedef boost::uint32_t Size;
    typedef DotsC_Int32 EnumInternal;
    typedef DotsC_Int64 ChannelId;
    typedef DotsC_Int64 HandlerId;

    static const Size OFFSET_SIZE                  =0;
    static const Size OFFSET_TYPE_ID               =4;
    static const Size OFFSET_HEADER_LENGTH         =12;
    static const Size MEMBER_STATUS_LENGTH         =sizeof(char); //1
    static const Size OFFSET_MEMBER_LENGTH         =  sizeof(Offset); //4
    static const Size DYNAMIC_MEMBER_SIZE          =  sizeof(Offset) + sizeof(Size); //8


    BOOST_STATIC_ASSERT(sizeof(char) == 1);
    BOOST_STATIC_ASSERT(sizeof(Offset) == 4);
    BOOST_STATIC_ASSERT(sizeof(Size) == 4);
    BOOST_STATIC_ASSERT(sizeof(Offset) <= sizeof(Int64)); //this is to ensure that an offset can fit into a hashedId member.

    /**
     * Definition of a value. Used in parameters, createRoutines and propertyMappings.
     */
    enum ValueDefinitionKind {ValueKind, NullKind, RefKind};
    struct ValueDefinition
    {
        ValueDefinitionKind kind;
        std::string stringVal;
        std::vector<char> binaryVal; //object and binary
        DotsC_Int64 hashedVal;
        union
        {
            DotsC_Int32 int32Val;
            DotsC_Int64 int64Val;
            DotsC_Float32 float32Val;
            DotsC_Float64 float64Val;
            bool boolVal;
        };

        ValueDefinition() : kind(ValueKind){}
        ValueDefinition(ValueDefinitionKind k) : kind(k){}
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
            m_msg=msgWriter.str();
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
        lllerr << "ENSURE failed: '"<< str.c_str() << "'" << std::endl;
        //Safir::Utilities::Internal::Internal::LowLevelLoggerBackend::Instance().FlushBuffer();
        std::wcout << "Please contact your nearest DOB developer!" << std::endl;

        throw InternalException(str, __FILE__,__LINE__);
    }

    static inline bool EnsureHelper(const bool expr)
    {
        return expr;
    }
}
}
}
}
#endif
