/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://safir.sourceforge.net)
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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/

#ifndef __DOTS_INTERNAL_DEFS__
#define __DOTS_INTERNAL_DEFS__

#include <sstream>
#include <iostream>
#include <vector>
#include <Safir/Dob/Typesystem/LanguageInterfaceDefs.h>

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

#ifndef BOOST_SPIRIT_THREADSAFE
  #error "Please define BOOST_SPIRIT_THREADSAFE in your project settings!"
#endif

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
namespace Internal
{
    typedef std::vector<std::string> StringVector;

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
    BOOST_STATIC_ASSERT(sizeof(Offset) <= sizeof(DotsC_Float64)); //this is to ensure that an offset can fit into a hashedId member.

    inline bool operator ==(const DotsC_EntityId& left, const DotsC_EntityId& right)
    {
        return left.typeId==right.typeId && left.instanceId==right.instanceId;
    }

    inline bool operator !=(const DotsC_EntityId& left, const DotsC_EntityId& right)
    {
        return !(left==right);
    }
}
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport::Internal

#endif
