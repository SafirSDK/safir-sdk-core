/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#ifndef __DOTS_ALLOCATION_HELPER_H__
#define __DOTS_ALLOCATION_HELPER_H__

#include <boost/noncopyable.hpp>
#include <boost/interprocess/containers/vector.hpp>
#include <boost/interprocess/containers/string.hpp>
#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/interprocess/containers/map.hpp>
#include <boost/interprocess/smart_ptr/shared_ptr.hpp>


namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{



    class AllocationHelper:
        private boost::noncopyable
    {
    public:

        template <class T>
        struct Containers
        {
            typedef typename boost::interprocess::allocator
            <
                T,
                boost::interprocess::managed_shared_memory::segment_manager
            >
            allocator;


            typedef typename boost::interprocess::vector
            <
                T,
                allocator
            >
            vector;

            typedef typename boost::interprocess::basic_string
            <
                T,
                std::char_traits<T>,
                allocator
            >
            string;
        };

        template <class T, class U>
        struct PairContainers
        {
            typedef typename std::pair<const T,U> value_type;

            typedef typename boost::interprocess::allocator
            <
                value_type,
                boost::interprocess::managed_shared_memory::segment_manager
            >
            allocator;

            //TODO: remove map and add unordered_map!!!
            typedef typename boost::interprocess::map
            <
                T,
                U,
                std::less<T>,
                allocator
            >
            map;

        };

        template <class T>
        struct SmartPointers
        {
            typedef typename boost::interprocess::managed_shared_ptr
            <
                T,
                boost::interprocess::managed_shared_memory
            >::type
            shared_ptr;
        };


        explicit AllocationHelper(boost::interprocess::managed_shared_memory * shmem):
            m_shmem(shmem) {}

        boost::interprocess::managed_shared_memory * GetShmem() {return m_shmem;}

        template <class T>
        inline const boost::interprocess::allocator<T,boost::interprocess::managed_shared_memory::segment_manager>
        GetAllocator()
        {
            return boost::interprocess::allocator<T,boost::interprocess::managed_shared_memory::segment_manager>(m_shmem->get_segment_manager());
        }
    private:
        boost::interprocess::managed_shared_memory * m_shmem;
    };

    //Some useful common definitions

    typedef AllocationHelper::Containers<char>::string ShmString;

    typedef boost::interprocess::offset_ptr<char> ParameterOffset;
    typedef boost::interprocess::offset_ptr<const char> ParameterOffsetConst;

    /** Cast from any offset pointer to any other.
     * Idea is first to cast to void ptr and then to target ptr.
     */
    template <class T>
    const boost::interprocess::offset_ptr<T>
    ParameterOffsetCast(const ParameterOffset & offset)
    {
        return boost::interprocess::static_pointer_cast<T>(boost::interprocess::static_pointer_cast<void>(offset));
    }

    template <class T>
    const boost::interprocess::offset_ptr<const T>
    ParameterOffsetCast(const ParameterOffsetConst & offset)
    {
        return ParameterOffsetCast<T>(boost::interprocess::const_pointer_cast<char>(offset));
    }


}
}
}
}

#endif
