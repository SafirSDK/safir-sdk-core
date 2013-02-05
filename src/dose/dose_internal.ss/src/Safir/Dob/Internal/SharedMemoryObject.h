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

#ifndef __DOSE_SHARED_MEMORY_OBJECT_H__
#define __DOSE_SHARED_MEMORY_OBJECT_H__

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Utilities/StartupSynchronizer.h>
#include <boost/thread/mutex.hpp>



#ifdef _MSC_VER

#pragma warning (push)
#pragma warning (disable:4100)
#pragma warning (disable:4189)
#endif

void intrusive_ptr_add_ref(const char * p);
void intrusive_ptr_release(const char * p);


#include <boost/interprocess/containers/map.hpp>
#include <boost/interprocess/containers/set.hpp>
#include <boost/interprocess/containers/string.hpp>
#include <boost/interprocess/containers/vector.hpp>
#include <boost/interprocess/containers/list.hpp>
#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/interprocess/smart_ptr/shared_ptr.hpp>
#include <boost/interprocess/smart_ptr/enable_shared_from_this.hpp>
#include <boost/interprocess/smart_ptr/intrusive_ptr.hpp>
#include <boost/interprocess/smart_ptr/weak_ptr.hpp>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/thread/once.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{

    /**
     * Base class for all objects that reside in shared memory.
     *
     * Note that this class must NOT have any virtual members
     * or anything else that makes it not work in shared memory.
     */
    class DOSE_INTERNAL_API SharedMemoryObject
    {
    private:
        /**
         * This is the singleton that holds the shared memory for all applications communicating via DOB.
         * Since this singleton in a protected type in SharedMemoryObject it is only SharedMemoryObjects
         * that can access it.
         * Note that the singleton does not reside in shared memory.
         */
        class DOSE_INTERNAL_API SharedMemoryHolder:
            private boost::noncopyable,
            public Safir::Utilities::Synchronized

        {
        public:
            static SharedMemoryHolder & Instance();

            boost::interprocess::managed_shared_memory & GetShmem() {return *m_shmem;}
        private:
            SharedMemoryHolder();
            ~SharedMemoryHolder();

            //Synchronized stuff
            virtual void Create();
            virtual void Use();
            virtual void Destroy();

            boost::shared_ptr<boost::interprocess::managed_shared_memory> m_shmem;

            /**
             * This class is here to ensure that only the Instance method can get at the 
             * instance, so as to be sure that boost call_once is used correctly.
             * Also makes it easier to grep for singletons in the code, if all 
             * singletons use the same construction and helper-name.
             */
            struct SingletonHelper
            {
            private:
                friend SharedMemoryHolder& SharedMemoryHolder::Instance();
                
                static SharedMemoryHolder& Instance();
                static boost::once_flag m_onceFlag;
            };

            Safir::Utilities::StartupSynchronizer m_startupSynchronizer;
        };

    public:
        //No public bits, apparently...

    protected:
        /** Default constructor is protected so that this class is only possible to instantiate
         * as a parent to an object that resides in shared memory. Note again that this class may
         * have no virtual members (i.e. no pure virtuals either!).
         */
        SharedMemoryObject() {}

    public:
        template <class T>
        class my_allocator
        {
        private:
            typedef boost::interprocess::allocator<T,boost::interprocess::managed_shared_memory::segment_manager> underlying_allocator;

        public:
            typedef typename underlying_allocator::pointer     pointer;
            typedef typename underlying_allocator::value_type  value_type;
            typedef typename underlying_allocator::size_type   size_type;
            typedef typename underlying_allocator::const_pointer               const_pointer;
            typedef typename underlying_allocator::reference                   reference;
            typedef typename underlying_allocator::const_reference             const_reference;
            typedef typename underlying_allocator::difference_type             difference_type;

            template<class T2>
            struct rebind
            {
                typedef my_allocator<T2>     other;
            };

            pointer allocate(size_type count /*, cvoid_ptr hint = 0*/) {return m_allocator.allocate(count);}
            void deallocate(const pointer & ptr, size_type dummy){m_allocator.deallocate(ptr,dummy);}

            size_type max_size() const {return m_allocator.max_size();}

            my_allocator(): m_allocator(SharedMemoryHolder::Instance().GetShmem().get_segment_manager()) {}
            my_allocator(const my_allocator & other):m_allocator(other.m_allocator) {}

            template <class T2>
            my_allocator(const my_allocator<T2> & other):m_allocator(other.m_allocator){}

            bool operator==(const my_allocator & other) const {return m_allocator == other.m_allocator;}
            bool operator!=(const my_allocator & other) const {return !(*this == other);}


            //All instances are identical....
            my_allocator& operator=(const my_allocator&) {return *this;}
        private:
            //other types of myself must be allowed to fiddle with privates...
            //needed for construct from my_allocator<T2> above
            template <class T2>
            friend class my_allocator;

            //Not assignable from related allocator
            template<class T2>
            my_allocator& operator=(const my_allocator<T2>&);


            underlying_allocator m_allocator;
        };

        template <class T>
        class my_deleter
        {
        private:
            typedef boost::interprocess::deleter<T,boost::interprocess::managed_shared_memory::segment_manager> underlying_deleter;
        public:
            typedef typename underlying_deleter::pointer pointer;

            my_deleter():m_deleter(SharedMemoryHolder::Instance().GetShmem().get_segment_manager()) {}

            void operator()(const pointer &p){m_deleter(p);}

        private:
            underlying_deleter m_deleter;
        };

        template<class T>
        struct Utilities
        {
            typedef typename boost::interprocess::enable_shared_from_this
            <
                T,
                my_allocator<T>,
                my_deleter<T>
            >
            enable_shared_from_this;
        };

        //
        // Typedefs
        //
        template <class T, class Pred = std::less<T> >
        struct Containers
        {
            typedef typename boost::interprocess::vector
            <
                T,
                my_allocator<T>
            >
            vector;

            typedef typename boost::interprocess::list
            <
                T,
                my_allocator<T>
            >
            list;


            typedef typename boost::interprocess::set
            <
                T,
                std::less<T>,
                my_allocator<T>
            >
            set;

            typedef typename boost::interprocess::multiset
            <
                T,
                std::less<T>,
                my_allocator<T>
            >
            multiset;


            typedef typename boost::interprocess::basic_string
            <
                T,
                std::char_traits<T>,
                my_allocator<T>
            >
            string;

        };


        template <class T, class U, class Pred = std::less<T> >
        struct PairContainers
        {
            typedef typename std::pair<const T,U> value_type;

            //TODO: remove map and add unordered_map!!!
            typedef typename boost::interprocess::map
            <
                T,
                U,
                Pred,
                my_allocator<value_type>
            >
            map;

        };


        template <class T>
        struct SmartPointers
        {
            typedef typename boost::interprocess::shared_ptr
            <
                T,
                my_allocator<T>,
                my_deleter<T>
            >
            shared_ptr;

            typedef typename boost::interprocess::weak_ptr
            <
                T,
                my_allocator<T>,
                my_deleter<T>
            >
            weak_ptr;

            typedef typename boost::interprocess::intrusive_ptr
            <
                T,
                boost::interprocess::offset_ptr<void>
            >
            intrusive_ptr;
        };



        typedef Containers<char>::string ShmString;
        typedef Containers<wchar_t>::string ShmWString;


        //
        // Functions
        //

        /**
         * Get an allocator for objects of type T.
         */
        template <class T>
        static const boost::interprocess::allocator<T,boost::interprocess::managed_shared_memory::segment_manager>
        GetAllocator()
        {
            return boost::interprocess::allocator<T,boost::interprocess::managed_shared_memory::segment_manager>(GetSharedMemory().get_segment_manager());
        }

        /**
         * Get hold of the shared memory.
         */
        static boost::interprocess::managed_shared_memory & GetSharedMemory() {return SharedMemoryHolder::Instance().GetShmem();}
    private:

    };


}
}
}



#endif

