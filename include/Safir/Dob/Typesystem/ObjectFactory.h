/******************************************************************************
*
* Copyright Consoden AB, 2006-2013 (http://safir.sourceforge.net)
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

#ifndef __DOTS_OBJECT_FACTORY_H__
#define __DOTS_OBJECT_FACTORY_H__

#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/unordered_map.hpp>
#include <boost/thread/once.hpp>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/Exceptions.h>

#ifdef _MSC_VER
#pragma warning(push) 
#pragma warning(disable: 4275)
#endif

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    //forward declaration since we would rather not include dots_object.h (Object.cpp includes this file...)
    class Object;

    /** A smart pointer to an Object. */
    typedef boost::shared_ptr<Object> ObjectPtr;

    /**
     * This class is an object factory for all automatically generated DOB classes.
     * Each generated class automatically registers itself with this class.
     * Users can call the CreateObject(TypeId) routine to create objects of a desired type
     * (this is if they receive the type id from some other application so that they cannot
     * call the Create routine of the class itself directly).
     */
    class DOTS_CPP_API ObjectFactory
        : private boost::noncopyable
    {
    public:
        /**
         * Get the instance of the singleton.
         *
         * @return The instance of the singleton.
         */
        static ObjectFactory & Instance();

        /**
         * Create a new "empty" object from a typeid.
         *
         * This method takes a TypeId and calls the
         * appropriate callback to create an object of the desired type.
         *
         * @param typeId [in] - The TypeId of the object to create.
         * @return A boost::shared_ptr to the object.
         * @exception IllegalValueException If the type couldn't be found in the ObjectFactory.
        */
        ObjectPtr CreateObject(const TypeId typeId) const;

        /**
         * @name Registration part.
         * Stuff for registering classes with the object factory.
         */
        /** @{ */

        /**
         * Function signature of the create callback function.
         * This is the signature of the function that the object factory will call to create the object.
         * If handle == 0 then just create an empty object.
         */
        typedef ObjectPtr (*CreateObjectCallback)(Int64 handle);

        /**
         * Register a class with the object factory.
         *
         * @param typeId [in] - The TypeId of the object that should be created using createFunction.
         * @param createFunction [in] - The function to call to create the object.
         */
        bool RegisterClass(const TypeId typeId, CreateObjectCallback createFunction);

        /** @} */




        /**
         * @name Blob deserialization part.
         */

        /** @{ */

        /**
         * Create a new object from a blob.
         *
         * This method takes a blob and extracts the typeId from it and then calls the
         * appropriate callback to create the object.
         *
         * @param blob [in] - The blob to deserialize.
         * @return A boost::shared_ptr to the object.
         * @exception IllegalValueException If the type represented by the blob isn't found
         *                                   in the ObjectFactory.
        */
        ObjectPtr CreateObject(char const * const blob) const;

        /** @} */

    private:
        ObjectFactory();
        ~ObjectFactory();

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251) // warning C4251: 'Safir::Dob::Typesystem::ObjectFactory::m_CallbackMap' : class 'stdext::hash_map<_Kty,_Ty>' needs to have dll-interface to be used by clients of class 'Safir::Dob::Typesystem::ObjectFactory'
#endif

        typedef boost::unordered_map<TypeId,CreateObjectCallback> CallbackMap;
        CallbackMap m_CallbackMap;

#ifdef _MSC_VER
#pragma warning (pop)
#endif

        /**
         * This class is here to ensure that only the Instance method can get at the 
         * instance, so as to be sure that boost call_once is used correctly.
         * Also makes it easier to grep for singletons in the code, if all 
         * singletons use the same construction and helper-name.
         */
        struct SingletonHelper
        {
        private:
            friend ObjectFactory& ObjectFactory::Instance();

            static ObjectFactory& Instance();
            static boost::once_flag m_onceFlag;
        };
    };
}
}
}

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#endif

