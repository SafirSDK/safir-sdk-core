/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#ifndef __DOTS_LIBRARY_EXCEPTIONS_H__
#define __DOTS_LIBRARY_EXCEPTIONS_H__

#include <boost/noncopyable.hpp>
#include <boost/unordered_map.hpp>
#include <boost/thread/once.hpp>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/Exceptions.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251) // warning C4251: 'Safir::Dob::Typesystem::LibraryExceptions::m_CallbackMap' : class 'stdext::hash_map<_Kty,_Ty>' needs to have dll-interface to be used by clients of class 'Safir::Dob::Typesystem::LibraryExceptions'
#pragma warning (disable: 4275) // warning C4275: non dll-interface class 'boost::noncopyable_::noncopyable' used as base for dll-interface class 'Safir::Dob::Typesystem::LibraryExceptions'
#endif

#define CATCH_LIBRARY_EXCEPTIONS_AND_RUN(statement) \
    catch (const Safir::Dob::Typesystem::FundamentalException & exc) \
    {Safir::Dob::Typesystem::LibraryExceptions::Instance().Set(exc); statement;} \
    catch (const Safir::Dob::Typesystem::Exception & exc) \
    {Safir::Dob::Typesystem::LibraryExceptions::Instance().Set(exc); statement;} \
    catch (const std::exception & exc) \
    {Safir::Dob::Typesystem::LibraryExceptions::Instance().Set(exc); statement;} \
    catch (...) \
    {Safir::Dob::Typesystem::LibraryExceptions::Instance().SetUnknown(); statement;}

#define CATCH_LIBRARY_EXCEPTIONS \
    catch (const Safir::Dob::Typesystem::FundamentalException & exc) \
    {Safir::Dob::Typesystem::LibraryExceptions::Instance().Set(exc);} \
    catch (const Safir::Dob::Typesystem::Exception & exc) \
    {Safir::Dob::Typesystem::LibraryExceptions::Instance().Set(exc);} \
    catch (const std::exception & exc) \
    {Safir::Dob::Typesystem::LibraryExceptions::Instance().Set(exc);} \
    catch (...) \
    {Safir::Dob::Typesystem::LibraryExceptions::Instance().SetUnknown();}


namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    /**
     * Contains methods used when passing exceptions across language boundaries.
     */
    class DOTS_CPP_API LibraryExceptions:
        private boost::noncopyable
    {
    public:
        /**
         * Get the instance of the singleton.
         *
         * @return The instance of the singleton.
         */
        static LibraryExceptions & Instance();

        /**
         * Set the current exception.
         *
         * When you have caught an exception that you would like to pass across language
         * boundaries, call this method with the exception.
         * Then, on the other side of the language boundary, call
         * the Throw method, which will throw the exception in the other language.
         *
         * The easiest way of doing this is to use the CATCH_LIBRARY_EXCEPTIONS macro above
         * using code like this:
         * {
         *     success = false;
         *     try
         *     {
         *         do your stuff here;
         *         success = true;
         *     }
         *     CATCH_LIBRARY_EXCEPTIONS;
         * }
         *
         * The success flag should be passed across the language boundary so that the
         * Throw method can be called when an exception has occurred.
         * The CATCH_LIBRARY_EXCEPTIONS catches all C++ exceptions and puts them
         * into the LibraryExceptions singleton.
         *
         * @param exception [in] - The exception.
         */
        void Set(const FundamentalException & exception);


        /**
         * Set the current exception.
         *
         * When you have caught an exception that you would like to pass across language
         * boundaries, call this method with the exception.
         * Then, on the other side of the language boundary, call
         * the Throw method, which will throw the exception in the other language.
         *
         * The easiest way of doing this is to use the CATCH_LIBRARY_EXCEPTIONS macro above
         * using code like this:
         * {
         *     success = false;
         *     try
         *     {
         *         do your stuff here;
         *         success = true;
         *     }
         *     CATCH_LIBRARY_EXCEPTIONS;
         * }
         *
         * The success flag should be passed across the language boundary so that the
         * Throw method can be called when an exception has occurred.
         * The CATCH_LIBRARY_EXCEPTIONS catches all C++ exceptions and puts them
         * into the LibraryExceptions singleton.
         *
         * @param exception [in] - The exception.
         */
        void Set(const Exception & exception);


        /**
         * Set the current exception.
         *
         * When you have caught an exception that you would like to pass across language
         * boundaries, call this method with the exception.
         * Then, on the other side of the language boundary, call
         * the Throw method, which will throw the exception in the other language.
         *
         * The easiest way of doing this is to use the CATCH_LIBRARY_EXCEPTIONS macro above
         * using code like this:
         * {
         *     success = false;
         *     try
         *     {
         *         do your stuff here;
         *         success = true;
         *     }
         *     CATCH_LIBRARY_EXCEPTIONS;
         * }
         *
         * The success flag should be passed across the language boundary so that the
         * Throw method can be called when an exception has occurred.
         * The CATCH_LIBRARY_EXCEPTIONS catches all C++ exceptions and puts them
         * into the LibraryExceptions singleton.
         *
         * @param exception [in] - The exception.
         */
        void Set(const std::exception & exception);


        /**
         * Set the current exception.
         *
         * This should be used when a ... exception is caught.
         *
         * The easiest way of doing this is to use the CATCH_LIBRARY_EXCEPTIONS macro above
         * using code like this:
         * {
         *     success = false;
         *     try
         *     {
         *         do your stuff here;
         *         success = true;
         *     }
         *     CATCH_LIBRARY_EXCEPTIONS;
         * }
         *
         * The success flag should be passed across the language boundary so that the
         * Throw method can be called when an exception has occurred.
         * The CATCH_LIBRARY_EXCEPTIONS catches all C++ exceptions and puts them
         * into the LibraryExceptions singleton.
         *
         */
        void SetUnknown();

        /**
         * Append some text to the description of the current exception.
         *
         * This can be used just inside a language boundary to add extra
         * information to the exception so that it is easier to debug the exception.
         */
        void AppendDescription(const std::wstring & moreDescription);

        /**
         * Throw the current exception.
         *
         * Call this to throw the current exception. It is considered a programming
         * error to call this function if no exception is set.
         */
        void Throw();

        /**
         * @name Registration part.
         * Stuff for registering exceptions with the exception factory.
         */
        /** @{ */

        /**
         * Function signature of the throw exception callback function.
         * This is the signature of the function that the exception factory will call to throw
         * an exception of a type.
         */
        typedef void (*ThrowExceptionCallback)(const std::wstring & description);

        /**
         * Register an exception with the object factory.
         *
         * Only for use by the automatically generated code!
         *
         * @param exceptionId [in] - The TypeId of the exception that should be thrown using throwFunction.
         * @param throwFunction [in] - The function to call to throw the exception.
         */
        bool RegisterException(const TypeId exceptionId, ThrowExceptionCallback throwFunction)
        {return m_CallbackMap.insert(CallbackMap::value_type(exceptionId,throwFunction)).second;}

        /** @} */

        /**
         * Throw a specific exception.
         *
         * This function takes no notice of currently set exceptions. Rather
         * it throws the specified exception with the specified description string.
         *
         * @param exceptionId [in] - The TypeId of the exception to throw.
         * @param description [in] - String with more information about what happened.
         *                           This is expected to be ascii if exceptionId == 0,
         *                           and utf8 if exceptionId != 0.
         */
        void Throw(const TypeId exceptionId, const std::string& description) const;

    private:
        LibraryExceptions();
        ~LibraryExceptions();


        void Set(const Internal::CommonExceptionBase & exception);

        typedef boost::unordered_map<TypeId, ThrowExceptionCallback> CallbackMap;
        CallbackMap m_CallbackMap;

        /**
         * This class is here to ensure that only the Instance method can get at the 
         * instance, so as to be sure that boost call_once is used correctly.
         * Also makes it easier to grep for singletons in the code, if all 
         * singletons use the same construction and helper-name.
         */
        struct SingletonHelper
        {
        private:
            friend LibraryExceptions& LibraryExceptions::Instance();

            static LibraryExceptions& Instance();
            static boost::once_flag m_onceFlag;
        };
    };
}
}
}

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#endif


