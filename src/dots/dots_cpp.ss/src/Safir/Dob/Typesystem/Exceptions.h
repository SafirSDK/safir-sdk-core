/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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

#ifndef _dots_exception_h
#define _dots_exception_h

//Definition of __WFILE__ which is the wide-string equivalent of __FILE__
#define WIDEN2(x) L ## x
#define WIDEN(x) WIDEN2(x)
#define __WFILE__ WIDEN(__FILE__)

#include <string>
#include <Safir/Dob/Typesystem/Defs.h>
#include <boost/static_assert.hpp>
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    /**
     * This namespace contains stuff that is internal to the DOB typesystem.
     * If you ever have to use anything from here there is something wrong. Either with your
     * design or with the DOB...
     */
    namespace Internal
    {
        /**
         * Common functionality for the exception hierarchy of the DOB typesystem.
         *
         * This class contains the common parts of the FundamentalException and Exception
         * base classes.
         *
         * No exceptions except FundamentalException and Exception shall ever inherit from this class.
         * See Exception and FundamentalException for more information on how to define your exceptions.
         */
        class DOTS_API CommonExceptionBase:
            public std::exception
        {
        public:
            /**
             * Constructor with exception information.
             *
             * Creates an exception that contains information about why and where the exception
             * occurred.
             *
             * @param message [in] - A message describing why the exception occurred.
             * @param fileName [in] - The name of the file where the exception occurred.
             *                        Always use __WFILE__ as the argument for this parameter.
             * @param lineNumber [in] - The line number that the exception occurred on.
             *                          Always use __LINE__ as the argument for this parameter.
             */
            CommonExceptionBase(const std::wstring & message,
                                const std::wstring & fileName, //Use the __WFILE__ macro
                                const Dob::Typesystem::Int64 lineNumber); //Use the __LINE__ macro

            /**
             * Destroy the exception.
             */
            virtual ~CommonExceptionBase() throw();

            /**
             * Get a formatted string containing all the information in the exception.
             *
             * The string will look like this (yes, there's a line break in it):
             * "ExceptionName at some_file_name.cpp: 453
             *  Description: Some kind of explanation of the exception".
             *
             * @return A formatted exception information string.
             */
            const std::wstring GetExceptionInfo() const;

            /**
             * @name Selectors for exception information.
             * Probably you want to use the GetExceptionInfo method instead of these methods.
             * @see GetExceptionInfo
             */
            /** @{ */

            /**
             * Get the message about why the exception occurred.
             *
             * @return The exception message.
             */
            const std::wstring & GetMessage() const {return m_Message;}

            /**
             * Get the exception name.
             *
             * @return The name of the exception
             */
            virtual const std::wstring GetName() const = 0;

            /**
             * Get the name of the file where the exception occurred.
             *
             * @return The file where the exception occurred.
             */
            const std::wstring & GetFileName() const {return m_FileName;}

            /**
             *Get the line number that the exception occurred on.
             *
             * @return The line number where the exception occurred.
             */
            const Dob::Typesystem::Int64 GetLineNumber() const {return m_LineNumber;}

            /** @} */


            /**
             * Get the GetExceptionInfo as a UTF8 encoded string.
             *
             * This method is only provided to be compatible with the std::exception
             * class.
             *
             * If at all possible, use GetExceptionInfo instead since
             * this method performs a copy and a conversion of the string.
             * Note: This override allow catches of std::exception to still
             * print the complete exception message.
             *
             * @return The exception information as a UTF8 encoded string.
             */
            virtual const char * what() const throw();

            /**
             * Get the type id of this exception.
             *
             * Gets the type id of the exception. Method is virtual to ensure that the right
             * value gets returned for pointers or references.
             *
             * Note: This method is overridden by all auto-generated classes.
             *
             * @return The TypeId of the exception.
             */
            virtual TypeId GetTypeId() const = 0;

        private:
            Dob::Typesystem::Int64 m_LineNumber;
            std::wstring m_FileName;
            std::wstring m_Message;

            /**
             * This member variable holds the converted exception information string
             * when what() has been called.
             * It is mutable to allow the what() method to be declared-const (which it must be)
             */
            mutable std::string m_ConvertedMessage;
        };
    }

    /**
    * This is the base class of all Fundamental Exceptions.
    * All exceptions that signify "static errors" should inherit from this class.
    * Fundamental Exceptions are exceptions that are only expected to be caught
    * by the main loop in applications. They usually mean that something has gone very
    * wrong.
    */
    class DOTS_API FundamentalException :
        public Internal::CommonExceptionBase
    {
    public:
        /**
         * Constructor with exception information.
         *
         * Creates an exception that contains information about why and where the exception
         * occurred.
         *
         * @param message [in] - A message describing why the exception occurred.
         * @param fileName [in] - The name of the file where the exception occurred.
         *                        Always use __WFILE__ as the argument for this parameter.
         * @param lineNumber [in] - The line number that the exception occurred on.
         *                          Always use __LINE__ as the argument for this parameter.
         */
        FundamentalException(const std::wstring & message,
                             const std::wstring & fileName,            //Use the __WFILE__ macro
                             const Dob::Typesystem::Int64 lineNumber); //Use the __LINE__ macro

        /** The TypeId of this exception. */
        static const TypeId ExceptionTypeId = 5177142987005172374LL;

        //override of CommonExceptionBase::GetTypeId
        virtual TypeId GetTypeId() const {return ExceptionTypeId;}
    };

    /**
     * This is the base class of all (non-Fundamental) Exceptions.
     * In comparison to FundamentalException the exceptions that derive from Exception
     * are more of a dynamic nature. These exceptions are expected to be handled directly
     * by the calling function and are possible to recover from.
     */
    class DOTS_API Exception :
        public Internal::CommonExceptionBase
    {
    public:
        /**
         * Constructor with exception information.
         *
         * Creates an exception that contains information about why and where the exception
         * occurred.
         *
         * @param message [in] - A message describing why the exception occurred.
         * @param fileName [in] - The name of the file where the exception occurred.
         *                        Always use __WFILE__ as the argument for this parameter.
         * @param lineNumber [in] - The line number that the exception occurred on.
         *                          Always use __LINE__ as the argument for this parameter.
         */
        Exception(const std::wstring & message,
                  const std::wstring & fileName,            //Use the __WFILE__ macro
                  const Dob::Typesystem::Int64 lineNumber); //Use the __LINE__ macro

        /** The TypeId of this exception. */
        static const TypeId ExceptionTypeId = 8409897425067168944LL;

        //override of CommonExceptionBase::GetTypeId
        virtual TypeId GetTypeId() const {return ExceptionTypeId;}
    };

    /**
    * A parameter in the call was invalid.
    * One of the parameters in the call was out of range or unexpected in some other way.
    */
    class DOTS_API IllegalValueException :
        public FundamentalException
    {
    public:
        /**
         * Constructor with exception information.
         *
         * Creates an exception that contains information about why and where the exception
         * occurred.
         *
         * @param message [in] - A message describing why the exception occurred.
         * @param fileName [in] - The name of the file where the exception occurred.
         *                        Always use __WFILE__ as the argument for this parameter.
         * @param lineNumber [in] - The line number that the exception occurred on.
         *                          Always use __LINE__ as the argument for this parameter.
         */
        IllegalValueException(const std::wstring & message,
                              const std::wstring & fileName,           //Use the __WFILE__ macro
                              const Dob::Typesystem::Int64 lineNumber)://Use the __LINE__ macro
            FundamentalException(message,fileName,lineNumber) {}

        //override of CommonExceptionBase::GetName
        const std::wstring GetName() const {return L"Safir.Dob.Typesystem.IllegalValueException";}

        /** The TypeId of this exception. */
        static const TypeId ExceptionTypeId = -3653935143986901894LL;

        //override of CommonExceptionBase::GetTypeId
        virtual TypeId GetTypeId() const {return ExceptionTypeId;}
    };

    /**
    * This exception is thrown if a class cannot be cast to the expected type.
    * This usually signals a programming error in the client code.
    */
    class DOTS_API IncompatibleTypesException :
        public FundamentalException
    {
    public:
        /**
         * Constructor with exception information.
         *
         * Creates an exception that contains information about why and where the exception
         * occurred.
         *
         * @param message [in] - A message describing why the exception occurred.
         * @param fileName [in] - The name of the file where the exception occurred.
         *                        Always use __WFILE__ as the argument for this parameter.
         * @param lineNumber [in] - The line number that the exception occurred on.
         *                          Always use __LINE__ as the argument for this parameter.
         */
        IncompatibleTypesException(const std::wstring & message,
                                   const std::wstring & fileName,           //Use the __WFILE__ macro
                                   const Dob::Typesystem::Int64 lineNumber)://Use the __LINE__ macro
            FundamentalException(message,fileName,lineNumber) {}

        //override of CommonExceptionBase::GetName
        const std::wstring GetName() const {return L"Safir.Dob.Typesystem.IncompatibleTypesException";}

        /** The TypeId of this exception. */
        static const TypeId ExceptionTypeId = -5150658527844777416LL;

        //override of CommonExceptionBase::GetTypeId
        virtual TypeId GetTypeId() const {return ExceptionTypeId;}
    };

    /**
    * Used when there is an error that implies that there is
    *  something wrong in the configuration.
    */
    class DOTS_API ConfigurationErrorException :
        public FundamentalException
    {
    public:
        /**
         * Constructor with exception information.
         *
         * Creates an exception that contains information about why and where the exception
         * occurred.
         *
         * @param message [in] - A message describing why the exception occurred.
         * @param fileName [in] - The name of the file where the exception occurred.
         *                        Always use __WFILE__ as the argument for this parameter.
         * @param lineNumber [in] - The line number that the exception occurred on.
         *                          Always use __LINE__ as the argument for this parameter.
         */
        ConfigurationErrorException(const std::wstring & message,
                                    const std::wstring & fileName,           //Use the __WFILE__ macro
                                    const Dob::Typesystem::Int64 lineNumber)://Use the __LINE__ macro
            FundamentalException(message,fileName,lineNumber) {}

        //override of CommonExceptionBase::GetName
        const std::wstring GetName() const {return L"Safir.Dob.Typesystem.ConfigurationErrorException";}

        /** The TypeId of this exception. */
        static const TypeId ExceptionTypeId = 2909620812590558895LL;

        //override of CommonExceptionBase::GetTypeId
        virtual TypeId GetTypeId() const {return ExceptionTypeId;}
    };

    /**
    * Meant to be used when something goes very wrong.
    *  It means that there is a programming error somewhere.
    *  Can be used instead of using assert(...).
    */
    class DOTS_API SoftwareViolationException :
        public FundamentalException
    {
    public:
        /**
         * Constructor with exception information.
         *
         * Creates an exception that contains information about why and where the exception
         * occurred.
         *
         * @param message [in] - A message describing why the exception occurred.
         * @param fileName [in] - The name of the file where the exception occurred.
         *                        Always use __WFILE__ as the argument for this parameter.
         * @param lineNumber [in] - The line number that the exception occurred on.
         *                          Always use __LINE__ as the argument for this parameter.
         */
        SoftwareViolationException(const std::wstring & message,
                                   const std::wstring & fileName,           //Use the __WFILE__ macro
                                   const Dob::Typesystem::Int64 lineNumber)://Use the __LINE__ macro
            FundamentalException(message,fileName,lineNumber) {}

        //override of CommonExceptionBase::GetName
        const std::wstring GetName() const {return L"Safir.Dob.Typesystem.SoftwareViolationException";}

        /** The TypeId of this exception. */
        static const TypeId ExceptionTypeId = -2318636033853590373LL;

        //override of CommonExceptionBase::GetTypeId
        virtual TypeId GetTypeId() const {return ExceptionTypeId;}
    };

    /**
    * Thrown when an application attempts to get the value of a member that is null.
    */
    class DOTS_API NullException :
        public FundamentalException
    {
    public:
        /**
         * Constructor with exception information.
         *
         * Creates an exception that contains information about why and where the exception
         * occurred.
         *
         * @param message [in] - A message describing why the exception occurred.
         * @param fileName [in] - The name of the file where the exception occurred.
         *                        Always use __WFILE__ as the argument for this parameter.
         * @param lineNumber [in] - The line number that the exception occurred on.
         *                          Always use __LINE__ as the argument for this parameter.
         */
        NullException(const std::wstring & message,
                      const std::wstring & fileName,           //Use the __WFILE__ macro
                      const Dob::Typesystem::Int64 lineNumber)://Use the __LINE__ macro
            FundamentalException(message,fileName,lineNumber) {}

        //override of CommonExceptionBase::GetName
        const std::wstring GetName() const {return L"Safir.Dob.Typesystem.NullException";}

        /** The TypeId of this exception. */
        static const TypeId ExceptionTypeId = -6392953138294149211LL;

        //override of CommonExceptionBase::GetTypeId
        virtual TypeId GetTypeId() const {return ExceptionTypeId;}
    };


    /**
    * Used when someone tries to set a property that is mapped
    *  to something that cannot be changed.
    */
    class DOTS_API ReadOnlyException :
        public FundamentalException
    {
    public:
        /**
         * Constructor with exception information.
         *
         * Creates an exception that contains information about why and where the exception
         * occurred.
         *
         * @param message [in] - A message describing why the exception occurred.
         * @param fileName [in] - The name of the file where the exception occurred.
         *                        Always use __WFILE__ as the argument for this parameter.
         * @param lineNumber [in] - The line number that the exception occurred on.
         *                          Always use __LINE__ as the argument for this parameter.
         */
        ReadOnlyException(const std::wstring & message,
                          const std::wstring & fileName,           //Use the __WFILE__ macro
                          const Dob::Typesystem::Int64 lineNumber)://Use the __LINE__ macro
            FundamentalException(message,fileName,lineNumber) {}

        //override of CommonExceptionBase::GetName
        const std::wstring GetName() const {return L"Safir.Dob.Typesystem.ReadOnlyException";}

        /** The TypeId of this exception. */
        static const TypeId ExceptionTypeId = -4804695341042352897LL;

        //override of CommonExceptionBase::GetTypeId
        virtual TypeId GetTypeId() const {return ExceptionTypeId;}
    };
}
}
}
#endif
