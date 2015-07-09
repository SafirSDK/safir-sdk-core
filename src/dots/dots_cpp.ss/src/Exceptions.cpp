/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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

#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <sstream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
#ifdef __GNUC__
    //obligatory static member initialization is needed by gcc
    const Safir::Dob::Typesystem::TypeId FundamentalException::ExceptionTypeId;
    const Safir::Dob::Typesystem::TypeId Exception::ExceptionTypeId;
    const Safir::Dob::Typesystem::TypeId IllegalValueException::ExceptionTypeId;
    const Safir::Dob::Typesystem::TypeId IncompatibleTypesException::ExceptionTypeId;
    const Safir::Dob::Typesystem::TypeId ConfigurationErrorException::ExceptionTypeId;
    const Safir::Dob::Typesystem::TypeId SoftwareViolationException::ExceptionTypeId;
    const Safir::Dob::Typesystem::TypeId NullException::ExceptionTypeId;
    const Safir::Dob::Typesystem::TypeId ReadOnlyException::ExceptionTypeId;
#endif

    //An anonymous namespace for exception factory registration
    //this can never be called directly by anyone (since it is anonymous)
    namespace
    {

        struct Unused{template<class T> Unused(const T&){}};  // To keep compilers happy about unused variable

        //register IllegalValueException in exception factory
        void ThrowIllegalValueException(const std::wstring & description)
        {
            throw IllegalValueException(description,__WFILE__,__LINE__);
        }
        const Unused registeredIllegalValueException =
                LibraryExceptions::Instance().RegisterException
                (Safir::Dob::Typesystem::IllegalValueException::ExceptionTypeId,
                 ThrowIllegalValueException);

        //register IncompatibleTypesException in exception factory
        void ThrowIncompatibleTypesException(const std::wstring & description)
        {
            throw IncompatibleTypesException(description,__WFILE__,__LINE__);
        }

        const Unused registeredIncompatibleTypesException =
            LibraryExceptions::Instance().RegisterException
            (Safir::Dob::Typesystem::IncompatibleTypesException::ExceptionTypeId,
             ThrowIncompatibleTypesException);

        //register ConfigurationErrorException in exception factory
        void ThrowConfigurationErrorException(const std::wstring & description)
        {
            throw ConfigurationErrorException(description,__WFILE__,__LINE__);
        }
        const Unused registeredConfigurationErrorException =
            LibraryExceptions::Instance().RegisterException
            (Safir::Dob::Typesystem::ConfigurationErrorException::ExceptionTypeId,
             ThrowConfigurationErrorException);


        //register SoftwareViolationException in exception factory
        void ThrowSoftwareViolationException(const std::wstring & description)
        {
            throw SoftwareViolationException(description,__WFILE__,__LINE__);
        }
        const Unused registeredSoftwareViolationException =
            LibraryExceptions::Instance().RegisterException
            (Safir::Dob::Typesystem::SoftwareViolationException::ExceptionTypeId,
             ThrowSoftwareViolationException);

        //register NullException in exception factory
        void ThrowNullException(const std::wstring & description)
        {
            throw NullException(description,__WFILE__,__LINE__);
        }
        const Unused registeredNullException =
            LibraryExceptions::Instance().RegisterException
            (Safir::Dob::Typesystem::NullException::ExceptionTypeId,
             ThrowNullException);

        //register ReadOnlyException in exception factory
        void ThrowReadOnlyException(const std::wstring & description)
        {
            throw ReadOnlyException(description,__WFILE__,__LINE__);
        }
        const Unused registeredReadOnlyException =
            LibraryExceptions::Instance().RegisterException
            (Safir::Dob::Typesystem::ReadOnlyException::ExceptionTypeId,
             ThrowReadOnlyException);
    }


    FundamentalException::FundamentalException(const std::wstring & message,
                                               const std::wstring & fileName,
                                               const Int64 lineNumber):
        Internal::CommonExceptionBase(message,fileName,lineNumber)
    {
    }

    Exception::Exception(const std::wstring & message,
                         const std::wstring & fileName,
                         const Int64 lineNumber):
        Internal::CommonExceptionBase(message,fileName,lineNumber)
    {

    }

    namespace Internal
    {
        const char STRING_CONVERSION_FAILED []= "FAILED TO CONVERT EXCEPTION MESSAGE";


        CommonExceptionBase::CommonExceptionBase(const std::wstring & message,
                                                 const std::wstring & fileName,
                                                 const Int64 lineNumber):
            m_LineNumber(lineNumber),
            m_FileName(fileName),
            m_Message(message),
            m_ConvertedMessage()
        {

        }


        CommonExceptionBase::~CommonExceptionBase() throw()
        {

        }

        const std::wstring
        CommonExceptionBase::GetExceptionInfo() const
        {
            //Format the information to the following style:
            //ExceptionName at some_file_name.cpp: 453
            //Description: Some kind of explanation of the exception
            std::wostringstream out;
            out << GetName()
                << L" at "
                << GetFileName()
                << L": "
                << GetLineNumber() << std::endl
                << L"Description: "
                << GetMessage();
            return out.str();
        }

        const char * CommonExceptionBase::what() const throw ()
        {
            try 
            {
                if (m_ConvertedMessage.empty())
                {
                    m_ConvertedMessage = Utilities::ToUtf8(GetExceptionInfo());
                }
                
                return m_ConvertedMessage.c_str();
            }
            catch (...)
            {
                return STRING_CONVERSION_FAILED;
            }
        }
    }
}
}
}
