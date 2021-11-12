/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <sstream>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/thread/mutex.hpp>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    std::once_flag LibraryExceptions::SingletonHelper::m_onceFlag;

    // -----------------------------------------------------------
    LibraryExceptions & LibraryExceptions::SingletonHelper::Instance()
    {
        static LibraryExceptions instance;
        return instance;
    }

    class LibraryExceptions::Impl
    {
    public:
        typedef std::unordered_map<TypeId, ThrowExceptionCallback> CallbackMap;
        CallbackMap m_CallbackMap;
    };

    // -----------------------------------------------------------
    LibraryExceptions & LibraryExceptions::Instance()
    {
        std::call_once(SingletonHelper::m_onceFlag,[]{SingletonHelper::Instance();});
        return SingletonHelper::Instance();
    }

    // -----------------------------------------------------------
    LibraryExceptions::LibraryExceptions()
        : m_impl(new Impl)
    {

    }

    // -----------------------------------------------------------
    LibraryExceptions::~LibraryExceptions()
    {
        delete m_impl;
    }

    bool LibraryExceptions::RegisterException(const TypeId exceptionId, ThrowExceptionCallback throwFunction)
    {
        return m_impl->m_CallbackMap.insert(Impl::CallbackMap::value_type(exceptionId,throwFunction)).second;
    }

    // -----------------------------------------------------------
    void LibraryExceptions::Set(const FundamentalException & exception)
    {
        Set(static_cast<const Internal::CommonExceptionBase &>(exception));
    }


    // -----------------------------------------------------------
    void LibraryExceptions::Set(const Exception & exception)
    {
        Set(static_cast<const Internal::CommonExceptionBase &>(exception));
    }

    // -----------------------------------------------------------
    void LibraryExceptions::Set(const Internal::CommonExceptionBase & exception)
    {
        DotsC_SetException(exception.GetTypeId(),
                           Utilities::ToUtf8(exception.GetExceptionInfo()).c_str());
    }

    // -----------------------------------------------------------
    void LibraryExceptions::Set(const std::exception & exception)
    {
        DotsC_SetException(0,exception.what());
    }


    // -----------------------------------------------------------
    void LibraryExceptions::SetUnknown()
    {
        DotsC_SetException(0,"Unknown exception (caught as ...)");
    }


    // -----------------------------------------------------------
    void LibraryExceptions::AppendDescription(const std::wstring & moreDescription)
    {
        DotsC_AppendExceptionDescription(Utilities::ToUtf8(moreDescription).c_str());
    }

    // -----------------------------------------------------------
    /*    void LibraryExceptions::Set(const TypeId exceptionId, const std::wstring & description)
    {
        DotsC_SetException(exceptionId,Utilities::ToUtf8(description).c_str());
        }*/

    static const char * err1 = "Failed to copy the exception string in UnknownException constructor";
    static const char * err2 = "Failed to extract the c_str from the std::string in what()";

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4275) //unreachable code
#endif

    class DOTS_CPP_API UnknownException:
        public std::exception
    {
    public:
        UnknownException(const std::string & what) throw()
        {
            try
            {
                m_what = what;
            }
            catch (...)
            {

            }
        }

        ~UnknownException() throw() {}

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4702) //unreachable code
#endif

        const char * what() const throw () override
        {
            try
            {
                if (m_what.empty())
                {
                    return err1;
                }
                return m_what.c_str();
            }
            catch (...)
            {
                return err2;
            }
        }

#ifdef _MSC_VER
#pragma warning (pop)
#endif

    private:

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251) //unreachable code
#endif

        std::string m_what;

#ifdef _MSC_VER
#pragma warning (pop)
#endif
    };

#ifdef _MSC_VER
#pragma warning (pop)
#endif
    // -----------------------------------------------------------
    void LibraryExceptions::Throw()
    {
        bool wasSet;
        TypeId exceptionId;
        char * description;
        DotsC_BytePointerDeleter deleter;
        DotsC_GetAndClearException(exceptionId,description,deleter,wasSet);
        if (wasSet)
        {
            const std::string desc(description);
            deleter(description);
            Throw(exceptionId,desc);
        }
        else
        {
            throw SoftwareViolationException(L"There was no exception set when LibraryExceptions::Throw was called!",__WFILE__,__LINE__);
        }
    }

    // -----------------------------------------------------------
    void LibraryExceptions::Throw(const TypeId exceptionId, const std::string& desc) const
    {
        if (exceptionId == 0)
        {
            throw UnknownException(desc);
        }
        else
        {
            const std::wstring description = Utilities::ToWstring(desc);
            Impl::CallbackMap::const_iterator it = m_impl->m_CallbackMap.find(exceptionId);
            if (it == m_impl->m_CallbackMap.end())
            {
                std::wostringstream ostr;
                ostr << "LibraryExceptions::Throw was called with an exception that was not registered in the exception-factory!" << std::endl
                     << "exceptionId = " << exceptionId << ", description = '" << description << "'." << std::endl
                     << "Please report this to your nearest DOB developer!";
                lllout << ostr.str() << std::endl;
                throw SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
            }
            //invoke the function
            it->second(description);
        }
    }

}
}
}
