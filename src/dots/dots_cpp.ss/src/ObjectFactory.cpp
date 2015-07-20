/******************************************************************************
*
* Copyright Saab AB, 2006-2013,2015 (http://safir.sourceforge.net)
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

#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Utilities/DynamicLibraryLoader.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <boost/bind.hpp>
#include <boost/thread/mutex.hpp>
#include <sstream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    namespace
    {
        struct Unused{template<class T> Unused(const T&){}};  // To keep compilers happy about unused variable

        void LoadGeneratedLibrary(const std::string& path, std::string name)
        {
#if defined (_MSC_VER) && !defined (NDEBUG)
            name += 'd';
#endif

            try
            {
                Safir::Utilities::DynamicLibraryLoader loader;
                if (path.empty())
                {
                    loader.Load(name, false, true);
                }
                else
                {
                    loader.Load(name, path, false, true);
                }
            }
            catch (const std::logic_error& e)
            {
                SEND_SYSTEM_LOG (Critical,  << "Failed to load " << name.c_str() << " library: " << e.what());
                std::wostringstream ostr;
                ostr << "Failed to load " << name.c_str() << " library. Please check your configuration!";
                throw Safir::Dob::Typesystem::ConfigurationErrorException(ostr.str(), __WFILE__, __LINE__);
            }
        }

        bool LoadGeneratedLibraries()
        {
            DotsC_GeneratedLibrary* generatedLibraries;
            DotsC_Int32 size;
            DotsC_GeneratedLibraryListDeleter deleter;

            DotsC_GetGeneratedLibraryList(generatedLibraries,
                                          size,
                                          deleter);

            if (size ==0)
            {
                throw Safir::Dob::Typesystem::ConfigurationErrorException(L"Failed to read information from typesystem.ini",
                                                                          __WFILE__, __LINE__);
            }

            for (int i = 0; i < size; ++i)
            {
                std::string location;

                if (generatedLibraries[i].cppLibraryLocation != NULL)
                {
                    location = generatedLibraries[i].cppLibraryLocation;
                }

                if (generatedLibraries[i].library != 0)
                {
                    LoadGeneratedLibrary(location, generatedLibraries[i].cppLibraryName);
                }
            }

            deleter(generatedLibraries,size);

            return true;
        }

        const Unused loadedLibraries = LoadGeneratedLibraries();
    }

    boost::once_flag ObjectFactory::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;

    ObjectFactory & ObjectFactory::SingletonHelper::Instance()
    {
        static ObjectFactory instance;
        return instance;
    }

    ObjectFactory & ObjectFactory::Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
        return SingletonHelper::Instance();
    }

    ObjectFactory::ObjectFactory()
    {

    }

    ObjectFactory::~ObjectFactory()
    {

    }


    ObjectPtr
    ObjectFactory::CreateObject(char const * const blob) const
    {
        if (blob == NULL)
        {
            throw SoftwareViolationException(L"Cannot create object from NULL blob!",__WFILE__,__LINE__);
        }
        const TypeId typeId = DotsC_GetTypeId(blob);
        CallbackMap::const_iterator it = m_CallbackMap.find(typeId);
        if (it == m_CallbackMap.end())
        {
            std::wostringstream ostr;
            ostr << "There is no such type registered in the ObjectFactory: ";
            if (Operations::Exists(typeId))
            {
                ostr << Operations::GetName(typeId);
            }
            else
            {
                ostr << typeId;
            }
            throw IllegalValueException(ostr.str(),__WFILE__,__LINE__);
        }

        //invoke the function
        Int64 handle=DotsC_CreateBlobReader(blob);
        ObjectPtr obj=it->second(handle);
        DotsC_DeleteBlobReader(handle);
        return obj;
    }

    ObjectPtr
    ObjectFactory::CreateObject(const TypeId typeId) const
    {
        CallbackMap::const_iterator it = m_CallbackMap.find(typeId);
        if (it == m_CallbackMap.end())
        {
            std::wostringstream ostr;
            ostr << "There is no such type registered in the ObjectFactory: ";
            if (Operations::Exists(typeId))
            {
                ostr << Operations::GetName(typeId);
            }
            else
            {
                ostr << typeId;
            }
            throw IllegalValueException(ostr.str(),__WFILE__,__LINE__);
        }

        //invoke the function
        return it->second(0);
    }

    bool
    ObjectFactory::RegisterClass(const TypeId typeId, CreateObjectCallback createFunction)
    {
        return m_CallbackMap.insert(CallbackMap::value_type(typeId,createFunction)).second;
    }
}
}
}
