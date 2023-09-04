/******************************************************************************
*
* Copyright Saab AB, 2006-2013,2015 (http://safirsdkcore.com)
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
#include <sstream>
#include <unordered_map>

#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    namespace
    {
        struct Unused{template<class T> Unused(const T&){}};  // To keep compilers happy about unused variable

        void LoadGeneratedLibrary(const std::string& path, const std::string& name)
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

    std::once_flag ObjectFactory::SingletonHelper::m_onceFlag;

    ObjectFactory & ObjectFactory::SingletonHelper::Instance()
    {
        static ObjectFactory instance;
        return instance;
    }

    class ObjectFactory::Impl
    {
    public:
        typedef std::unordered_map<TypeId,CreateObjectCallback> CallbackMap;
        CallbackMap m_CallbackMap;
    };


    ObjectFactory & ObjectFactory::Instance()
    {
        std::call_once(SingletonHelper::m_onceFlag,[]{SingletonHelper::Instance();});
        return SingletonHelper::Instance();
    }

    ObjectFactory::ObjectFactory()
        : m_impl(new Impl)
    {

    }

    ObjectFactory::~ObjectFactory()
    {
        delete m_impl;
    }


    ObjectPtr
    ObjectFactory::CreateObject(char const * const blob) const
    {
        if (blob == NULL)
        {
            throw SoftwareViolationException(L"Cannot create object from NULL blob!",__WFILE__,__LINE__);
        }
        const TypeId typeId = DotsC_GetTypeId(blob);
        Impl::CallbackMap::const_iterator it = m_impl->m_CallbackMap.find(typeId);
        if (it == m_impl->m_CallbackMap.end())
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
        Impl::CallbackMap::const_iterator it = m_impl->m_CallbackMap.find(typeId);
        if (it == m_impl->m_CallbackMap.end())
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
        return m_impl->m_CallbackMap.insert(Impl::CallbackMap::value_type(typeId,createFunction)).second;
    }

    std::vector<TypeId> ObjectFactory::GetRegisteredTypes() const
    {
        std::vector<TypeId> typeIds;
        for (const auto& kv : m_impl->m_CallbackMap)
        {
            typeIds.push_back(kv.first);
        }
        return typeIds;
    }
}
}
}
