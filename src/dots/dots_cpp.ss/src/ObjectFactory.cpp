/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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
#include <Safir/Dob/Typesystem/BlobOperations.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Utilities/DynamicLibraryLoader.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <boost/thread/mutex.hpp>
#include <boost/bind.hpp>
#include <sstream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    namespace 
    {
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
            
            for (int i = 0; i < size; ++i)
            {
                std::string location;

                if (generatedLibraries[i].cppLibraryLocation != NULL)
                {
                    location = generatedLibraries[i].cppLibraryLocation;
                }
                
                if (generatedLibraries[i].library == 0)
                {
                    lllog(1) << "Not loading " << generatedLibraries[i].cppLibraryLocation << " since it is an override" << std::endl;
                }
                else
                {
                    lllog(1) << "Loading " << generatedLibraries[i].cppLibraryName << std::endl;
                    LoadGeneratedLibrary(location, generatedLibraries[i].cppLibraryName);
                }
            }

            return true;
        }

        const bool loadedLibraries = LoadGeneratedLibraries();
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
        const TypeId typeId = BlobOperations::GetTypeId(blob);
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
        return it->second(blob);
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
        return it->second(NULL);
    }

    bool 
    ObjectFactory::RegisterClass(const TypeId typeId, CreateObjectCallback createFunction)
    {
        lllout << "Adding type " << Operations::GetName(typeId) << " to ObjectFactory" << std::endl;
        return m_CallbackMap.insert(CallbackMap::value_type(typeId,createFunction)).second;
    }
}
}
}
