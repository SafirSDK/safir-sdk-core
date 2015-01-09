/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#include <Safir/Utilities/DynamicLibraryLoader.h>
#include <iostream>

#if defined(linux) || defined(__linux) || defined(__linux__)
#  include <dlfcn.h>
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#  include <windows.h>
#else
#  error You need to implement DynamicLibraryLoader for this platform!
#endif


namespace Safir
{
namespace Utilities
{
    class DynamicLibraryLoader::Impl 
    {
    public:
        Impl()
            : m_handle(NULL)
        {

        }
        
        bool Loaded() const {return m_handle != NULL;}

        void Load(const std::string& libraryName, const bool global)
        {
#if defined(linux) || defined(__linux) || defined(__linux__)
            const std::string filename = std::string("lib") + libraryName + ".so";

            //clear any previous errors
            dlerror();

            m_handle = dlopen(filename.c_str(), RTLD_NOW | (global ? RTLD_GLOBAL : 0));

            if (m_handle == NULL)
            {
                throw std::logic_error("Failed to load library '"
                                       + libraryName
                                       + "' (tried loading file with name '"
                                       + filename +"'). dlerror() info:\n" + dlerror());
            }

#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
            (global); //unreferenced parameter
            const std::string filename = libraryName + ".dll";
            m_handle = LoadLibraryA(filename.c_str());

            if (m_handle == NULL)
            {
                throw std::logic_error("Failed to load library '"
                                       + libraryName
                                       + "' (tried loading file with name '"
                                       + filename +"')");
            }
#endif
        }

        void Load(const std::string& libraryName, const std::string& path, const bool global)
        {
#if defined(linux) || defined(__linux) || defined(__linux__)
            const std::string filename = path + "/" + std::string("lib") + libraryName + ".so";

            //clear any previous errors
            dlerror();

            m_handle = dlopen(filename.c_str(), RTLD_NOW | (global ? RTLD_GLOBAL : 0));

            if (m_handle == NULL)
            {
                throw std::logic_error("Failed to load library '"
                                       + libraryName
                                       + "' (tried loading file with name '"
                                       + filename +"'). dlerror() info:\n" + dlerror());
            }

#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
            (global); //unreferenced parameter

            std::string filename = path;
            if (path.back() != '\\' && path.back() != '/')
            {
                filename += "\\";
            }
            filename += libraryName + ".dll";
            m_handle = LoadLibraryA(filename.c_str());

            if (m_handle == NULL)
            {
                throw std::logic_error("Failed to load library '"
                                       + libraryName
                                       + "' (tried loading file with name '"
                                       + filename +"')");
            }
#endif
        }


        void Unload()
        {
#if defined(linux) || defined(__linux) || defined(__linux__)
            const bool result = (0 == dlclose(m_handle));
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
            const bool result = (TRUE == FreeLibrary(m_handle));
#endif
            if (!result)
            {
                throw std::logic_error("Failed to unload library!");
            }

            m_handle = NULL;
        }

        void* GetFunctionAddress(const std::string& functionName)
        {
#if defined(linux) || defined(__linux) || defined(__linux__)
            void* result = dlsym(m_handle, functionName.c_str());
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
            void* result = GetProcAddress(m_handle, functionName.c_str());
#endif
            if (result == NULL)
            {
                throw std::logic_error("Can't find symbol named '" + functionName + "'");
            }
            return result;
        }

    private:
#if defined(linux) || defined(__linux) || defined(__linux__)
        void* m_handle;
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        HMODULE m_handle;
#endif
    };
    
    DynamicLibraryLoader::DynamicLibraryLoader()
        : m_impl(new Impl())
        , m_unloadOnDestruction(false)
    {
        
    }
    
    DynamicLibraryLoader::~DynamicLibraryLoader()
    {
        if (m_unloadOnDestruction)
        {
            Unload();
        }
    }

    void DynamicLibraryLoader::Load(const std::string& libraryName,
                                    const bool unloadOnDestruction,
                                    const bool global)
    {
        if (m_impl->Loaded())
        {
            throw std::logic_error("A library has already been loaded.");
        }
        m_impl->Load(libraryName, global);
        m_unloadOnDestruction = unloadOnDestruction;
    }


    void DynamicLibraryLoader::Load(const std::string& libraryName,
                                    const std::string& path,
                                    const bool unloadOnDestruction,
                                    const bool global)
    {
        if (m_impl->Loaded())
        {
            throw std::logic_error("A library has already been loaded.");
        }
        m_impl->Load(libraryName, path, global);
        m_unloadOnDestruction = unloadOnDestruction;
    }
    
    void DynamicLibraryLoader::Unload()
    {
        if (m_impl->Loaded())
        {
            m_impl->Unload();
        }
    }

    void * DynamicLibraryLoader::GetFunctionInternal(const std::string& functionName)
    {
        if (!m_impl->Loaded())
        {
            throw std::logic_error("No library has been loaded!");
        }
        return m_impl->GetFunctionAddress(functionName);        
    }

}
}



