/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
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
#ifndef __DYNAMIC_LIBRARY_LOADER_H__
#define __DYNAMIC_LIBRARY_LOADER_H__

#include <string>
#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef lluf_utils_EXPORTS
#  define LLUF_UTILS_API SAFIR_HELPER_DLL_EXPORT
#else
#  define LLUF_UTILS_API SAFIR_HELPER_DLL_IMPORT
#  define SAFIR_LIBRARY_NAME "lluf_utils"
#  include <Safir/Utilities/Internal/AutoLink.h>
#endif
#define LLUF_UTILS_LOCAL SAFIR_HELPER_DLL_LOCAL

#ifndef SAFIR_NO_DEPRECATED

namespace Safir
{
namespace Utilities
{
    /**
     * This class provides a simple wrapper around dynamic loading
     * functionality of the operating system. E.g. dlopen/dlsym
     * of *nix and LoadLibrary/GetProcAddress of Win32.
     *
     * @deprecated This class lacks functionality and flexibility. A more competent alternative would be Boost.DLL.
     */
    class LLUF_UTILS_API DynamicLibraryLoader
    {
    public:
        /** Constructor */
        DynamicLibraryLoader();

        /** Destructor */
        ~DynamicLibraryLoader();

        /**
         * Load a library.
         *
         * Attempt to load the specified library dynamically.
         *
         * @param [in] libraryName The name of the library to load. On linux "lib" and ".so" are added
         *                         to the beginning and end of the name, and on windows ".dll" is appended.
         * @param [in] unloadOnDestruction If this is true the library will be unloaded when the object
         *                                 is destroyed. This will invalidate any function pointers.
         * @param [in] global Use RTLD_GLOBAL flag when loading on *nix.
         * @throws std::logic_error If library cannot be found or cannot be not loaded.
         */
        void Load(const std::string& libraryName,
                  const bool unloadOnDestruction,
                  const bool global = false);


        /**
         * Load a library from a given location.
         *
         * Attempt to load the specified library dynamically.
         *
         * @param [in] libraryName The name of the library to load. On linux "lib" and ".so" are added
         *                         to the beginning and end of the name, and on windows ".dll" is appended.
         * @param [in] path Location to load the library from.
         * @param [in] unloadOnDestruction If this is true the library will be unloaded when the object
         *                                 is destroyed. This will invalidate any function pointers.
         * @param [in] global Use RTLD_GLOBAL flag when loading on *nix.
         * @throws std::logic_error If library cannot be found or cannot be not loaded.
         */
        void Load(const std::string& libraryName,
                  const std::string& path,
                  const bool unloadOnDestruction,
                  const bool global = false);

        /**
         * Unload the library.
         */
        void Unload();

        /**
         * Find a function in the library.
         *
         * Attempt to load the specified function and return it as a function
         * pointer with the specified signature.
         * The return type is a raw function pointer rather than a std::function
         * object since otherwise things get very messy if you have to specify
         * calling convention.
         * But it easy to put the result into a function object.
         * For example:
         * std::function<double(int,int)> func =
         *     lib.GetFunction<double(int,int)>("myfunc")
         * And with specified calling convention on ms visual c++:
         * std::function<double(int,int)> func =
         *     lib.GetFunction<double __stdcall (int,int)>("myfunc")
         * And with specified calling convention on gcc:
         * std::function<double(int,int)> func =
         *     lib.GetFunction<double __attribute__(stdcall) (int,int)>("myfunc")
         *
         * @param [in] functionName Name of the function to load
         * @throws std::logic_error If function cannot be found.
         */
        template<class T>
        T* GetFunction(const std::string& functionName)
        {
            return reinterpret_cast<T*>(GetFunctionInternal(functionName));
        }

        /**
         * If this is true the library will be unloaded when the object
         * is destroyed. This will invalidate any function pointers.
         *
         * @param [in] value New value
         */
        void SetUnloadOnDestruction(const bool value) {m_unloadOnDestruction = value;}

        /**
         * If this is true the library will be unloaded when the object
         * is destroyed. This will invalidate any function pointers.
         *
         * @return The current value
         */
        bool GetUnloadOnDestruction() const {return m_unloadOnDestruction;}

    private:
        DynamicLibraryLoader(const DynamicLibraryLoader&) = delete;
        DynamicLibraryLoader& operator=(const DynamicLibraryLoader&) = delete;

        void * GetFunctionInternal(const std::string& functionName);

        class Impl;
        Impl* m_impl;

        bool m_unloadOnDestruction;
    };
}
}


#endif

#endif
