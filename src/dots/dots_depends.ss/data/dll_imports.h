/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
* 
* Created by: Mikael Wennerberg / stmiwn
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
#ifndef __DOTS_DLL_IMPORTS_H__
#define __DOTS_DLL_IMPORTS_H__

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef dots_generated_cpp_EXPORTS
#  define DOTS_GENERATED_CPP_API SAFIR_HELPER_DLL_EXPORT
#else
#  define DOTS_GENERATED_CPP_API SAFIR_HELPER_DLL_IMPORT
#  define SAFIR_LIBRARY_NAME "dots_generated-cpp"
#  include <Safir/Utilities/Internal/AutoLink.h>
#endif
#define DOTS_GENERATED_CPP_LOCAL SAFIR_HELPER_DLL_LOCAL

#include <boost/thread/once.hpp>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    class DOTS_GENERATED_CPP_API Dll_Imports
    {
        
    public:
        static bool Init();
        
    private:
        Dll_Imports();
        ~Dll_Imports();
        
        void LoadAllDlls();
        
        //disable copying by defining but not implementing copy construction and assignment
        Dll_Imports (const Dll_Imports &);
        const Dll_Imports & operator = (const Dll_Imports &);
        
        static void Create();
        
        //the single instance
        static Dll_Imports * volatile m_pInstance;
        static boost::once_flag m_onceFlag;
    };
}
}
}

#endif

