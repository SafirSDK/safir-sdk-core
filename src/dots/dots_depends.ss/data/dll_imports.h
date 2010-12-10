/******************************************************************************
*
* Copyright Saab AB, 2006-2010 (http://www.safirsdk.com)
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

#ifdef _MSC_VER
#ifdef DOTS_GENERATED_CPP_EXPORTS
#define GENERATED_API __declspec(dllexport)
#else
#define GENERATED_API __declspec(dllimport)
#ifdef _DEBUG
#pragma comment( lib, "dots_generated-cppd.lib" )
#else
#pragma comment( lib, "dots_generated-cpp.lib" )
#endif
#endif
#endif
#ifdef __GNUC__
#define GENERATED_API
#endif

namespace Safir
{
    namespace Dob
    {
        namespace Typesystem
        {
            class GENERATED_API Dll_Imports
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

                //the single instance
                static Dll_Imports * volatile m_pInstance;

            };
        }
    }
}

#endif

