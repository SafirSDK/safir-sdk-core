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

#if defined _MSC_VER
#  define EXPORT __declspec(dllexport)
#  if defined (_M_IX86)
#    define STANDARD_CALLING_CONVENTION __cdecl
#    define ANOTHER_CALLING_CONVENTION __stdcall
#  elif defined (_M_AMD64)
     // Couldnt find any info on using other calling conventions on msvc 64
#    define STANDARD_CALLING_CONVENTION
#    define ANOTHER_CALLING_CONVENTION
#  endif
#elif defined __GNUC__
#  define EXPORT
#  if defined (__i386)
#    define STANDARD_CALLING_CONVENTION __attribute__((cdecl))
#    define ANOTHER_CALLING_CONVENTION __attribute__((stdcall))
#  elif defined (__x86_64)
#    define STANDARD_CALLING_CONVENTION __attribute__((sysv_abi))
#    define ANOTHER_CALLING_CONVENTION __attribute__((ms_abi))
#  elif defined (__arm__)
     // Can't get any calling conventions to work for arm. __attribute__((pcs("aapcs"))) doesnt work..
#    define STANDARD_CALLING_CONVENTION
#    define ANOTHER_CALLING_CONVENTION
#  endif
#endif

#if !defined (STANDARD_CALLING_CONVENTION) || !defined (ANOTHER_CALLING_CONVENTION)
#  error You need to define some calling conventions for this architecture
#endif

extern "C"
{
    EXPORT double STANDARD_CALLING_CONVENTION TestFunction(int a, long b, float c, double d)
    {
        return a + b + c + d;
    }

    EXPORT double ANOTHER_CALLING_CONVENTION TestFunction2(short a, int b, long c, float d, double e)
    {
        return a + b + c + d + e;
    }

}
