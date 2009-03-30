/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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

#ifndef __DOSE_DLL_DEFS_H__
#define __DOSE_DLL_DEFS_H__

//Make a hash_map available even though their locations are different
//call it unordered_map, as it will be called in tr1
#if defined _MSC_VER
    #include <hash_map>
    #define unordered_map stdext::hash_map
#elif defined __GNUC__
    #include <tr1/unordered_map>
    using std::tr1::unordered_map;
#else
#error We need a definition of unordered_map
#endif


//Make a hash_set available even though their locations are different
//call it unordered_set, as it will be called in tr1
#if defined _MSC_VER
    #include <hash_set>
    #define unordered_set stdext::hash_set
    typedef stdext::hash_compare<void*> PointerHash;
#elif defined __GNUC__
    #include <tr1/unordered_set>
    using std::tr1::unordered_set;
    typedef std::tr1::hash<void*> PointerHash;
#else
#error We need a definition of unordered_set
#endif


#endif

