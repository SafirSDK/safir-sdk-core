/******************************************************************************
*
* Copyright Saab Systems AB, 2011 (http://www.safirsdk.com)
*
* Created by: Lars Hagström
*
******************************************************************************/
#ifndef __LLUF_UNORDERED_SET_H__
#define __LLUF_UNORDERED_SET_H__



//If we've got boost > 1.42 we use their unordered set
#if ((BOOST_VERSION / 100000) >= 1 && (BOOST_VERSION / 100 % 1000) > 42)
    #include <boost/tr1/unordered_set.hpp>
    using std::tr1::unordered_set;
#else
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

#endif
