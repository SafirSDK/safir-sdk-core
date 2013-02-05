/******************************************************************************
*
* Copyright Saab Systems AB, 2011 (http://www.safirsdk.com)
*
* Created by: Lars Hagström
*
******************************************************************************/
#ifndef __LLUF_UNORDERED_MAP_H__
#define __LLUF_UNORDERED_MAP_H__

#include <boost/version.hpp>


//If we've got boost > 1.42 we use their unordered map
#if ((BOOST_VERSION / 100000) >= 1 && (BOOST_VERSION / 100 % 1000) > 42)
    #include <boost/tr1/unordered_map.hpp>
    using std::tr1::unordered_map;
#else
    #if defined _MSC_VER
       #if (_MSC_VER >= 1500)
           #include <unordered_map>
           using std::tr1::unordered_map;
        #else
           #include <hash_map>
           #define unordered_map stdext::hash_map
        #endif
    #elif defined __GNUC__
        #include <tr1/unordered_map>
        using std::tr1::unordered_map;

        //The int64 hash is not predefined on g++ 4.1, we assume it is in > 4.2
        #if (__GNUC_MINOR__ < 2)
            namespace std {namespace tr1 {

               template<>
               struct hash<long long int>
               {
                   size_t
                   operator()(const long long int v) const
                   { return static_cast<size_t>(v);}
               };
            }}
        #endif

    #endif
#endif

#endif

