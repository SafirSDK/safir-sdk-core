/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#ifndef __SAFIR_MAKE_UNIQUE_H__
#define __SAFIR_MAKE_UNIQUE_H__

namespace Safir
{
    template<typename T>
    std::unique_ptr<T> make_unique( )
    { return std::unique_ptr<T>( new T( ) ); }

    template<typename T, typename A1>
    std::unique_ptr<T> make_unique( A1&& a1 )
    { return std::unique_ptr<T>( new T( std::forward<A1>(a1) ) ); }
    
    template<typename T, typename A1, typename A2>
    std::unique_ptr<T> make_unique( A1&& a1, A2&& a2 )
    { return std::unique_ptr<T>( new T( std::forward<A1>(a1), std::forward<A2>(a2) ) ); }
    
    template<typename T, typename A1, typename A2, typename A3>
    std::unique_ptr<T> make_unique( A1&& a1, A2&& a2, A3&& a3 )
    { return std::unique_ptr<T>( new T( std::forward<A1>(a1), std::forward<A2>(a2), std::forward<A3>(a3) ) ); }

    template<typename T, typename A1, typename A2, typename A3, typename A4>
    std::unique_ptr<T> make_unique( A1&& a1, A2&& a2, A3&& a3, A4&& a4 )
    { return std::unique_ptr<T>( new T( std::forward<A1>(a1), std::forward<A2>(a2), std::forward<A3>(a3), std::forward<A4>(a4) ) ); }

    template<typename T, typename A1, typename A2, typename A3, typename A4, typename A5>
    std::unique_ptr<T> make_unique( A1&& a1, A2&& a2, A3&& a3, A4&& a4, A5&& a5 )
    { return std::unique_ptr<T>( new T( std::forward<A1>(a1), std::forward<A2>(a2), 
                                        std::forward<A3>(a3), std::forward<A4>(a4),
                                        std::forward<A5>(a5) ) ); }

    template<typename T, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6>
    std::unique_ptr<T> make_unique( A1&& a1, A2&& a2, A3&& a3, A4&& a4, A5&& a5, A6&& a6 )
    { return std::unique_ptr<T>( new T( std::forward<A1>(a1), std::forward<A2>(a2), 
                                        std::forward<A3>(a3), std::forward<A4>(a4),
                                        std::forward<A5>(a5), std::forward<A6>(a6) ) ); }

    template<typename T, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7>
            std::unique_ptr<T> make_unique( A1&& a1, A2&& a2, A3&& a3, A4&& a4, A5&& a5, A6&& a6, A7&& a7 )
    { return std::unique_ptr<T>( new T( std::forward<A1>(a1), std::forward<A2>(a2), 
                                        std::forward<A3>(a3), std::forward<A4>(a4),
                                        std::forward<A5>(a5), std::forward<A6>(a6),
                                        std::forward<A7>(a7) ) ); }

    template<typename T, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6, typename A7, typename A8>
    std::unique_ptr<T> make_unique( A1&& a1, A2&& a2, A3&& a3, A4&& a4, A5&& a5, A6&& a6, A7&& a7, A8&& a8 )
    { return std::unique_ptr<T>( new T( std::forward<A1>(a1), std::forward<A2>(a2), 
                                        std::forward<A3>(a3), std::forward<A4>(a4),
                                        std::forward<A5>(a5), std::forward<A6>(a6),
                                        std::forward<A7>(a7), std::forward<A8>(a8) ) ); }

    template<typename T, typename A1, typename A2, typename A3, typename A4, typename A5, 
             typename A6, typename A7, typename A8, typename A9>
    std::unique_ptr<T> make_unique( A1&& a1, A2&& a2, A3&& a3, A4&& a4, A5&& a5, 
                                    A6&& a6, A7&& a7, A8&& a8, A9&& a9 )
    { return std::unique_ptr<T>( new T( std::forward<A1>(a1), std::forward<A2>(a2), 
                                        std::forward<A3>(a3), std::forward<A4>(a4),
                                        std::forward<A5>(a5), std::forward<A6>(a6),
                                        std::forward<A7>(a7), std::forward<A8>(a8),
                                        std::forward<A9>(a9) ) ); }

    template<typename T, typename A1, typename A2, typename A3, typename A4, typename A5, 
             typename A6, typename A7, typename A8, typename A9, typename A10>
    std::unique_ptr<T> make_unique( A1&& a1, A2&& a2, A3&& a3, A4&& a4, A5&& a5, 
                                    A6&& a6, A7&& a7, A8&& a8, A9&& a9, A10&& a10 )
    { return std::unique_ptr<T>( new T( std::forward<A1>(a1), std::forward<A2>(a2), 
                                        std::forward<A3>(a3), std::forward<A4>(a4),
                                        std::forward<A5>(a5), std::forward<A6>(a6),
                                        std::forward<A7>(a7), std::forward<A8>(a8),
                                        std::forward<A9>(a9), std::forward<A10>(a10) ) ); }

 
    // etc. for as many constructor parameters as you want to support
}
#endif

