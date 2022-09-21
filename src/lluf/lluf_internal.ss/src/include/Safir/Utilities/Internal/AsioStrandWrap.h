/******************************************************************************
*
* Copyright Saab AB, 2013-2022 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@gmail.com
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
#pragma once

#include <boost/asio.hpp>

namespace Safir
{
namespace Utilities
{
namespace Internal
{

// Use WrapInStrand below
template <class StrandType, class Callback>
class StrandWrappedCallback
{
public:
    StrandWrappedCallback(StrandType& strand, const Callback& callback)
        :m_strand(strand)
        ,m_callback(callback) {}

    template <class... Args>
    void operator()(Args... a)
    {
        boost::asio::dispatch(m_strand, [=]{m_callback(a...);});
    }

private:
    StrandType& m_strand;
    Callback m_callback;
};


/**
 * @brief WrapInStrand - Creates an object that wraps a callback with return type void in a strand (dispatch).
 * @param strand - A strand object
 * @param callback - The callback that will be wrapped in the strand
 * @return A function object that can be stored in a std::function.
 */
template <class StrandType, class Callback>
StrandWrappedCallback<StrandType, Callback> WrapInStrand(StrandType& strand, Callback callback)
{
    StrandWrappedCallback<StrandType, Callback> wc(strand, callback);
    return wc;
}

}
}
}


