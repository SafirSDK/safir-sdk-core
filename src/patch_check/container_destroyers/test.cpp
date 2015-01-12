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
#include <boost/container/detail/destroyers.hpp>
#include <iostream>

using namespace boost::container;
using namespace boost::container::container_detail;

int destroy_calls = 0;

int* const pointer = new int;

struct Alloc
{
    typedef int value_type;

    void destroy(int* p)
    {
        ++destroy_calls;
        if (destroy_calls == 1 && pointer != p)
        {
            std::cout << "Unexpected pointer in destroy call "
                      << destroy_calls << ": " << std::hex << (void*)p << std::endl;
            exit(10);
        }
        if (destroy_calls == 2 && pointer == p)
        {
            std::cout << "Unexpected pointer in destroy call "
                      << destroy_calls << ": " << std::hex << (void*)p << std::endl;
            exit(12);
        }

    }
};

typedef scoped_destructor_n<Alloc> DestructorN;

int main()
{
    {
        std::cout << "Original pointer " << std::hex << (void*)pointer << std::endl;
        Alloc alloc;
        DestructorN d(pointer,alloc,1);
        d.increment_size(1);
    }
    if (destroy_calls != 2)
    {
        std::cout << "Unexpected number of destroy calls: " << destroy_calls << std::endl;
        return 20;
    }
    return 0;
}
