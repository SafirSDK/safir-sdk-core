/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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

//
// Test program for iterators
//

#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <iostream>
#include <boost/iterator/iterator_adaptor.hpp>
#include <map>

using namespace Safir::Dob::Typesystem;

int lastIteratorId = 0;
std::map <int,int> iteratorTable;

Int32 HandlerIteratorCreate(TypeId /*typeId*/,
                            bool /*includeSubclasses*/)
{
    ++lastIteratorId;
    iteratorTable.insert(std::make_pair(lastIteratorId,0));
    std::wcout << "HandlerIteratorCreate: " << lastIteratorId << std::endl;
    return lastIteratorId;
}

void HandlerIteratorDestroy(Int32 iterator)
{
    std::wcout << "HandlerIteratorDestroy: " << iterator << std::endl;
    iteratorTable.erase(iterator);
}

HandlerId HandlerIteratorDereference(Int32 iterator)
{
    std::wcout << "HandlerIteratorDereference: " << iterator << std::endl;
    return HandlerId(iteratorTable.find(iterator)->second+100);
}

void HandlerIteratorIncrement(Int32 & iterator,
                              bool & end)
{
    std::wcout << "HandlerIteratorIncrement: " << iterator << std::endl;
    end = false;
    std::map<int,int>::iterator findIt = iteratorTable.find(iterator);
    ++findIt->second;
    if (findIt->second > 3)
    {
        end = true;
    }
}

Int32 HandlerIteratorCopy(const Int32 iterator)
{
    std::wcout << "HandlerIteratorCopy: "<< iterator << std::endl;
    std::map<int,int>::iterator findIt = iteratorTable.find(iterator);
    ++lastIteratorId;
    iteratorTable.insert(std::make_pair(lastIteratorId,findIt->second));
    return lastIteratorId;
}

bool HandlerIteratorCompare(const Int32 first, const Int32 second)
{
    std::wcout << "HandlerIteratorCompare: "<< first << " and " << second << std::endl;
    std::map<int,int>::iterator findFirst = iteratorTable.find(first);
    std::map<int,int>::iterator findSecond = iteratorTable.find(second);
    return findFirst->second == findSecond->second;
}

class HandlerIterator:
    public boost::iterator_facade
    <
     HandlerIterator,
     HandlerId,
     boost::single_pass_traversal_tag,
     HandlerId
    >
{
public:
    HandlerIterator(): m_iteratorId(-1),m_end(true) {std::wcout << "Create End Iterator" << std::endl;}
    ~HandlerIterator() {HandlerIteratorDestroy(m_iteratorId);}

    HandlerIterator(const HandlerIterator & other)
    {
        m_iteratorId = HandlerIteratorCopy(other.m_iteratorId);
        m_end = other.m_end;
    }

    HandlerIterator & operator=(const HandlerIterator& other)
    {
        HandlerIteratorDestroy(m_iteratorId);
        m_iteratorId = HandlerIteratorCopy(other.m_iteratorId);
        m_end = other.m_end;
        return *this;
    }

private:
    friend class boost::iterator_core_access;
    friend HandlerIterator Get();

    HandlerId dereference() const
    {
        return HandlerIteratorDereference(m_iteratorId);
    }

    void increment()
    {
        HandlerIteratorIncrement(m_iteratorId,m_end);
    }

    bool equal(const HandlerIterator & other) const
    {
        if (m_end && other.m_end)
        {
            return true;
        }
        return HandlerIteratorCompare(m_iteratorId,other.m_iteratorId);
    }

    Int32 m_iteratorId;
    bool m_end;
};

HandlerIterator Get()
{
    HandlerIterator it;
    it.m_end=false;
    it.m_iteratorId = HandlerIteratorCreate(0,false);
    return it;
}

int main (int, char **)
{
    for (HandlerIterator it = Get(); it != HandlerIterator(); ++it)
    {
        std::wcout << *it << std::endl;
    }

    HandlerIterator one = Get();
    HandlerIterator two = Get();
    HandlerIterator three;
    three = one;
    three = two;
}
