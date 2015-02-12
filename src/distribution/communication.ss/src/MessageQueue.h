/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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

#include <vector>
#include <boost/circular_buffer.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
    template <class T>
    class MessageQueue
            :public boost::circular_buffer<T>
    {
    public:
        typedef MessageQueue<T> ThisType;

        MessageQueue(size_t size)
            :boost::circular_buffer<T>(size)
            ,m_tempStorage()
            ,m_firstUnhandled(0)
        {
            m_tempStorage.reserve(size);
        }

        void enqueue(const T& val)
        {
            if (!ThisType::full())
            {
                ThisType::push_back(val);
            }
            else
            {
                m_tempStorage.push_back(val);
            }
        }

        void dequeue()
        {
            ThisType::pop_front();
            if (!m_tempStorage.empty())
            {
                typename std::vector<T>::iterator it=m_tempStorage.begin();
                ThisType::push_back(*it);
                m_tempStorage.erase(it);
            }

            if (m_firstUnhandled>0)
            {
                --m_firstUnhandled;
            }
        }

        bool has_unhandled() const
        {
            return m_firstUnhandled<ThisType::size();
        }

        void step_unhandled()
        {
            if (m_firstUnhandled<ThisType::size())
            {
                ++m_firstUnhandled;
            }
        }

        size_t first_unhandled_index() const
        {
            return m_firstUnhandled;
        }

        void clear_queue()
        {
            ThisType::clear();
            m_tempStorage.clear();
            m_firstUnhandled=0;
        }

        template <class OStreamT>
        void DumpInfo(OStreamT& os) const
        {
            os<<"size="<<ThisType::size()<<std::endl;
            os<<"firstUnhandled=";
            if (!ThisType::has_unhandled())
                os<<"None"<<std::endl;
            else
                os<<m_firstUnhandled<<std::endl;
            os<<"full="<<std::boolalpha<<ThisType::full()<<std::dec<<std::endl;
            os<<"tmpStorageSize="<<m_tempStorage.size()<<std::endl;
        }

    private:
        //When m_storage is full, there can be a few messages posted to be asynch inserted before it became full
        //those are added here. Also big messages that needs to be split into fragments sometimes fills upp the queue
        //and the exceeding message parts will temporarily be stored here.
        //
        std::vector<T> m_tempStorage;
        size_t m_firstUnhandled;

    };

    template <class T>
    class CircularArray
    {
    public:
        CircularArray(size_t size)
            :m_zeroIndex(0)
        {
            m_storage.resize(size);
        }

        const T& operator[](size_t index) const
        {
            return m_storage[InternalIndex(index)];
        }

        T& operator[](size_t index)
        {
            return m_storage[InternalIndex(index)];
        }

        void Step(size_t steps=1)
        {
            m_zeroIndex=InternalIndex(steps);
        }

        template <class OStreamT>
        void DumpInfo(OStreamT& os) const
        {
            for (size_t i=0; i<m_storage.size(); ++i)
            {
                os<<i<<". "<<this->operator [](i)<<std::endl;
            }
        }

        size_t Size() const {return m_storage.size();}

    private:
        size_t m_zeroIndex;
        std::vector<T> m_storage;
        size_t InternalIndex(size_t index) const {return (index+m_zeroIndex)%m_storage.size();}
    };
}
}
}
}
