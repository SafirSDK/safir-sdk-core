/******************************************************************************
*
* Copyright Saab AB, 2006-2014 (http://safir.sourceforge.net)
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
#pragma once

#include <typeinfo>
#include <vector>
#include <boost/container/vector.hpp>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/ContainerBase.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    template <class T>
    class SequenceContainer : public ContainerBase
    {
    public:

        typedef T ContainedType;
        typedef boost::container::vector<T> StorageType;  //we use boost version instead of std because we want to be able to use vector<bool> without warnings and errors.
        typedef typename StorageType::const_iterator const_iterator;

        /**
         * Default Constructor.
         *
         * Construct a container that is not changed.
         */
        SequenceContainer()
            :m_isNull(true)
            ,m_values()
        {
        }

        //implementation of pure virtual in ContainerBase.
        virtual bool IsNull() const {return m_isNull;}

        //implementation of pure virtual in ContainerBase.
        virtual void SetNull()
        {
            m_values.clear();
            m_isNull=true;
            m_bIsChanged=true;
        }

        virtual bool IsChanged() const {return m_bIsChanged;}

        virtual void SetChanged(const bool changed) {m_bIsChanged=changed;}

        size_t size() const {return m_values.size();}

        bool empty() const {return m_values.empty();}

        const ContainedType& front() const {return m_values.front();}

        const ContainedType& back() const {return m_values.back();}

        const_iterator begin() const {return m_values.begin();}

        const_iterator end() const {return m_values.end();}

        void clear()
        {
            m_bIsChanged=true;
            m_values.clear();
        }

        const ContainedType& operator [](size_t index) const
        {
            return m_values[index];
        }

        void SetVal(size_t index, const ContainedType& val)
        {
            m_bIsChanged=true;
            m_values[index]=val;
        }

        const ContainedType& GetVal(size_t index) const
        {
            return m_values[index];
        }

        void push_back(const ContainedType& val)
        {
            m_isNull=false;
            m_bIsChanged=true;
            m_values.push_back(val);
        }

        void erase_at(size_t index)
        {
            m_bIsChanged=true;
            m_values.erase(m_values.begin()+index);
        }

        void insert_at(size_t index, const ContainedType& value)
        {
            m_values.insert(m_values.begin()+index, value);
        }

        virtual void Copy(const ContainerBase& /*that*/)
        {

        }

    private:
        bool m_isNull;
        StorageType m_values;
    };
}
}
}
