/******************************************************************************
*
* Copyright Saab AB, 2004-2014 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#pragma once

#include <typeinfo>
#include <vector>
#include <boost/unordered_map.hpp>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/ContainerBase.h>

#include <map>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    /**
     * Container class for sequences of values. A sequence is a collection of values that can dynamically
     * grow or shrink in size. The whole container can be null, and has a change flag that will automatically
     * be set when values are added, removed or changed or when the container is set to null. Individual values
     * does not have change flags and can not be null.
     */
    template <class KeyT, class ValT>
    class DictionaryContainer : public ContainerBase
    {
    public:

        typedef KeyT KeyType;
        typedef ValT ValType;
        typedef boost::unordered_map<KeyType, ValType> StorageType;  //we use boost version instead of std because we want to be able to use vector<bool> without warnings and errors.
        typedef typename StorageType::const_iterator const_iterator;
        typedef typename StorageType::value_type value_type;

        /**
         * Default Constructor.
         *
         * Construct a container that is not changed and not null.
         */
        DictionaryContainer()
            :m_isNull(true)
            ,m_values()
        {
        }

        /**
         * @brief IsNull - Check if the whole sequence is null.
         * @return True if the sequence is null, else false.
         */
        virtual bool IsNull() const {return m_isNull;}

        /**
         * @brief SetNull - Set the whole sequence to null.
         */
        virtual void SetNull()
        {
            m_values.clear();
            m_isNull=true;
            m_bIsChanged=true;
        }

        /**
         * @brief IsChanged - Check if the sequence has changed.
         * @return True if changed, else false.
         */
        virtual bool IsChanged() const
        {
            if (m_bIsChanged)
            {
                return true; //top level change flag is set
            }

            for (const_iterator it=m_values.begin(); it!=m_values.end(); ++it)
            {
                if (it->second.IsChanged()) //a value container has changed flag set
                    return true;
            }

            return false; //if we get here nothing is changed

        }

        /**
         * @brief SetChanged - Set the change state of the sequence.
         * @param changed [in] - If true, the sequence is set to changed, it is set to not changed.
         */
        virtual void SetChanged(const bool changed)
        {
            m_bIsChanged=changed;
            if (!changed)
            {
                for (typename StorageType::iterator it=m_values.begin(); it!=m_values.end(); ++it)
                {
                    it->second.SetChanged(false);
                }
            }
        }

        /**
         * @brief size - Get the size of the sequence, i.e number of contained values.
         * @return The number of values in the sequence.
         */
        size_t size() const {return m_values.size();}

        /**
         * @brief empty - Check if sequence is empty.
         * @return True if sequence is empty, else false.
         */
        bool empty() const {return m_values.empty();}

        /**
         * @brief begin - Get const_iterator pointing to the first element in the sequence.
         * @return const_iterator.
         */
        const_iterator begin() const {return m_values.begin();}

        /**
         * @brief end - Get const_iterator pointing past the last element in the sequence.
         * @return const_iterator.
         */
        const_iterator end() const {return m_values.end();}

        /**
         * @brief clear - Clear the sequence, i.e remove all values. After a call to clear
         * the sequence will be empty but not automatically set to null.
         */
        void clear()
        {
            m_bIsChanged=true;
            m_values.clear();
        }

        void insert(const value_type& val)
        {
            m_bIsChanged=true;
            m_values.insert(val);
        }

        void insert(const KeyType& key, const ValType& val)
        {
            insert(value_type(key, val));
        }

        bool Exist(const KeyType& key) const
        {
            return m_values.find(key)!=m_values.end();
        }

        /**
         * @brief operator [] - Get const reference to the value with specified index.
         * @param index [in] - Index of the value to get.
         * @return Const reference to a value.
         */
        const ValType& operator [](const KeyType& key) const
        {
            const_iterator it= m_values.find(key);
            if (it!=m_values.end())
            {
                return it->second;
            }

            throw std::logic_error("Key does not exist!");
        }

        /**
         * @brief Copy - Copy all the members from "that" into "this". Types must be the same for this to work!
         * @param that [in] - The object to copy into this.
         * @throws SoftwareViolationException If the types are not of the same kind.
         */
        virtual void Copy(const ContainerBase& /*that*/)
        {
            //TODO
        }

    private:
        bool m_isNull;
        StorageType m_values;
    };
}
}
}
