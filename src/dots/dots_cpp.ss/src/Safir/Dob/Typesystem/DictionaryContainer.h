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
    class DictionaryContainer : public ContainerBase,
                                private boost::unordered_map<KeyT, ValT>
    {
    public:

        typedef KeyT KeyType;
        typedef ValT ValueContainerType;
        typedef typename ValueContainerType::ContainedType ContainedType;
        typedef boost::unordered_map<KeyType, ValueContainerType> StorageType;  //we use boost version instead of std because we want to be able to use vector<bool> without warnings and errors.
        typedef typename StorageType::const_iterator const_iterator;
        typedef typename StorageType::value_type value_type;

        /**
         * Default Constructor.
         *
         * Construct a container that is not changed and not null.
         */
        DictionaryContainer()
            :m_isNull(true)
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
            StorageType::clear();
            m_isNull=true;
            m_bIsChanged=true;
        }

        using StorageType::begin;
        using StorageType::end;
        using StorageType::find;
        using StorageType::operator[];
        using StorageType::size;
        using StorageType::empty;

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

            for (const_iterator it=begin(); it!=end(); ++it)
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
                for (typename StorageType::iterator it=begin(); it!=end(); ++it)
                {
                    it->second.SetChanged(changed);
                }
            }
        }

        /**
         * @brief clear - Clear the sequence, i.e remove all values. After a call to clear
         * the sequence will be empty but not automatically set to null.
         */
        void clear()
        {
            m_bIsChanged=true;
            StorageType::clear();
        }


        void Insert(const KeyType& key, const ContainedType& val)
        {
            m_bIsChanged=true;
            ValueContainerType container;
            container.SetVal(val);
            container.SetChanged(true);
            StorageType::insert(value_type(key, container));
        }

        size_t erase(const KeyType& key)
        {
            m_bIsChanged=true;
            return StorageType::erase(key);
        }

        bool Exist(const KeyType& key) const
        {
            return find(key)!=end();
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
    };
}
}
}
