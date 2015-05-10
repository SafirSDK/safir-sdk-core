/******************************************************************************
*
* Copyright Consoden AB, 2004-2015 (http://safir.sourceforge.net)
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
#include <map>
#include <Safir/Dob/Typesystem/ContainerBase.h>
#include <Safir/Dob/Typesystem/Exceptions.h>

#include <map>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    /**
     * Container class for dictionaries of key value pairs. A dictionary is a collection of values that can dynamically
     * grow or shrink in size. The whole container has a change flag that will automatically
     * be set when values are added, removed or changed.
     */
    template <class KeyT, class ValT>
    class DictionaryContainer : public ContainerBase
    {
    public:

        typedef KeyT KeyType;
        typedef ValT ValueContainerType;
        typedef typename ValueContainerType::ContainedType ContainedType;
        typedef std::map<KeyType, ValueContainerType> StorageType;  //we use boost version instead of std because we want to be able to use vector<bool> without warnings and errors.
        typedef typename StorageType::const_iterator const_iterator;
        typedef typename StorageType::iterator iterator;
        typedef typename StorageType::value_type value_type;

        /**
         * Default Constructor.
         *
         * Construct a container that is not changed and not null.
         */
        DictionaryContainer()
            :ContainerBase()
            ,m_values()
        {
        }

        virtual bool IsNull() const {return false;}

        virtual void SetNull()
        {
            throw SoftwareViolationException(L"Dictionaries cannot be null",__WFILE__,__LINE__);
        }

        iterator begin() {return m_values.begin();}
        const_iterator begin() const {return m_values.begin();}

        iterator end() {return m_values.end();}
        const_iterator end() const {return m_values.end();}

        iterator find(const KeyType& key) {return m_values.find(key);}
        const_iterator find(const KeyType& key) const {return m_values.find(key);}

        size_t size() const {return m_values.size();}

        bool empty() const {return m_values.empty();}

        size_t count(const KeyType& key) const {return m_values.count(key);}

        ValueContainerType& operator[](const KeyType& key)
        {
            iterator it=m_values.find(key);
            if (it!=m_values.end())
            {
                return it->second;
            }
            else
            {
                m_bIsChanged=true;
                ValueContainerType& ct=m_values[key];
                ct.SetChanged(true);
                return ct;
            }
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
            for (iterator it=m_values.begin(); it!=m_values.end(); ++it)
            {
                it->second.SetChanged(changed);
            }
        }

        /**
         * Is the change flag in the container set?
         *
         * This method is like IsChanged without the recursion (on dictionary containers IsChanged is recursive).
         *
         * @return True if the containers change flag is set.
         */
        bool IsChangedHere() const {return m_bIsChanged;}

        /**
         * Set the change flag in the container.
         *
         * This method is like SetChanged without the recursion (on dictionary containers SetChanged is recursive).
         *
         * @param changed [in] - The value to set the change flag to.
         */
        void SetChangedHere(const bool changed) {m_bIsChanged = changed;}

        //TODO: is it possible to send a request that only adds one entry into the dictionary with SetChanges?
        //TODO: in dose tests change the keys as well
        //TODO Do we need changedhere?
        /**
         * @brief clear - Clear the sequence, i.e remove all values. After a call to clear
         * the sequence will be empty but not automatically set to null.
         */
        void clear()
        {
            m_bIsChanged=true;
            m_values.clear();
        }


        void Insert(const KeyType& key, const ContainedType& val)
        {
            m_bIsChanged=true;
            ValueContainerType container;
            container.SetVal(val);
            container.SetChanged(true);
            m_values.insert(value_type(key, container));
        }

        size_t erase(const KeyType& key)
        {
            size_t count=m_values.erase(key);
            if (count>0)
            {
                m_bIsChanged=true;
            }
            return count;
        }

        /**
         * @brief Copy - Copy all the members from "that" into "this". Types must be the same for this to work!
         * @param that [in] - The object to copy into this.
         * @throws SoftwareViolationException If the types are not of the same kind.
         */
        virtual void Copy(const ContainerBase& that)
        {
            if (this != &that)
            {
                if (typeid(*this) != typeid(that))
                {
                    throw SoftwareViolationException(L"Invalid call to Copy, containers are not of same type",__WFILE__,__LINE__);
                }

                const DictionaryContainer<KeyType, ValueContainerType>& other=static_cast<const DictionaryContainer<KeyT, ValT>& >(that);

                clear();
                m_bIsChanged=other.m_bIsChanged;

                for (const_iterator it=other.begin(); it!=other.end(); ++it)
                {
                    ValueContainerType val;
                    val.Copy(it->second);
                    m_values.insert(std::make_pair(it->first, val));
                }
            }
        }

    private:
        std::map<KeyT, ValT> m_values;
    };
}
}
}
