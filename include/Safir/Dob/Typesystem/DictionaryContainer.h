/******************************************************************************
*
* Copyright Saab AB, 2004-2015, 2022 (http://safirsdkcore.com)
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

#include <Safir/Dob/Typesystem/ContainerBase.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/ObjectContainer.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <memory>
#include <map>
#include <stdexcept>
#include <typeinfo>
#include <vector>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    /**
     * Base class for all dictionary containers.
     * The reason for the existence of this class is that code that uses the reflection
     * functionality must be able to get hold of members of items.
     */
    class DictionaryContainerBase : public ContainerBase
    {
    public:
        /** Default Constructor. */
        DictionaryContainerBase()
            :ContainerBase()
        {
        }

        /**
         * Is the change flag in the container set?
         *
         * This method is like IsChanged without the recursion.
         *
         * @return True if the containers change flag is set.
         */
        bool IsChangedHere() const
        {
            return m_bIsChanged;
        }

        /**
         * Set the change flag in the container.
         *
         * This method is like SetChanged without the recursion
         *
         * @param changed [in] - The value to set the change flag to.
         */
        void SetChangedHere(const bool changed)
        {
            m_bIsChanged = changed;
        }

        /**
         * Get the size of the dictionary, i.e number of contained keys.
         *
         * @return The number of values in the dictionary.
         */
        virtual size_t size() const = 0;

        /**
         * Check if dictionary is empty.
         *
         * @return True if dictionary is empty, else false.
         */
        virtual bool empty() const = 0;

        /**
         * @name Reflection part.
         * These methods allow applications to manipulate the members of objects
         * without having been compiled against it.
         * There should be no reason for most applications to use these methods.
         */
        /** @{ */

        /**
         * Get the key at a particular position in the dictionary.
         *
         * Note that the order of keys in the dictionary is not guaranteed.
         * This is not a particularly "cheap" way of accessing the contents of a
         * dictionary. Much better to use the iterators in the implementing class.
         *
         * This function needs to be called like this: dict.GetKeyAt<Int32>(10).
         * If the dictionary does not have KeyT as its key type the behaviour is undefined.
         *
         * For enumeration values, use EnumerationValue as the type, and you will get the
         * ordinal value.
         *
         * @param index an index between 0 and size().
         * @return The key at a position in the dictionary.
         */
        template<class KeyT>
        const KeyT& GetKeyAt(const size_t index) const
        {
            return *static_cast<const KeyT*>(GetKeyAtInternal(index));
        }

        /**
         * Get the container of the value at a particular position in the dictionary.
         *
         * Note that the order of keys in the dictionary is not guaranteed.
         * This is not a particularly "cheap" way of accessing the contents of a
         * dictionary. Much better to use the iterators in the implementing class.
         *
         * @param index an index between 0 and size().
         * @return The container at a position in the dictionary.
         */
        virtual ContainerBase& GetValueContainerAt(const size_t index) = 0;

        /** Const version of GetValueContainerAt() */
        virtual const ContainerBase& GetValueContainerAt(const size_t index) const = 0;

        /** @} */

    protected:
        virtual const void* GetKeyAtInternal(const size_t index) const = 0;

    private:
        friend void Utilities::MergeChanges(ObjectPtr into, const ObjectConstPtr& from);

        /**
         * Function needed by Utilities::MergeChanges to be able to merge
         * dictionaries. Will in turn call Utilities::MergeChanges recursively
         * if it needs to merge objects.
         */
        virtual void Merge(const DictionaryContainerBase& other) = 0;
    };

    /**
     * Container class for dictionaries of key value pairs. A dictionary is a collection of values that can dynamically
     * grow or shrink in size. The whole container has a change flag that will automatically
     * be set when values are added, removed or changed.
     */
    template <class KeyT, class ValT>
    class DictionaryContainer : public DictionaryContainerBase
    {
    public:

        typedef KeyT KeyType;
        typedef ValT ValueContainerType;
        typedef typename ValueContainerType::ContainedType ContainedType;
        typedef std::map<KeyType, ValueContainerType> StorageType;
        typedef typename StorageType::const_iterator const_iterator;
        typedef typename StorageType::iterator iterator;
        typedef typename StorageType::value_type value_type;

        /**
         * Default Constructor.
         *
         * Construct a container that is not changed and not null.
         */
        DictionaryContainer()
            : DictionaryContainerBase()
            , m_values()
        {
        }

        bool IsNull() const override {return empty();}

        void SetNull() override
        {
            clear();
        }

        iterator begin() {return m_values.begin();}
        const_iterator begin() const {return m_values.begin();}

        iterator end() {return m_values.end();}
        const_iterator end() const {return m_values.end();}

        iterator find(const KeyType& key) {return m_values.find(key);}
        const_iterator find(const KeyType& key) const {return m_values.find(key);}

        size_t size() const override {return m_values.size();}

        bool empty() const override {return m_values.empty();}

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
                return ct;
            }
        }

        /**
         * @brief Like operator[], but throws std::out_of_range if key is not in dictionary
         */
        ValueContainerType& at(const KeyType& key)
        {
            iterator it=m_values.find(key);
            if (it!=m_values.end())
            {
                return it->second;
            }
            else
            {
                throw std::out_of_range("Key is not in dictionary");
            }
        }

        /**
         * @brief Like operator[], but throws std::out_of_range if key is not in dictionary.
         * Const version
         */
        const ValueContainerType& at(const KeyType& key) const
        {
            return const_cast<DictionaryContainer&>(*this).at(key);
        }

        /**
         * @brief IsChanged - Check if the dictionary has changed.
         * @return True if changed, else false.
         */
        bool IsChanged() const override
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
         * @brief SetChanged - Set the change state of the dictionary.
         * @param changed [in] - If true, the dictionary is set to changed, it is set to not changed.
         */
        void SetChanged(const bool changed) override
        {
            m_bIsChanged=changed;
            for (iterator it=m_values.begin(); it!=m_values.end(); ++it)
            {
                it->second.SetChanged(changed);
            }
        }


        /**
         * @brief clear - Clear the dictionary, i.e remove all keys/values. After a call to clear
         * the dictionary will be empty and hence it will be null too.
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
            InsertHelper<ContainedType, ValueContainerType>::SetVal(val, container);
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
         *
         * Note: if types are not compatible the behaviour is undefined.
         */
        void Copy(const ContainerBase& that) override
        {
            if (this != &that)
            {
                const DictionaryContainer<KeyT, ValT>& other = Cast(that);

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

        /**
         * @name Reflection part.
         * These methods allow applications to manipulate the members of objects
         * without having been compiled against it.
         * There should be no reason for most applications to use these methods.
         */
        /** @{ */
        const ContainerBase& GetValueContainerAt(const size_t index) const override
        {
            return const_cast<DictionaryContainer*>(this)->GetValueContainerAt(index);
        }

        ContainerBase& GetValueContainerAt(const size_t index) override
        {
            if (index >= size())
            {
                throw SoftwareViolationException
                    (L"DictionaryContainer::GetKey: Index outside range!", __WFILE__, __LINE__);
            }
            return std::next(m_values.begin(),index)->second;
        }
        /** @} */

        
    private:

        void Merge(const DictionaryContainerBase& that) override
        {
            const DictionaryContainer<KeyT, ValT>& other = Cast(that);

            for (const_iterator it = other.begin(); it != other.end(); ++it)
            {
                const ObjectContainerBase* fromContainerOB =
                    dynamic_cast<const ObjectContainerBase*>(&it->second);
                //is it an object member?
                if (fromContainerOB != NULL)
                {
                    if (fromContainerOB->IsChangedHere())
                    {
                        iterator findIt = find(it->first);
                        if (findIt == end())
                        {
                            throw SoftwareViolationException
                                (L"DictionaryContainer::Merge: Changed key not found in target!",
                                 __WFILE__, __LINE__);
                        }

                        findIt->second.Copy(*fromContainerOB);
                    }
                    else if (fromContainerOB->IsChanged())
                    {
                        iterator findIt = find(it->first);
                        if (findIt == end())
                        {
                            throw SoftwareViolationException
                                (L"DictionaryContainer::Merge: Changed key not found in target!",
                                 __WFILE__, __LINE__);
                        }
                        ObjectContainerBase* intoContainerOB = dynamic_cast<ObjectContainerBase*>(&findIt->second);

                        ObjectPtr into = intoContainerOB->GetObjectPointer();
                        ObjectConstPtr from = fromContainerOB->GetObjectPointer();

                        //recurse
                        Utilities::MergeChanges(into,from);
                    }
                }
                else
                {
                    if (it->second.IsChanged())
                    {
                        iterator findIt = find(it->first);
                        if (findIt == end())
                        {
                            throw SoftwareViolationException
                                (L"DictionaryContainer::Merge: Changed key not found in target!",
                                 __WFILE__, __LINE__);
                        }

                        findIt->second.Copy(it->second);
                    }
                }
            }
        }

        const void* GetKeyAtInternal(const size_t index) const override
        {
            if (index >= size())
            {
                throw SoftwareViolationException
                    (L"DictionaryContainer::GetKeyAt: Index outside range!", __WFILE__, __LINE__);
            }
            return &(std::next(m_values.begin(),index)->first);
        }


        std::map<KeyT, ValT> m_values;

        template <class V, class C> struct InsertHelper
        {
            static void SetVal(const V& v, C& c) {c.SetVal(v);}
        };

        template <class V, class C> struct InsertHelper< std::shared_ptr<V>, C >
        {
            static void SetVal(const std::shared_ptr<V>& v, C& c) {c.SetPtr(v);}
        };

        static DictionaryContainer<KeyT,ValT>& Cast(ContainerBase& base)
        {
#ifndef NDEBUG
            if (typeid(DictionaryContainer<KeyT,ValT>) != typeid(base))
            {
                throw SoftwareViolationException(L"Invalid call to Copy or Merge, containers are not of same type",
                                                 __WFILE__,__LINE__);
            }
#endif
            return static_cast<DictionaryContainer<KeyT, ValT>&>(base);
        }

        static const DictionaryContainer<KeyT,ValT>& Cast(const ContainerBase& base)
        {
            return Cast(const_cast<ContainerBase&>(base));
        }
    };
}
}
}
