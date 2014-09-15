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
    //----------------------------------------

    template <class T> class SequenceContainer; //forward declaration
    template <class T>
    class SequenceItemProxy
    {
    public:
        typedef T ContainedType;

        explicit SequenceItemProxy(const T& val)
            :m_val(val)
            ,m_isChanged(false)
        {
        }

        void SetVal(const ContainedType& val)  {m_val=val; m_isChanged=true;}
        const ContainedType GetVal() const {return m_val;}
        operator ContainedType() const {return m_val;}
        void operator= (const ContainedType& val) {SetVal(val);}
        SequenceItemProxy& operator++ () {++m_val;return *this;}
        void operator++ (int) {++m_val;}
        SequenceItemProxy& operator-- () {--m_val;return *this;}
        void operator-- (int) {--m_val;}
        SequenceItemProxy& operator+= (const ContainedType& val) {m_val+=val;return *this;}
        SequenceItemProxy& operator-= (const ContainedType& val) {m_val-=val;return *this;}
        SequenceItemProxy& operator*= (const ContainedType& val) {m_val*=val;return *this;}
        SequenceItemProxy& operator/= (const ContainedType& val) {m_val/=val;return *this;}

    private:
        friend class SequenceContainer<ContainedType>;
        ContainedType m_val;
        bool m_isChanged;

        bool IsChanged() const {return m_isChanged;}
    };

    inline bool operator==(const SequenceItemProxy<ChannelId>& first, const ChannelId& second)
    {return second == first;}
    inline bool operator!=(const SequenceItemProxy<ChannelId>& first, const ChannelId& second)
    {return second != first;}

    inline bool operator==(const SequenceItemProxy<HandlerId>& first, const HandlerId& second)
    {return second == first;}
    inline bool operator!=(const SequenceItemProxy<HandlerId>& first, const HandlerId& second)
    {return second != first;}

    inline bool operator==(const SequenceItemProxy<InstanceId>& first, const InstanceId& second)
    {return second == first;}
    inline bool operator!=(const SequenceItemProxy<InstanceId>& first, const InstanceId& second)
    {return second != first;}

    inline bool operator==(const SequenceItemProxy<EntityId>& first, const EntityId& second)
    {return second == first;}
    inline bool operator!=(const SequenceItemProxy<EntityId>& first, const EntityId& second)
    {return second != first;}

    //-----------------------------

    template <class T>
    class SequenceContainer :
            public ContainerBase,
            private std::vector< SequenceItemProxy<T> >
    {
    public:

        typedef T ContainedType;
        typedef SequenceItemProxy<ContainedType> Item;
        typedef std::vector<Item> StorageType;
        typedef typename StorageType::iterator iterator;
        typedef typename StorageType::const_iterator const_iterator;

        /**
         * Default Constructor.
         *
         * Construct a container that is not changed.
         */
        SequenceContainer()
            :m_isNull(true)
        {
        }

        //implementation of pure virtual in ContainerBase.
        virtual bool IsNull() const
        {
            return m_isNull;
        }

        //implementation of pure virtual in ContainerBase.
        virtual void SetNull()
        {
            clear();
            m_isNull=true;
            m_bIsChanged=true;
        }

        virtual bool IsChanged() const
        {
            if (m_bIsChanged)
            {
                return true;
            }

            for (const_iterator it=begin(); it!=end(); ++it)
            {
                if (it->m_isChanged)
                    return true;
            }

            return false;
        }

        virtual void SetChanged(const bool changed)
        {
            m_bIsChanged=changed;

            if (!changed)
            {
                for (iterator it=begin(); it!=end(); ++it)
                {
                    it->m_isChanged=false;
                }
            }
        }

        using StorageType::front;
        using StorageType::back;
        using StorageType::size;
        using StorageType::clear;
        using StorageType::empty;
        using StorageType::begin;
        using StorageType::end;
        using StorageType::operator [];

        void push_back(const ContainedType& val)
        {
            m_isNull=false;
            m_bIsChanged=true;
            StorageType::push_back(Item(val));
        }

        iterator erase(iterator first, iterator last)
        {
            m_bIsChanged=true;
            return StorageType::erase(first, last);
        }

        iterator erase(iterator position)
        {
            m_bIsChanged=true;
            return StorageType::erase(position);
        }

        void erase_at(size_t index)
        {
            erase(begin()+index);
        }

        iterator insert(iterator position, const ContainedType& val)
        {
            m_bIsChanged=true;
            return StorageType::insert(position, Item(val));
        }

        void insert_at(size_t index, const ContainedType& value)
        {
            insert(begin()+index, value);
        }

        virtual void Copy(const ContainerBase& /*that*/)
        {

        }

    private:
        bool m_isNull;
    };
}
}
}
