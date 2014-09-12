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
#include "ContainerBase.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    //----------------------------------------

    template <class T> class SequenceContainer; //forward declaration
    template <class T>
    class SequencePodType
    {
    public:
        typedef T ContainedType;

        explicit SequencePodType(const T& val)
            :m_val(val)
            ,m_isChanged(false)
        {
        }

        operator ContainedType() const {return m_val;}
        void operator= (const ContainedType& val) {m_val=val; m_isChanged=true;}
        SequencePodType& operator++ () {++m_val;return *this;}
        void operator++ (int) {++m_val;}
        SequencePodType& operator-- () {--m_val;return *this;}
        void operator-- (int) {--m_val;}
        SequencePodType& operator+= (const ContainedType& val) {m_val+=val;return *this;}
        SequencePodType& operator-= (const ContainedType& val) {m_val-=val;return *this;}
        SequencePodType& operator*= (const ContainedType& val) {m_val*=val;return *this;}
        SequencePodType& operator/= (const ContainedType& val) {m_val/=val;return *this;}

    private:
        friend class SequenceContainer<ContainedType>;
        ContainedType m_val;
        bool m_isChanged;

        bool IsChanged() const {return m_isChanged;}
    };
    //-----------------------------

    template <class T>
    class SequenceContainer :
            public ContainerBase,
            private std::vector< SequencePodType<T> >
    {
    public:

        typedef T ContainedType;
        typedef SequencePodType<ContainedType> PodType;
        typedef std::vector<PodType> StorageType;
        typedef typename StorageType::iterator iterator;
        typedef typename StorageType::const_iterator const_iterator;

        /**
         * Default Constructor.
         *
         * Construct a container that is not changed.
         */
        SequenceContainer()
            :m_isNull(false)
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
        using StorageType::operator[];

        void push_back(const ContainedType& val)
        {
            m_bIsChanged=true;
            StorageType::push_back(PodType(val));
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
            return StorageType::insert(position, PodType(val));
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
