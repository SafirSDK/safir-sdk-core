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
    };
    //-----------------------------

    template <class T>
    class SequenceContainer
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
            ,m_isChanged(false)
        {
        }

        //implementation of pure virtual in ContainerBase.
        bool IsNull() const
        {
            return m_isNull;
        }

        //implementation of pure virtual in ContainerBase.
        void SetNull()
        {
            m_values.clear();
            m_isNull=true;
            m_isChanged=true;
        }

        bool IsChanged() const
        {
            if (m_isChanged)
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

        void SetChanged(const bool changed)
        {
            if (changed)
            {
                m_isChanged=true;
            }
            else
            {
                m_isChanged=false;
                for (iterator it=begin(); it!=end(); ++it)
                {
                    it->m_isChanged=false;
                }
            }
        }

        iterator begin() {return m_values.begin();}
        iterator end() {return m_values.end();}
        const_iterator begin() const {return m_values.begin();}
        const_iterator end() const {return m_values.end();}
        size_t size() const {return m_values.size();}
        bool empty() const {return m_values.empty();}
        void clear() {m_values.clear();}

        void push_back(const ContainedType& val)
        {
            m_values.push_back(PodType(val));
        }

        const ContainedType& operator[](size_t index) const
        {
            return m_values[index];
        }

        PodType& operator[](size_t index)
        {
            return m_values[index];
        }

    private:
        bool m_isNull;
        bool m_isChanged;
        StorageType m_values;
    };
}
}
}
