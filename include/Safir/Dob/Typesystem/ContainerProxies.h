/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
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

#ifndef __DOTS_CONTAINER_PROXIES_H__
#define __DOTS_CONTAINER_PROXIES_H__

#include <Safir/Dob/Typesystem/ValueContainers.h>
#include <Safir/Dob/Typesystem/ObjectContainer.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    template <class T>
    class ContainerProxy
    {
    public:
        typedef typename T::ContainedType ContainedType;

        explicit ContainerProxy(T& container):m_container(container){}

        ContainerProxy& operator=(const ContainerProxy& other)
        {
            if (other.IsNull())
            {
                m_container.SetNull();
            }
            else
            {
                m_container.SetVal(other.GetVal());
            }
            return *this;
        }

        ContainerProxy& operator=(const ContainedType other) {m_container.SetVal(other);return *this;}

        operator const ContainedType() const {return m_container.GetVal();}

        bool IsNull() const {return m_container.IsNull();}
        void SetNull() {m_container.SetNull();}
        bool IsChanged() const {return m_container.IsChanged();}
        void SetChanged(const bool changed) {m_container.SetChanged(changed);}


        void SetVal(const ContainedType value)  {m_container.SetVal(value);}

        const ContainedType GetVal() const {return m_container.GetVal();}

        void SetOrdinal(const EnumerationValue value){m_container.SetOrdinal(value);}
        EnumerationValue GetOrdinal() const {return m_container.GetOrdinal();}

        void Copy (const ContainerProxy& that) {m_container.Copy(that.m_container);}

        ContainerProxy& operator++ ()    // prefix ++
        {
            ContainedType val = GetVal();
            ++val;
            SetVal(val);
            return *this;
        }

        //Does not return anything since I'm not sure what it should return.
        void operator++ (int) // postfix ++
        {
            ++(*this);
        }

        ContainerProxy& operator-- ()    // prefix --
        {
            ContainedType val = GetVal();
            --val;
            SetVal(val);
            return *this;
        }

        //Does not return anything since I'm not sure what it should return.
        void operator-- (int) // postfix --
        {
            --(*this);
        }


        ContainerProxy& operator+= (const ContainedType& val)
        {
            SetVal(GetVal() + val);
            return *this;
        }

        ContainerProxy& operator-= (const ContainedType& val)
        {
            SetVal(GetVal() - val);
            return *this;
        }

        ContainerProxy& operator*= (const ContainedType& val)
        {
            SetVal(GetVal() * val);
            return *this;
        }

        ContainerProxy& operator/= (const ContainedType& val)
        {
            SetVal(GetVal() / val);
            return *this;
        }

        const T& GetContainer() const {return m_container;}
        T& GetContainer() {return m_container;}
    private:
        T& m_container;
    };

    static inline bool operator==(const ContainerProxy<ChannelIdContainer>& first, const ChannelId& second)
    {return second == first;}
    static inline bool operator!=(const ContainerProxy<ChannelIdContainer>& first, const ChannelId& second)
    {return second != first;}
    
    static inline bool operator==(const ContainerProxy<HandlerIdContainer>& first, const HandlerId& second)
    {return second == first;}
    static inline bool operator!=(const ContainerProxy<HandlerIdContainer>& first, const HandlerId& second)
    {return second != first;}

    static inline bool operator==(const ContainerProxy<InstanceIdContainer>& first, const InstanceId& second)
    {return second == first;}
    static inline bool operator!=(const ContainerProxy<InstanceIdContainer>& first, const InstanceId& second)
    {return second != first;}

    static inline bool operator==(const ContainerProxy<EntityIdContainer>& first, const EntityId& second)
    {return second == first;}
    static inline bool operator!=(const ContainerProxy<EntityIdContainer>& first, const EntityId& second)
    {return second != first;}


    template <>
    class ContainerProxy<StringContainer>
    {
    public:
        typedef StringContainer::ContainedType ContainedType;

        explicit ContainerProxy(StringContainer& container):m_container(container){}

        ContainerProxy& operator=(const ContainerProxy& other)
        {
            if (other.IsNull())
            {
                m_container.SetNull();
            }
            else
            {
                m_container.SetVal(other.GetVal());
            }
            return *this;
        }

        ContainerProxy& operator=(const ContainedType& other) {m_container.SetVal(other);return *this;}

        operator const ContainedType& () const {return m_container.GetVal();}

        bool IsNull() const {return m_container.IsNull();}
        void SetNull() {m_container.SetNull();}
        bool IsChanged() const {return m_container.IsChanged();}
        void SetChanged(const bool changed) {m_container.SetChanged(changed);}


        void SetVal(const ContainedType& value)  {m_container.SetVal(value);}

        const ContainedType& GetVal() const {return m_container.GetVal();}

        Int32 Utf8StringLength() const {return m_container.Utf8StringLength();}
        const std::string & Utf8String() const {return m_container.Utf8String();}

        void Copy (const ContainerProxy& that) {m_container.Copy(that.m_container);}

        ContainerProxy& operator+= (const ContainedType& val)
        {
            SetVal(GetVal() + val);
            return *this;
        }

        bool operator== (const ContainedType& val) const
        {
            return m_container.GetVal() == val;
        }

        bool operator!= (const ContainedType& val) const
        {
            return m_container.GetVal() != val;
        }

        const StringContainer& GetContainer() const {return m_container;}
        StringContainer& GetContainer() {return m_container;}
    private:
        StringContainer& m_container;
    };

    
    static inline bool operator==(const std::wstring& first, const ContainerProxy<StringContainer>& second)
    {return second == first;}
    static inline bool operator!=(const std::wstring& first, const ContainerProxy<StringContainer>& second)
    {return second != first;}


    template <>
    class ContainerProxy<BinaryContainer>
    {
    public:
        typedef BinaryContainer::ContainedType ContainedType;

        explicit ContainerProxy(BinaryContainer& container):m_container(container){}

        ContainerProxy& operator=(const ContainerProxy& other)
        {
            if (other.IsNull())
            {
                m_container.SetNull();
            }
            else
            {
                m_container.SetVal(other.GetVal());
            }
            return *this;
        }

        ContainerProxy& operator=(const ContainedType& other) {m_container.SetVal(other);return *this;}

        operator const ContainedType& () const {return m_container.GetVal();}

        bool IsNull() const {return m_container.IsNull();}
        void SetNull() {m_container.SetNull();}
        bool IsChanged() const {return m_container.IsChanged();}
        void SetChanged(const bool changed) {m_container.SetChanged(changed);}


        void SetVal(const ContainedType& value)  {m_container.SetVal(value);}

        const ContainedType& GetVal() const {return m_container.GetVal();}
        void Copy (const ContainerProxy& that) {m_container.Copy(that.m_container);}

        bool operator== (const ContainedType& val) const
        {
            return m_container.GetVal() == val;
        }

        bool operator!= (const ContainedType& val) const
        {
            return m_container.GetVal() != val;
        }

        const BinaryContainer& GetContainer() const {return m_container;}
        BinaryContainer& GetContainer() {return m_container;}
    private:
        BinaryContainer& m_container;
    };

    static inline bool operator==(const Binary& first, const ContainerProxy<BinaryContainer>& second)
    {return second == first;}
    static inline bool operator!=(const Binary& first, const ContainerProxy<BinaryContainer>& second)
    {return second != first;}


    template <class U>
    class ContainerProxy<ObjectContainerImpl<U> >
    {
    public:
        typedef typename ObjectContainerImpl<U>::T_Ptr ContainedType;

        explicit ContainerProxy(ObjectContainerImpl<U>& container):m_container(container){}

        ContainerProxy& operator=(const ContainerProxy& other)
        {
            if (other.IsNull())
            {
                m_container.SetNull();
            }
            else
            {
                m_container.SetPtr(other);
            }
            return *this;
        }

        ContainerProxy& operator=(const ContainedType& other) {m_container.SetPtr(other);return *this;}

        operator const ContainedType () const {return m_container.GetPtr();}

        U* const operator->() const
        { return m_container.operator->(); }

        bool IsNull() const {return m_container.IsNull();}
        void SetNull() {m_container.SetNull();}
        bool IsChanged() const {return m_container.IsChanged();}
        void SetChanged(const bool changed) {m_container.SetChanged(changed);}

        void SetChangedHere(const bool changed) {m_container.SetChangedHere(changed);}
        bool IsChangedHere() const {return m_container.IsChangedHere();}

        void SetPtr(const ContainedType& ptr)  {m_container.SetPtr(ptr);}
        void SetPtr(const ObjectPtr& ptr)      {m_container.SetPtr(ptr);}

        const ContainedType& GetPtr() const {return m_container.GetPtr();}
        void Copy (const ContainerProxy& that) {m_container.Copy(that.m_container);}

        const ObjectContainerImpl<U>& GetContainer() const {return m_container;}
        ObjectContainerImpl<U>& GetContainer() {return m_container;}
    private:
        ObjectContainerImpl<U>& m_container;
    };


    template <>
    class ContainerProxy<ObjectContainerImpl<Object> >
    {
    public:
        typedef ObjectContainerImpl<Object>::T_Ptr ContainedType;

        explicit ContainerProxy(ObjectContainerImpl<Object>& container):m_container(container){}

        ContainerProxy& operator=(const ContainerProxy& other)
        {
            if (other.IsNull())
            {
                m_container.SetNull();
            }
            else
            {
                m_container.SetPtr(other);
            }
            return *this;
        }

        ContainerProxy& operator=(const ContainedType& other) {m_container.SetPtr(other);return *this;}

        operator const ContainedType () const {return m_container.GetPtr();}

        Object* const operator->() const
        { return m_container.operator->(); }

        bool IsNull() const {return m_container.IsNull();}
        void SetNull() {m_container.SetNull();}
        bool IsChanged() const {return m_container.IsChanged();}
        void SetChanged(const bool changed) {m_container.SetChanged(changed);}

        void SetChangedHere(const bool changed) {m_container.SetChangedHere(changed);}
        bool IsChangedHere() const {return m_container.IsChangedHere();}

        void SetPtr(const ObjectPtr& ptr)      {m_container.SetPtr(ptr);}

        const ContainedType& GetPtr() const {return m_container.GetPtr();}

        void Copy (const ContainerProxy& that) {m_container.Copy(that.m_container);}

        const ObjectContainerImpl<Object>& GetContainer() const {return m_container;}
        ObjectContainerImpl<Object>& GetContainer() {return m_container;}
    private:
        ObjectContainerImpl<Object>& m_container;
    };

}
}
}

#endif

