/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#ifndef __DOTS_OBJECT_CONTAINER_H__
#define __DOTS_OBJECT_CONTAINER_H__


#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/ContainerBase.h>
#include <Safir/Dob/Typesystem/Object.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <typeinfo>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    /**
     * Base class for all object containers.
     * The reason for the existence of this class is that code that uses the reflection
     * functionality must be able to get hold of members of items.
     */
    class ObjectContainerBase: public ContainerBase
    {
    public:
        /** Default constructor. */
        ObjectContainerBase():ContainerBase() {}

        /**
         * Set the smart pointer in the container.
         *
         * This method will set the contained pointer to point to another
         * object. Checks are always made to see that it is of the correct type.
         * The change flag of the container will be updated.
         *
         * @param ptr [in] A pointer to the new object to point to.
         * @throws IncompatibleTypesException If the ptr is not of the type contained by the container.
         */
        virtual void SetPtr(const ObjectPtr & ptr) = 0;

        /**
         * Is the change flag in the container set?
         *
         * This method is like IsChanged without the recursion (on object containers IsChanged is recursive).
         *
         * @return True if the containers change flag is set.
         */
        bool IsChangedHere() const {return m_bIsChanged;}

        /**
         * Set the change flag in the container.
         *
         * This method is like SetChanged without the recursion (on object containers SetChanged is recursive).
         *
         * @param changed [in] - The value to set the change flag to.
         */
        void SetChangedHere(const bool changed) {m_bIsChanged = changed;}

        /**
         * @name Reflection part.
         * These methods allow applications to manipulate the members of objects
         * without having been compiled against it.
         * There should be no reason for most applications to use these methods.
         */
        /** @{ */

        /**
         * Get a reference to a member container from an object.
         *
         * Use the methods in Members to get member indices and array sizes for use
         * with this method.
         *
         * Note: Do not use this method unless you're very sure it is the one you need!
         *
         * @param member [in] - The index of the member to get.
         * @param index [in] - The array index of the member to get.
         * @return A reference to the member container.
         * @throws IllegalValueException If the index is not in the range of the array.
         * @throws SoftwareViolationException If the element is not an array and the index is not 0.
         */
        virtual       ContainerBase & GetMember(const int member, const int index)       = 0;

        /**
         * Get a const reference to a member container from an object.
         *
         * Use the methods in Members to get member indices and array sizes for use
         * with this method.
         *
         * Note: Do not use this method unless you're very sure it is the one you need!
         *
         * @param member [in] - The index of the member to get.
         * @param index [in] - The array index of the member to get.
         * @return A const reference to the member container.
         * @throws IllegalValueException If the index is not in the range of the array.
         * @throws SoftwareViolationException If the element is not an array and the index is not 0.
         */
        virtual const ContainerBase & GetMember(const int member, const int index) const = 0;

        /**
         * Get a smart pointer to the contained object.
         *
         * This method will cast the smart object pointer to the derived type
         * contained by the container to a smart pointer to an Object (the DOB
         * object base class.
         *
         * This method does not check if the container is null!
         *
         * Note: Do not use this method unless you're very sure it is the one you need!
         *
         * @return A smart pointer to the contained object.
        */
        virtual const ObjectPtr GetObjectPointer() const = 0;

        /**
         * Set the smart pointer in the container.
         *
         * This method will set the contained pointer to point to another
         * object. Checks are NOT always made to see that it is of the correct type.
         *
         * Warning: This method does not update the change flag!
         *
         * Note: Do not use this method unless you're very sure it is the one you need!
         *
         * @param ptr [in] A pointer to the new object to point to.
        */
        virtual void SetObjectPointer(const ObjectPtr ptr) = 0;

        /**
         * Reset (ie set to null) the contained pointer.
         * Warning: This method does not update the change flag!
         *
         * Note: Do not use this method unless you're very sure it is the one you need!
        */
        virtual void ResetObjectPointer() = 0;


        /** @} */
    protected:
        /**
         * Copy assignment operator.
         *
         * @param other [in] - The object to copy.
         * @return A reference to this.
         */
        ObjectContainerBase & operator=(const ObjectContainerBase & other)
        {ContainerBase::operator =(other); return *this;}
    };

    /**
     * Template class for all containers of automatically generated DOB objects.
     *
     * This class holds a smart pointer to an object, and has operations
     * to get information from it and modify it.
     * The -> operator is overloaded to make this class more transparent to use.
     *
     * It is called ObjectContainerImpl because the name ObjectContainer is "taken"
     * by the container that contains a Dob::Typesystem::Object object.
     *
     * @param T The type to contain. Must inherit from Dob::Typesystem::Object.
     */
    template <class T>
    class ObjectContainerImpl : public ObjectContainerBase
    {
    public:
        /** Typedef for the contained smart pointer. */
        typedef boost::shared_ptr<T> T_Ptr;

        /**
         * Default constructor.
         *
         * Creates a null and not changed container.
         */
        ObjectContainerImpl():ObjectContainerBase() {}

        /**
         * Copy constructor.
         *
         * Copy an object container. The contained object will be cloned.
         *
         * @param other [in] - The object to copy.
         * @throws IncompatibleTypesException Something has gone horribly wrong with your copying!
         */
        ObjectContainerImpl(const ObjectContainerImpl & other):
            ObjectContainerBase(other) //make sure we use copy constructor of parent
        {
            if (!other.IsNull())
            {
                m_pObject = boost::dynamic_pointer_cast<T>(other.m_pObject->Clone());
                if (m_pObject == NULL)
                {
                    throw IncompatibleTypesException(L"The types are not compatible!",__WFILE__,__LINE__);
                }
            }
        }

        /**
         * Copy assignment operator.
         *
         * @param other [in] - The object to copy.
         * @return A reference to this.
         * @throws IncompatibleTypesException The contained types are not compatible!
         */
        ObjectContainerImpl & operator=(const ObjectContainerImpl & other)
        {
            ObjectContainerBase::operator=(other);//make sure we use copy assignment of parent
            if (other.IsNull())
            {
                m_pObject.reset();
            }
            else
            {
                m_pObject = boost::dynamic_pointer_cast<T>(other.m_pObject->Clone());
                if (m_pObject == NULL)
                {
                    throw IncompatibleTypesException(L"The types are not compatible!",__WFILE__,__LINE__);
                }
            }
            return *this;
        }

        /**
         * Set the smart pointer in the container.
         *
         * This method will set the contained pointer to point to another
         * object.
         * The change flag of the container will be updated.
         *
         * @param ptr [in] A pointer to the new object to point to.
        */
        void SetPtr(const T_Ptr & ptr)
        {
            m_bIsChanged = true;
            m_pObject = ptr;
        }

        //implementation of pure virtual method.
        virtual void SetPtr(const ObjectPtr & ptr)
        {
            m_bIsChanged = true;
            m_pObject = boost::dynamic_pointer_cast<T>(ptr);
            if (m_pObject == NULL)
            {
                throw IncompatibleTypesException(L"The types are not compatible!",__WFILE__,__LINE__);
            }
        }

        /**
         * Get the smart pointer from the container.
         *
         * This method will return the contained smart pointer unless the container is null, then
         * an exception will be thrown.
         *
         * @return A smart pointer to the contained object.
         * @throws NullException The container is null.
        */
        const T_Ptr & GetPtr() const {if (IsNull()) throw NullException(L"Object is null",__WFILE__,__LINE__); return m_pObject;}

        /**
         * Dereference the smart pointer in the container.
         *
         * This method will call the -> operator on the smart pointer in the container to allow
         * users to directly access members in the object.
         *
         * @return A pointer to the contained object.
         * @throws NullException The container is null.
        */
        T * const operator->() const
        { if (IsNull()) throw NullException(L"Object is null",__WFILE__,__LINE__); return m_pObject.operator->(); }


        //Override of inherited method. Parent comment describes this behaviour too..
        virtual void SetChanged(const bool changed) {m_bIsChanged = changed; if (!IsNull()) m_pObject->SetChanged(changed);}

        //Override of inherited method. Parent comment describes this behaviour too..
        virtual bool IsChanged() const {return m_bIsChanged || (!IsNull() && m_pObject->IsChanged());}

        //override of pure virtual method
        virtual bool IsNull() const {return m_pObject == NULL;}

        //override of pure virtual method
        virtual void SetNull()
        {
            m_bIsChanged = true;
            m_pObject.reset();
        }

        //implementation of pure virtual in ContainerBase.
        virtual void Copy(const ContainerBase & that)
        {
            if (this != &that)
            {
                if (typeid(*this) != typeid(that))
                {
                    throw SoftwareViolationException(L"Invalid call to Copy, containers are not of same type",__WFILE__,__LINE__);
                }
                const ObjectContainerImpl<T> & castedThat = static_cast<const ObjectContainerImpl<T> &>(that);
                m_bIsChanged = castedThat.m_bIsChanged;
                if (that.IsNull())
                {
                    m_pObject.reset();
                }
                else
                {
                    SetObjectPointer(castedThat.m_pObject->Clone());
                }
            }
        }
        //Reflection part (Don't use unless you really know what you're doing!!)
        //Comments are in ObjectContainerBase.
        virtual       ContainerBase & GetMember(const int member, const int index)
        {if (IsNull()) throw NullException(L"Object is null",__WFILE__,__LINE__); return m_pObject->GetMember(member,index);}
        virtual const ContainerBase & GetMember(const int member, const int index) const
        {if (IsNull()) throw NullException(L"Object is null",__WFILE__,__LINE__); return m_pObject->GetMember(member,index);}


        /**
         * @name Blob serialization/deserialization part.
         * These functions are for internal use only!
         * Their names and functionality are likely to change in the near future!
         */
        /** @{ */

        /**
         * Calculate the size of the blob-serialized form of the contained object.
         *
         * @return The needed size in bytes. 0 if the container is null.
         */
        Int32 CalculateBlobSize() const {if (IsNull()) return 0; else return m_pObject->CalculateBlobSize();}

        /** @} */

        virtual const ObjectPtr GetObjectPointer() const {return boost::static_pointer_cast<Object>(m_pObject);}
        virtual void SetObjectPointer(const ObjectPtr ptr)
        {
#ifndef NDEBUG
            m_pObject = boost::dynamic_pointer_cast<T>(ptr);
            if(m_pObject == NULL)
            {
                throw SoftwareViolationException(L"Failed to cast object pointer to expected type",__WFILE__,__LINE__);
            }
#else
            m_pObject = boost::static_pointer_cast<T>(ptr);
#endif
        }
        virtual void ResetObjectPointer(){m_pObject.reset();}
    private:

        T_Ptr m_pObject;
    };

    /**
     * This is a specialization of the ObjectContainerImpl template for the case where the
     * template argument is Dob::Typesystem::Object.
     * The reason that this is needed is that for example the SetPtr function will be multiply defined
     * if the ObjectContainerImpl is instantiated with Object.
     */
    template <>
    class ObjectContainerImpl<Object> : public ObjectContainerBase
    {
    public:
        /** Typedef for the contained smart pointer. */
        typedef boost::shared_ptr<Object> T_Ptr;

        /**
         * Default constructor.
         *
         * Creates a null and not changed container.
         */
        ObjectContainerImpl():ObjectContainerBase() {}


        /**
         * Copy constructor.
         *
         * Copy an object container. The contained object will be cloned.
         *
         * @param other [in] - The object to copy.
         */
        ObjectContainerImpl(const ObjectContainerImpl & other):
            ObjectContainerBase(other) //make sure we use copy constructor of parent
        {
            if (!other.IsNull())
            {
                m_pObject = other.m_pObject->Clone();
            }
        }

        /**
         * Copy assignment operator.
         *
         * @param other [in] - The object to copy.
         * @return A reference to this.
         */
        ObjectContainerImpl & operator=(const ObjectContainerImpl & other)
        {
            ObjectContainerBase::operator =(other);//make sure we use copy assignment of parent
            if (other.IsNull())
            {
                m_pObject.reset();
            }
            else
            {
                m_pObject = other.m_pObject->Clone();
            }
            return *this;
        }


        //implementation of pure virtual method.
        virtual void SetPtr(const ObjectPtr & ptr)
        {
            m_bIsChanged = true;
            m_pObject = ptr;
        }

        /**
         * Get the smart pointer from the container.
         *
         * This method will return the contained smart pointer unless the container is null, then
         * an exception will be thrown.
         *
         * @return A smart pointer to the contained object.
         * @throws NullException The container is null.
        */
        const T_Ptr & GetPtr() const {if (IsNull()) throw NullException(L"Object is null",__WFILE__,__LINE__); return m_pObject;}

        /**
         * Dereference the smart pointer in the container.
         *
         * This method will call the -> operator on the smart pointer in the container to allow
         * users to directly access members in the object.
         *
         * @return A pointer to the contained object.
         * @throws NullException The container is null.
        */
        Object * const operator->() const
        {
            if (IsNull()) throw NullException(L"Object is null",__WFILE__,__LINE__); return m_pObject.operator->();
        }

        //Override of inherited method. Parent comment describes this behaviour too..
        virtual void SetChanged(const bool changed) {m_bIsChanged = changed; if (!IsNull()) m_pObject->SetChanged(changed);}

        //Override of inherited method. Parent comment describes this behaviour too..
        virtual bool IsChanged() const {return m_bIsChanged || (!IsNull() && m_pObject->IsChanged());}

        //override of pure virtual method
        virtual bool IsNull() const {return m_pObject == NULL;}

        //override of pure virtual method
        virtual void SetNull()
        {
            m_bIsChanged = true;
            m_pObject.reset();
        }

        //implementation of pure virtual in ContainerBase.
        virtual void Copy(const ContainerBase & that)
        {
            if (this != &that)
            {
                if (typeid(*this) != typeid(that))
                {
                    throw SoftwareViolationException(L"Invalid call to Copy, containers are not of same type",__WFILE__,__LINE__);
                }
                const ObjectContainerImpl<Object> & castedThat = static_cast<const ObjectContainerImpl<Object> &>(that);
                m_bIsChanged = castedThat.m_bIsChanged;
                if (that.IsNull())
                {
                    m_pObject.reset();
                }
                else
                {
                    SetObjectPointer(castedThat.m_pObject->Clone());
                }
            }
        }

        //Reflection part (Don't use unless you really know what you're doing!!)
        //Comments are in ObjectContainerBase.
        virtual       ContainerBase & GetMember(const int member, const int index)
        {if (IsNull()) throw NullException(L"Object is null",__WFILE__,__LINE__); return m_pObject->GetMember(member,index);}
        virtual const ContainerBase & GetMember(const int member, const int index) const
        {if (IsNull()) throw NullException(L"Object is null",__WFILE__,__LINE__); return m_pObject->GetMember(member,index);}

        /**
         * @name Blob serialization/deserialization part.
         * These functions are for internal use only!
         * Their names and functionality are likely to change in the near future!
         */
        /** @{ */

        /**
         * Calculate the size of the blob-serialized form of the contained object.
         *
         * @return The needed size in bytes. 0 if the container is null.
         */
        Int32 CalculateBlobSize() const {if (IsNull()) return 0; else return m_pObject->CalculateBlobSize();}

        /** @} */
    private:
        virtual const ObjectPtr GetObjectPointer() const {return boost::static_pointer_cast<Object>(m_pObject);}
        virtual void SetObjectPointer(const ObjectPtr ptr)
        {
            m_pObject = ptr;
        }
        virtual void ResetObjectPointer(){m_pObject.reset();}


        T_Ptr m_pObject;
    };

    /**
     * Container for DOB Objects.
     *
     * This typedef defines an ObjectContainer that contains an Object...
     * (This is also the reason that the object container template is called ObjectContainerImpl. The
     *  name ObjectContainer must be reserved for the container that contains the class Object...
     *  But there should be no need for you to worry about this...)
     */
    typedef Safir::Dob::Typesystem::ObjectContainerImpl<Object> ObjectContainer;
}
}
}
#endif

