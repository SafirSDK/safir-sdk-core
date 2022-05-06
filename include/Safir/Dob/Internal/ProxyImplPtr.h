/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
*
* Created by: Anders Widén / stawi
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

#include <Safir/Dob/Typesystem/Exceptions.h>
#include <type_traits>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    // A copy of boost checked_delete
    template<class T> inline void checked_delete(T * x)
    {
        // intentionally complex - simplification causes regressions
        typedef char type_must_be_complete[ sizeof(T)? 1: -1 ];
        (void) sizeof(type_must_be_complete);
        delete x;
    }

    // Class that provides a smart pointer that can be copied just once. It has
    // std::auto_ptr semantics so don't use the original object after copying.
    template <class ImplT>
    class ProxyImplPtr
    {
    public:
        explicit ProxyImplPtr(ImplT* pImpl) :
            m_pImpl(pImpl),
            m_copyCounter(0),
            m_deleterFunc(checked_delete<ImplT>)
        {
            // This is all stuff that is meant to disallow usage of this type with an incomplete type

            static_assert(!std::is_void<ImplT>::value,
                          "can't delete pointer to incomplete type");
            static_assert(sizeof(ImplT)>0,
                          "can't delete pointer to incomplete type");


            typedef char type_must_be_complete[ sizeof(ImplT)? 1: -1 ];
            (void) sizeof(type_must_be_complete);
        }

        ProxyImplPtr(const ProxyImplPtr& other):
            m_copyCounter(other.m_copyCounter),
            m_deleterFunc(other.m_deleterFunc)
        {
            if (other.m_copyCounter != 0)
            {
                // This object has been copied before!
                throw Dob::Typesystem::SoftwareViolationException
                                    (L"Trying to copy a ProxyImplPtr more than once",__WFILE__,__LINE__);
            }

            m_pImpl = other.m_pImpl;

            const_cast<ProxyImplPtr&>(other).m_pImpl = NULL;
            ++const_cast<ProxyImplPtr&>(other).m_copyCounter;

            ++m_copyCounter;
        }

        ~ProxyImplPtr()
        {
            if (m_pImpl != NULL)
            {
                m_deleterFunc(m_pImpl);
                m_pImpl = NULL;
            }
        }

        ImplT& operator* () const
        {
            if (m_pImpl == NULL)
            {
                throw Dob::Typesystem::SoftwareViolationException
                        (L"Dereferencing the original ProxyImplPtr after it has been copied",__WFILE__,__LINE__);
            }
            return *m_pImpl;
        }

        ImplT* operator-> () const
        {
            if (m_pImpl == NULL)
            {
                throw Dob::Typesystem::SoftwareViolationException
                        (L"Dereferencing the original ProxyImplPtr after it has been copied",__WFILE__,__LINE__);
            }
            return m_pImpl;
        }

    private:

        // Assignment not allowed
        ProxyImplPtr& operator=(const ProxyImplPtr& other);

        ImplT* m_pImpl;

        unsigned int m_copyCounter;

        void (*m_deleterFunc) (ImplT *);

    };

}
}
}


