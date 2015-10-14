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

#ifndef __DOSE_INTERNAL_META_SUBSCRIPTION_H__
#define __DOSE_INTERNAL_META_SUBSCRIPTION_H__

#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/SubscriptionOptions.h>

#include <set>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class MetaSubscription : public SharedMemoryObject
    {
    public:

        explicit MetaSubscription(const SubscriptionOptionsPtr & subscriptionOptions)
            : m_all(false),
              m_blacklist(),
              m_whitelist(),
              m_subscriptionOptions(subscriptionOptions)
        {
        }

        void SubscribeAll()
        {
            m_all = true;
            m_blacklist.clear();
            m_whitelist.clear();
        }

        void UnsubscribeAll()
        {
            m_all = false;
            m_blacklist.clear();
            m_whitelist.clear();
        }

        void Subscribe(const Dob::Typesystem::Int64 item)
        {
            if (m_all)
            {
                m_blacklist.erase(item);
            }
            else
            {
                m_whitelist.insert(item);
            }
        }

        void Unsubscribe(const Dob::Typesystem::Int64 item)
        {
            if (m_all)
            {
                m_blacklist.insert(item);
            }
            else
            {
                m_whitelist.erase(item);
            }
        }

        bool IsSubscribed(const Dob::Typesystem::Int64 item) const
        {
            if (m_all)
            {
                if (m_blacklist.find(item) == m_blacklist.end())
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            else
            {
                if (m_whitelist.find(item) != m_whitelist.end())
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
        }

        bool IsAnySubscribed() const
        {
            if (m_all || !m_whitelist.empty())
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        const SubscriptionOptionsPtr& GetSubscriptionOptions() const
        {
            return m_subscriptionOptions;
        }

    private:

        bool m_all;

        typedef Containers<Dob::Typesystem::Int64>::set ShmItemSet;

        ShmItemSet m_blacklist;
        ShmItemSet m_whitelist;

        SubscriptionOptionsPtr m_subscriptionOptions;
    };
}
}
}

#endif
