/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
#ifndef __SUBSCRIPTIONOPTIONS_H__
#define __SUBSCRIPTIONOPTIONS_H__

#include <Safir/Dob/Internal/SharedMemoryObject.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class SubscriptionOptions:
        public SharedMemoryObject
    {
    public:
        typedef SmartPointers<void>::shared_ptr SubscriptionOptionsPtr;

        class EntitySubscriptionOptions:
            boost::noncopyable
        {
        private:
            struct private_constructor_t {};
        public:
            static SubscriptionOptionsPtr Create(const bool includeUpdates,
                                                 const bool wantsGhostDelete,
                                                 const bool wantsLastState,
                                                 const bool doesntWantSourceIsPermanentStore,
                                                 const bool wantsAllStateChanges,
                                                 const bool timestampChangeInfo)
            {
                return SubscriptionOptionsPtr(
                    GetSharedMemory().construct<EntitySubscriptionOptions>(boost::interprocess::anonymous_instance)
                        (private_constructor_t(),
                         includeUpdates,
                         wantsGhostDelete,
                         wantsLastState,
                         doesntWantSourceIsPermanentStore,
                         wantsAllStateChanges,
                         timestampChangeInfo));
            }

            const bool includeUpdates;
            const bool wantsGhostDelete;
            const bool wantsLastState;
            const bool doesntWantSourceIsPermanentStore;
            const bool wantsAllStateChanges;
            const bool timestampChangeInfo;

            EntitySubscriptionOptions(private_constructor_t,
                                      const bool _includeUpdates,
                                      const bool _wantsGhostDelete,
                                      const bool _wantsLastState,
                                      const bool _doesntWantSourceIsPermanentStore,
                                      const bool _wantsAllStateChanges,
                                      const bool _timestampChangeInfo):
                includeUpdates(_includeUpdates),
                wantsGhostDelete(_wantsGhostDelete),
                wantsLastState(_wantsLastState),
                doesntWantSourceIsPermanentStore(_doesntWantSourceIsPermanentStore),
                wantsAllStateChanges(_wantsAllStateChanges),
                timestampChangeInfo(_timestampChangeInfo)
            {}
        private:

        };
    };


    typedef SubscriptionOptions::SubscriptionOptionsPtr SubscriptionOptionsPtr;
    typedef SubscriptionOptions::EntitySubscriptionOptions EntitySubscriptionOptions;
}
}
}

#endif

