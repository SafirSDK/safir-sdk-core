/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#ifndef __DOSE_END_STATES_H__
#define __DOSE_END_STATES_H__

#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/LeveledLock.h>
#include <boost/interprocess/sync/interprocess_mutex.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API EndStates:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    private:
        //This is to make sure that only Instance can call the constructor even though the constructor
        //itself has to be public (limitation of boost::interprocess)
        struct private_constructor_t {};
    public:
        static void Initialize();

        /**
         * Get the singleton instance.
         * Note that this is not a singleton in the usual sense of the word,
         * it is the single instance of something that resides in shared memory.
         */
        static EndStates & Instance();

        //The constructor and destructor have to be public for the boost::interprocess internals to be able to call
        //them, but we can make the constructor "fake-private" by making it require a private type as argument.
        explicit EndStates(private_constructor_t);
        ~EndStates();

        /** Called by dose_internal when it wants an end state to be kept for "a while". */
        void Add(const StateSharedPtr& state);

        /** Called by dose_main every "while", every call will remove states added before the previous call.
         * E.g. calling every minute causes states to be kept for at least one minute but for a maximum of two minutes.
         */
        void HandleTimeout();
    private:
        //Locking Policy: Just lock when using the m_states table.
        typedef Safir::Dob::Internal::LeveledLock<boost::interprocess::interprocess_mutex,
                                                  END_STATES_LOCK_LEVEL,
                                                  NO_MASTER_LEVEL_REQUIRED> EndStatesLock;
        EndStatesLock m_lock;
        typedef boost::interprocess::scoped_lock<EndStatesLock> ScopedEndStatesLock;

        typedef PairContainers<StateSharedPtr,Typesystem::Int64>::map StateTable;

        StateTable m_states;

        Typesystem::Int64 m_lastTimestamp;

        static EndStates* m_instance;
    };
}
}
}
#endif

