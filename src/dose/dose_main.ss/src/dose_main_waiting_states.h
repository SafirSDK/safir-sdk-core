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
#ifndef __DOSE_MAIN_WAITING_STATES_H__
#define __DOSE_MAIN_WAITING_STATES_H__

#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include "dose_main_timers.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class WaitingStates:
        public TimeoutHandler,
        private boost::noncopyable
    {
    public:
        WaitingStates();

        void Add(const DistributionData & state);

        /** Remove all states waiting for registrations older than this state */
        void CleanUp(const DistributionData & registrationState);

        /** Remove all states waiting for the connection from the tables */
        void Disconnect(const ConnectionId& connId);

        /** Remove all states waiting for anything from a node */
        void NodeDown(const Typesystem::Int32 node);

        typedef boost::function<void (const DistributionData& state)> ProcessStateFunc;

        /** calls processFunc for every state that is waiting for the connection */
        void PerformStatesWaitingForConnection(const ConnectionId & connId,
                                               const ProcessStateFunc& processFunc);

        /** calls processFunc for every state that is waiting for that registrationState.
          * To remove states waiting for older registrations, call CleanUp.
          */
        void PerformStatesWaitingForRegistration(const DistributionData & registrationState,
                                                 const ProcessStateFunc& processFunc);

    private:
        virtual void HandleTimeout(const TimerInfoPtr & timer);

        typedef std::map<Typesystem::InstanceId, DistributionData> Instances;

        struct States
        {
            explicit States(): registrationState(DistributionData(no_state_tag)){}
            ConnectionId connectionId;
            DistributionData registrationState;
            Instances instances;
        };

        struct Key
        {
            Key(const Typesystem::TypeId _typeId,
                const Typesystem::HandlerId& _handlerId,
                const LamportTimestamp& _regTime):
              typeId(_typeId),handlerId(_handlerId),registrationTime(_regTime) {}

            Typesystem::TypeId typeId;
            Typesystem::HandlerId handlerId;
            LamportTimestamp registrationTime;

            bool operator < (const Key& other) const
            {
                if (typeId < other.typeId)
                {
                    return true;
                }
                else if (typeId > other.typeId)
                {
                    return false;
                }

                if (handlerId < other.handlerId)
                {
                    return true;
                }
                else if (other.handlerId < handlerId)
                {
                    return false;
                }

                if (registrationTime < other.registrationTime)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
        };

        typedef std::map<Key,States> WaitingStateTable;
        WaitingStateTable m_waitingStateTable;

        //This bool is set when we're doing one of the Perform* methods, so that we
        //can avoid unwanted recursion.
        bool m_isPerforming;

        void UnsetPerformingFlag(void *){m_isPerforming = false;}

        //the timer is for checking that nothing "gets stuck" in this structure.
        //we check every 5 minutes and if the size of the structure is unchanged
        //we log an error.
        //A little bit rudimentary, but it should catch the errors and it is _very_
        //unlikely
        TimerId m_timerId;
        size_t m_lastSize;
    };
}
}
}

#endif

