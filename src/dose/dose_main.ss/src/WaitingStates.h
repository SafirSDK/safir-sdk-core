/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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

#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/DistributionData.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    /**
     * This class is used as part of the mechanism that handles the cases when registration and/or
     * entity states are received in the wrong order.
     *
     * Be aware that the class is NOT thread safe!
     */
    class WaitingStates:
        private boost::noncopyable
    {
    public:
        WaitingStates();

        void Add(const DistributionData& state, int64_t fromNodeType);

        /** Remove all states waiting for registrations older than this state */
        void CleanUp(const DistributionData& registrationState);

        /** Remove all states waiting for the connection from the tables */
        void Disconnect(const ConnectionId& connId);

        /** Remove all states waiting for anything from a node */
        void NodeDown(const int64_t node);

        typedef boost::function<void (const DistributionData& state,
                                      int64_t fromNodeType)> ProcessStateFunc;

        /** calls processFunc for every state that is waiting for the connection */
        void PerformStatesWaitingForConnection(const ConnectionId & connId,
                                               const ProcessStateFunc& processFunc);

        /** calls processFunc for every state that is waiting for that registrationState.
          * To remove states waiting for older registrations, call CleanUp.
          */
        void PerformStatesWaitingForRegistration(const DistributionData & registrationState,
                                                 const ProcessStateFunc& processFunc);

    private:
        struct State
        {
           State()
               :state(DistributionData(no_state_tag)), fromNodeType(0) {}
           State(const DistributionData& _state, int64_t _fromNodeType)
               : state(_state), fromNodeType(_fromNodeType) {}
           DistributionData state;
           int64_t fromNodeType;
        };

        typedef std::vector<State> EntityStates;

        struct States
        {
            ConnectionId connectionId;
            State registrationState;
            EntityStates entityStates;
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

        size_t m_lastSize;
    };
}
}
}


