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
#include "WaitingStates.h"
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Dob/Internal/Connections.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    WaitingStates::WaitingStates():
        m_isPerforming(false)
    {
    }

    void WaitingStates::Add(const DistributionData& state, int64_t fromNodeType)
    {
        if (m_isPerforming)
        {
            lllog(5) << "WaitingStates::Add: We are performing. Ignoring add" << std::endl;
            // Ignore Adds while we're performing a state.
            return;
        }

        const ConnectionId senderId = state.GetSenderId();

        const Key key(state.GetTypeId(),state.GetHandlerId(),state.GetRegistrationTime());

        WaitingStateTable::iterator regIt = m_waitingStateTable.find(key);
        if (regIt == m_waitingStateTable.end())
        { //haven't seen this combo before, add it.
            regIt = m_waitingStateTable.insert(std::make_pair(key,States())).first;
            lllout << "WaitingStates::Add: A new registration has been added to WaitingStates, due to state "
                   << state.Image() << std::endl;
        }

        //make a ref to the state struct to make the code below easier to read.
        States& theStates = regIt->second;

        if (state.GetType() == DistributionData::RegistrationState ||
           (state.GetType() == DistributionData::EntityState && state.GetEntityStateKind() != DistributionData::Ghost))
        {
            // Connection id handling only for non-ghosts
            
            if (theStates.connectionId == ConnectionId())
            {
                //if we didn't have a connection id before
                theStates.connectionId = senderId;
            }
            else if (theStates.connectionId.m_id != -1 && senderId.m_id != -1)
            {
                ENSURE(theStates.connectionId == senderId, << "WaitingStates::Add does not expect connection id's to change! old "
                       << theStates.connectionId << ", new " << senderId << ". While processing state " << state.Image());
            }
        }

        switch (state.GetType())
        {
        case DistributionData::RegistrationState:
            {
                ENSURE(state.IsRegistered(), << "WaitingStates::Add can not handle unregistration states! Got state " << state.Image());
                theStates.registrationState = State(state, fromNodeType);
            }
            break;

        case DistributionData::EntityState:
            {
                theStates.entityStates.push_back(State(state, fromNodeType));
            }
            break;

        default:
            ENSURE(false, << "WaitingStates::Add: Unexpected state type in state " << state.Image());
        }
    }

    void WaitingStates::CleanUp(const DistributionData & registrationState)
    {
        if (m_isPerforming || m_waitingStateTable.empty())
        {
            return;
        }

        const Typesystem::TypeId typeId = registrationState.GetTypeId();
        const Typesystem::HandlerId handlerId = registrationState.GetHandlerId();
        const LamportTimestamp regTime = registrationState.GetRegistrationTime();

        for (WaitingStateTable::iterator it = m_waitingStateTable.begin();
             it != m_waitingStateTable.end();) //increment below
        {
            const Key& key = it->first;
            if (key.typeId == typeId && key.handlerId == handlerId && key.registrationTime < regTime)
            {
                m_waitingStateTable.erase(it++);
            }
            else
            {
                ++it;
            }
        }
    }


    void WaitingStates::Disconnect(const ConnectionId& connId)
    {
        for (WaitingStateTable::iterator it = m_waitingStateTable.begin();
             it != m_waitingStateTable.end();) //increment below
        {
            if (it->second.connectionId == connId)
            {
                m_waitingStateTable.erase(it++);
            }
            else
            {
                ++it;
            }
        }
    }


    void WaitingStates::NodeDown(const int64_t node)
    {
        for (WaitingStateTable::iterator it = m_waitingStateTable.begin();
             it != m_waitingStateTable.end();) //increment below
        {
            if (it->second.connectionId.m_node == node)
            {
                m_waitingStateTable.erase(it++);
            }
            else
            {
                ++it;
            }
        }
    }


    void WaitingStates::PerformStatesWaitingForConnection(const ConnectionId & connId,
                                                          const ProcessStateFunc& processFunc)
    {
        if (m_isPerforming || m_waitingStateTable.empty())
        {
            return;
        }
        //use a shared ptr to guarantee that the performing flag is unset when we exit method.
        std::shared_ptr<void> guard =
                std::shared_ptr<void>(static_cast<void*>(NULL),
                                        [this](void*){UnsetPerformingFlag();});
        m_isPerforming = true;

        for (WaitingStateTable::iterator it = m_waitingStateTable.begin();
             it != m_waitingStateTable.end();) //increment below
        {
            if (it->second.connectionId == connId)
            {
                //make a ref to the state struct to make the code below easier to read.
                States& theStates = it->second;

                if (theStates.registrationState.state.IsNoState())
                {
                    //we don't have the registration yet, so we wait for that instead.
                    ++it;
                }
                else
                {
                    //process the registration state ...
                    processFunc(theStates.registrationState.state, theStates.registrationState.fromNodeType);

                    // ... and the instances.
                    for (auto entityState = theStates.entityStates.cbegin(); entityState != theStates.entityStates.cend(); ++entityState)
                    {
                        processFunc(entityState->state, entityState->fromNodeType);
                    }

                    m_waitingStateTable.erase(it++);
                }
            }
            else
            {
                ++it;
            }
        }
    }


    void WaitingStates::PerformStatesWaitingForRegistration(const DistributionData & registrationState,
                                                            const ProcessStateFunc& processFunc)
    {
        if (m_isPerforming || m_waitingStateTable.empty())
        {
            return;
        }

        //use a shared ptr to guarantee that the performing flag is unset when we exit method.
        std::shared_ptr<void> guard =
                std::shared_ptr<void>(static_cast<void*>(NULL),
                                      [this](void*){UnsetPerformingFlag();});

        m_isPerforming = true;

        const Key key(registrationState.GetTypeId(), registrationState.GetHandlerId(),registrationState.GetRegistrationTime());
        const WaitingStateTable::iterator findIt = m_waitingStateTable.find(key);
        if (findIt != m_waitingStateTable.end())
        {
            //make a ref to the state struct to make the code below easier to read.
            States& theStates = findIt->second;

            for (auto entityState = theStates.entityStates.cbegin(); entityState != theStates.entityStates.cend(); ++entityState)
            {
                processFunc(entityState->state, entityState->fromNodeType);
            }

            m_waitingStateTable.erase(findIt);
        }
    }

    void WaitingStates::SanityCheck()
    {
        for (auto element = m_waitingStateTable.begin(); element != m_waitingStateTable.end(); ++element)
        {
            if (element->second.sanityCounter == 0)
            {
                // The sanity method hasn't seen this state before
                ++element->second.sanityCounter;
            }
            else
            {
                // This state was here the last time this method was executed.
                std::wostringstream ostr;
                ostr << "One or more items seem to be stuck in WaitingStates! "
                     << "If the system is artificially stopped when debugging or testing this "
                     << "warning can be ignored.\n"
                     << "ConnectionId=" << element->second.connectionId << "\n"
                     << "registrationState=" << element->second.registrationState.state.Image() << "\n"
                     << "Number of entityStates=" << element->second.entityStates.size();
                if (!element->second.entityStates.empty())
                {
                    ostr << "First entityState=" << element->second.entityStates.cbegin()->state.Image();
                }

                Safir::Utilities::Internal::Log::Send(Safir::Utilities::Internal::Log::Warning,
                                                      ostr.str());

                lllog(1) << ostr.str() << std::endl;
            }

        }
    }
}
}
}
