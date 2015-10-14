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
        m_isPerforming(false),
        m_lastSize(0)
    {
    }

    void WaitingStates::Add(const DistributionData& state, int64_t fromNodeType)
    {
        if (m_isPerforming)
        {
            lllog(1) << "WaitingStates::Add: Ignoring state (m_isPerforming == true) State:" << state.Image() << std::endl;

            // Ignore Adds while we're performing a state.
            return;
        }

        const ConnectionId senderId = state.GetSenderId();

        const Key key(state.GetTypeId(),state.GetHandlerId(),state.GetRegistrationTime());

        WaitingStateTable::iterator regIt = m_waitingStateTable.find(key);
        if (regIt == m_waitingStateTable.end())
        { //haven't seen this combo before, add it.
            regIt = m_waitingStateTable.insert(std::make_pair(key,States())).first;
            lllog(1) << "WaitingStates::Add: A new item with Key: " << key.ToString()
                     << " has been added to WaitingStates, due to state " << state.Image() << std::endl;
        }
        else
        {
             lllog(1) << "WaitingStates::Add: Found existing Key: " << key.ToString() << std::endl;
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

                lllog(1) << "WaitingStates::Add: Setting connectionId " << senderId << " for Key: " << key.ToString() << std::endl;
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
                lllog(1) << "WaitingStates::Add: Adding registration state to Key: " << key.ToString()
                         << " RegState: " << state.Image() << std::endl;
            }
            break;

        case DistributionData::EntityState:
            {
                theStates.entityStates.push_back(State(state, fromNodeType));
                lllog(1) << "WaitingStates::Add: Adding entity state to Key: " << key.ToString()
                         << " EntityState: " << state.Image() << std::endl;
            }
            break;

        default:
            ENSURE(false, << "WaitingStates::Add: Unexpected state type in state " << state.Image());
        }
    }

    void WaitingStates::CleanUp(const DistributionData & registrationState)
    {
        lllog(1) << "WaitingStates::CleanUp: RegState: " << registrationState.Image() << std::endl;

        if (m_isPerforming || m_waitingStateTable.empty())
        {
            lllog(1) << "WaitingStates::Cleanup: RETURN (m_isPerforming is " << m_isPerforming
                     << ") State:" << registrationState.Image() << std::endl;
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
                lllog(1) << "WaitingStates::Cleanup: Erasing Key: " << key.ToString()
                         << " State:" << registrationState.Image() << std::endl;
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
        lllog(1) << "WaitingStates::Disconnect: connId: " << connId << std::endl;

        for (WaitingStateTable::iterator it = m_waitingStateTable.begin();
             it != m_waitingStateTable.end();) //increment below
        {
            if (it->second.connectionId == connId)
            {
                lllog(1) << "WaitingStates::Disconnect: Erasing Key: " << it->first.ToString() << std::endl;
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
        lllog(1) << "WaitingStates::NodeDown: node: " << node << std::endl;

        for (WaitingStateTable::iterator it = m_waitingStateTable.begin();
             it != m_waitingStateTable.end();) //increment below
        {
            if (it->second.connectionId.m_node == node)
            {
                lllog(1) << "WaitingStates::NodeDown: Erasing Key: " << it->first.ToString() << std::endl;
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
        lllog(1) << "WaitingStates::PerformStatesWaitingForConnection: ConnectionId: " << connId << std::endl;

        if (m_isPerforming || m_waitingStateTable.empty())
        {
            lllog(1) << "WaitingStates::PerformStatesWaitingForConnection: RETURN (m_isPerforming is "
                     << m_isPerforming << ")" << std::endl;

            return;
        }
        //use a shared ptr to guarantee that the performing flag is unset when we exit method.
        boost::shared_ptr<void> guard =
                boost::shared_ptr<void>(static_cast<void*>(NULL),
                                        boost::bind(&WaitingStates::UnsetPerformingFlag,this,_1));
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
                    lllog(1) << "WaitingStates::PerformStatesWaitingForConnection: We don't have the registration yet, so we wait for that instead."
                             << "Key:" << it->first.ToString() << std::endl;

                    //we don't have the registration yet, so we wait for that instead.
                    ++it;
                }
                else
                {
                    lllog(1) << "WaitingStates::PerformStatesWaitingForConnection: Process registrationState:"
                             << theStates.registrationState.state.Image() << std::endl;

                    //process the registration state ...
                    processFunc(theStates.registrationState.state, theStates.registrationState.fromNodeType);

                    // ... and the instances.
                    for (auto entityState = theStates.entityStates.cbegin(); entityState != theStates.entityStates.cend(); ++entityState)
                    {
                        lllog(1) << "WaitingStates::PerformStatesWaitingForConnection: Process Entity instance:"
                                 << entityState->state.Image() << std::endl;

                        processFunc(entityState->state, entityState->fromNodeType);
                    }

                    lllog(1) << "WaitingStates::PerformStatesWaitingForConnection: Erasing Key: "
                             << it->first.ToString() << std::endl;

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
        lllog(1) << "WaitingStates::PerformStatesWaitingForRegistration: RegState: " << registrationState.Image() << std::endl;

        if (m_isPerforming || m_waitingStateTable.empty())
        {
            lllog(1) << "WaitingStates::PerformStatesWaitingForRegistration: RETURN (m_isPerforming is "
                     << m_isPerforming << ")" << std::endl;
            return;
        }

        //use a shared ptr to guarantee that the performing flag is unset when we exit method.
        boost::shared_ptr<void> guard =
                boost::shared_ptr<void>(static_cast<void*>(NULL),
                                        boost::bind(&WaitingStates::UnsetPerformingFlag,this,_1));

        m_isPerforming = true;

        const Key key(registrationState.GetTypeId(), registrationState.GetHandlerId(),registrationState.GetRegistrationTime());
        const WaitingStateTable::iterator findIt = m_waitingStateTable.find(key);
        if (findIt != m_waitingStateTable.end())
        {
            lllog(1) << "WaitingStates::PerformStatesWaitingForRegistration: Found Key: " << findIt->first.ToString() << std::endl;

            //make a ref to the state struct to make the code below easier to read.
            States& theStates = findIt->second;

            for (auto entityState = theStates.entityStates.cbegin(); entityState != theStates.entityStates.cend(); ++entityState)
            {
                lllog(1) << "WaitingStates::PerformStatesWaitingForRegistration: Process EntityState:"
                         << entityState->state.Image() << std::endl;

                processFunc(entityState->state, entityState->fromNodeType);
            }

            lllog(1) << "WaitingStates::PerformStatesWaitingForRegistration: Erasing Key: "
                     << findIt->first.ToString() << std::endl;

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
                     << "entityStates=\n";
                for (auto entityState = element->second.entityStates.cbegin(); entityState != element->second.entityStates.cend(); ++entityState)
                {
                    ostr << entityState->state.Image() << "\n";
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
