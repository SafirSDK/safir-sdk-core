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
#include "dose_main_waiting_states.h"
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Internal/Connections.h>
#include <boost/bind.hpp>


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
        m_timerId = TimerHandler::Instance().RegisterTimeoutHandler(L"Waiting States Timer", *this);

        TimerInfoPtr timerInfo(new EmptyTimerInfo(m_timerId));
        TimerHandler::Instance().Set(Discard,
                                     timerInfo,
                                     GetUtcTime() + 60.0*5); //time out in 5 minutes
    }

    void WaitingStates::Add(const DistributionData & state)
    {
        if (m_isPerforming)
        {
            // Ignore Adds while we're performing a state.
            return;
        }

        const ConnectionId senderId = state.GetSenderId();

        const Key key(state.GetTypeId(),state.GetHandlerId(),state.GetRegistrationTime());

        WaitingStateTable::iterator regIt = m_waitingStateTable.find(key);
        if (regIt == m_waitingStateTable.end())
        { //haven't seen this combo before, add it.
            regIt = m_waitingStateTable.insert(std::make_pair(key,States())).first;
            lllout << "WaitingStates::Add: A new registration has been added to WaitingStates, due to state " << state.Image() << std::endl;
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
            else
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
                theStates.registrationState = state;
            }
            break;

        case DistributionData::EntityState:
            {
                theStates.instances.push_back(state);
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


    void WaitingStates::NodeDown(const Typesystem::Int32 node)
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
        boost::shared_ptr<void> guard = boost::shared_ptr<void>(static_cast<void*>(NULL),boost::bind(&WaitingStates::UnsetPerformingFlag,this,_1));
        m_isPerforming = true;

        for (WaitingStateTable::iterator it = m_waitingStateTable.begin();
             it != m_waitingStateTable.end();) //increment below
        {
            if (it->second.connectionId == connId)
            {
                //make a ref to the state struct to make the code below easier to read.
                States& theStates = it->second;

                if (theStates.registrationState.IsNoState())
                {
                    //we don't have the registration yet, so we wait for that instead.
                    ++it;
                }
                else
                {
                    //process the registration state ...
                    processFunc(theStates.registrationState);

                    // ... and the instances.
                    std::for_each(theStates.instances.begin(), theStates.instances.end(),
                                  boost::bind(processFunc,_1));
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
        boost::shared_ptr<void> guard = boost::shared_ptr<void>(static_cast<void*>(NULL),boost::bind(&WaitingStates::UnsetPerformingFlag,this,_1));
        m_isPerforming = true;

        const Key key(registrationState.GetTypeId(), registrationState.GetHandlerId(),registrationState.GetRegistrationTime());
        const WaitingStateTable::iterator findIt = m_waitingStateTable.find(key);
        if (findIt != m_waitingStateTable.end())
        {
            //make a ref to the state struct to make the code below easier to read.
            States& theStates = findIt->second;

            std::for_each(theStates.instances.begin(), theStates.instances.end(),
                          boost::bind(processFunc,_1));

            m_waitingStateTable.erase(findIt);
        }
    }

    void WaitingStates::HandleTimeout(const TimerInfoPtr& timer)
    {
        TimerHandler::Instance().Set(Discard,
                                     timer,
                                     GetUtcTime() + 5*60.0); //time out again in 60 seconds

        const size_t size = m_waitingStateTable.size();
        if (size != 0 && size == m_lastSize)
        {
            std::wostringstream ostr;
            ostr << "The number of items in the WaitingStates structure has not changed for 5 minutes!" << std::endl
                << "This is probably due to a lost connection to one or more nodes (node split/join)" << std::endl
                 << "Contents of the WaitingStates structure" << std::endl;

            for (WaitingStateTable::iterator it = m_waitingStateTable.begin();
                 it != m_waitingStateTable.end(); ++it)
            {
                ostr << " " << Typesystem::Operations::GetName(it->first.typeId)
                     << ", " << it->first.handlerId
                     << ", " << it->first.registrationTime
                     << ", conn = " << it->second.connectionId
                     << ", hasRegstate = " << std::boolalpha << !it->second.registrationState.IsNoState()
                     << ", #inst = " << it->second.instances.size() <<std::endl;
            }

            lllerr << ostr.str() << std::endl;

            // Remove all entries
            m_waitingStateTable.clear();
        }
        else
        {
            m_lastSize = size;
        }
    }
}
}
}
