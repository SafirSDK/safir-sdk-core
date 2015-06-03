/******************************************************************************
*
* Copyright Saab AB, 2014 (http://www.safirsdk.com)
*
* Created by: Mikael Wennerberg
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
#include "internalfunctions.h"
#include <Safir/Dob/NodeParameters.h>


void Safir::Dob::Internal::StatisticsCollector(Safir::Dob::Internal::EntityTypes& entityTypes, void* arg)
{
    Arguments& arguments = *static_cast<Arguments*>(arg);
    StatisticsCollector(entityTypes.GetType(arguments.typeId), arg);
}

void Safir::Dob::Internal::StatisticsCollector(Safir::Dob::Internal::EntityType& entityType, void* arg)
{
    Arguments& arguments = *static_cast<Arguments*>(arg);

    if (arguments.simple)
    {
        StatisticsCollector(entityType.m_entityStates[0],arg);
    }
    else
    {
        for (int context = 0; context < NodeParameters::NumberOfContexts(); ++context)
        {
            arguments.ResetOutParam();
            arguments.context = context;

            // Add context rows if neccessary and check the "show" checkbox
            arguments._this->AddContextRow(entityType, arguments);

            arguments._this->InitRemoveInstances(arguments);

            // Update the instance table.
            // (This routine updates  m_removeInstances.)
            StatisticsCollector(entityType.m_entityStates[context],arg);

            // Delete the instance rows that shouldn't be kept
            arguments._this->RemoveInstances();

            // Now that we have visited all instances in this context we can produce the overall
            // context data.
            arguments._this->AddContextGlobalData(arguments);
        }
    }
}

void Safir::Dob::Internal::StatisticsCollector(Safir::Dob::Internal::State& state, void* arg)
{
    Arguments& arguments = *static_cast<Arguments*>(arg);
    DistributionData realState = state.m_realState.GetState();

    if (!realState.IsNoState())
    {
        if (realState.GetEntityStateKind() == DistributionData::Real)
        {
            arguments.kind = "Real";
            ++arguments.realStates;
        }
        else
        {
            arguments.kind = "Ghost";
            ++arguments.ghostStates;
        }

        arguments.handler = Safir::Dob::Typesystem::Utilities::ToUtf8(realState.GetHandlerId().ToString());
    }

    DistributionData injectionState = state.m_injectionState.GetState();
    if (!injectionState.IsNoState())
    {
        ++arguments.injectionStates;

        if (!arguments.kind.empty())
        {
            arguments.kind.append(" ");
        }
        arguments.kind.append("has Injection");
    }

    if (arguments.getSubscibers)
    {
        std::wostringstream ostr;
        ostr << "Subscribers for instance : " << arguments.instanceId << std::endl;
        State::Subscriptions subscriptions = state.m_subscriptions;

        for (State::Subscriptions::iterator iter = subscriptions.begin(); iter != subscriptions.end(); ++iter)
        {
           ostr << iter->first.connectionConsumer.connection->NameWithoutCounter() << std::endl;
        }

        arguments.info = ostr.str();
    }

    if (arguments.getInfo)
    {
        std::wostringstream ostr;

        if (!arguments.info.empty())
        {
            ostr << std::endl;
        }

        ostr << "Instance: " << arguments.instanceId << std::endl;

        if (!realState.IsNoState())
        {
            ostr << "RealState: " << realState.Image();
        }

        if (!injectionState.IsNoState())
        {
            ostr << "Injection State: " << injectionState.Image();
        }

        if (arguments.info.empty())
        {
            arguments.info = ostr.str();
        }
        else
        {
            arguments.info.append(ostr.str());
        }
    }

    if (!arguments.simple && state.m_connection != NULL)
    {
        arguments.connection = state.m_connection->NameWithCounter();
    }
    else
    {
        arguments.connection = "No owner";
    }


    arguments.numSubscribers = static_cast<int>(state.m_subscriptions.size());
}

void Safir::Dob::Internal::StatisticsCollector(Safir::Dob::Internal::StateContainer& stateContainer, void* arg)
{
    Arguments& arguments = *static_cast<Arguments*>(arg);
    stateContainer.ForEachState(boost::bind(&IStatisticsCollector::ProcessState,
        arguments._this,
        _1,
        _2,
        boost::ref(arguments)),
        true);
}
