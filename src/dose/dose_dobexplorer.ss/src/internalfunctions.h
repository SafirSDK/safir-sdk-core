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
#ifndef INTERNALFUNCTIONS_H
#define INTERNALFUNCTIONS_H
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/EntityType.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/StateDeleter.h>


namespace Safir
{
    namespace Dob
    {
        namespace Internal
        {
            class IStatisticsCollector;

            struct Arguments
            {
                Arguments():
                simple(false),
                numStates(0),
                downgraded(0),
                realStates(0),
                ghostStates(0),
                injectionStates(0),
                numSubscribers(0)
            {}

            void ResetOutParam()
            {
                numStates = 0;
                downgraded = 0;
                realStates = 0;
                ghostStates = 0;
                injectionStates = 0;
                numSubscribers = 0;
            }

            //IN PARAMETERS
            Safir::Dob::Typesystem::TypeId typeId;
            IStatisticsCollector* _this;
            int context;
            bool simple;

            //OUT PARAMETERS
            boost::uint64_t numStates;
            boost::uint64_t downgraded;
            boost::uint64_t realStates;
            boost::uint64_t ghostStates;
            boost::uint64_t injectionStates;

            //TEMPORARIES
            Safir::Dob::Typesystem::Int64 instanceId;
            bool getInfo;
            std::wstring info;
            bool contextChecked;

            std::string connection;
            std::string kind;
            std::string handler;
            int numSubscribers;
            };


            void StatisticsCollector(State& state, void* arg);
            void StatisticsCollector(EntityTypes& entityTypes, void* arg);
            void StatisticsCollector(EntityType& entityType, void* arg);
            void StatisticsCollector(StateContainer& stateContainer, void* arg);

            class IStatisticsCollector
            {
            public:
                virtual void ProcessState(const Safir::Dob::Typesystem::Int64 instance, const StateSharedPtr& statePtr, Arguments& arguments) = 0;
                virtual void AddContextRow(EntityType& entityType, Arguments& argumentsXS) = 0;
                virtual void InitRemoveInstances(Arguments& arguments) = 0;
                virtual void RemoveInstances() = 0;
                virtual void AddContextGlobalData(Arguments& arguments) = 0;
            };
        }
    }
}

#endif //INTERNALFUNCTIONS_H


