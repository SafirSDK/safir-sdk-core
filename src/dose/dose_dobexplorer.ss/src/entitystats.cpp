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

#include "common_header.h"
#include "entitystats.h"
#include <boost/lexical_cast.hpp>
#include <sstream>
#include <math.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Internal/Connection.h>


const int CHECK_COLUMN = 0;
const int INSTANCE_ID_COLUMN = 1;
const int CONNECTION_COLUMN = 2;
const int ENTITY_STATE_KIND_COLUMN = 3;
const int HANDLER_ID_COLUMN = 4;
const int NUM_SUBS_COLUMN = 5;


EntityStats::EntityStats(QWidget * /*parent*/, const Safir::Dob::Typesystem::TypeId typeId):
    m_timer(this),
    m_typeId(typeId)
{
    setupUi(this); // this sets up GUI

    Safir::Dob::Internal::EntityTypes::Initialize();
    Safir::Dob::Internal::InjectionKindTable::Initialize();
    // signals/slots mechanism in action
    connect(&m_timer, SIGNAL(timeout()), this, SLOT(UpdateStatistics()));
    m_timer.start(3000);

    instances->verticalHeader()->hide();
    instances->sortByColumn(INSTANCE_ID_COLUMN,Qt::AscendingOrder);
    instances->setSortingEnabled(true);
    UpdateStatistics();
}

struct Arguments
{
    Arguments():
        numStates(0),
        downgraded(0),
        realStates(0),
        ghostStates(0),
        injectionStates(0)
    {}

    //IN PARAMETERS
    Safir::Dob::Typesystem::TypeId typeId;
    EntityStats* _this;

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

    std::string connection;
    std::string kind;
    std::string handler;
    int numSubscribers;
};



void Safir::Dob::Internal::StatisticsCollector(Safir::Dob::Internal::EntityTypes& entityTypes, void* arg)
{
    Arguments& arguments = *static_cast<Arguments*>(arg);
    Safir::Dob::Internal::EntityType &entityType = entityTypes.GetType(arguments.typeId);

    StatisticsCollector(entityType,arg);
}

void Safir::Dob::Internal::StatisticsCollector(Safir::Dob::Internal::EntityType& entityType, void* arg)
{
    StatisticsCollector(entityType.m_entityStates,arg);
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

    if (arguments.getInfo)
    {
        std::wostringstream ostr;
        ostr << "Instance: " << arguments.instanceId << std::endl;

        if (!realState.IsNoState())
        {
            ostr << "RealState: " << realState.Image();
        }

        if (!injectionState.IsNoState())
        {
            ostr << "Injection State: " << injectionState.Image();
        }
        arguments.info = ostr.str();
    }

    if (state.m_connection != NULL)
    {
        arguments.connection = state.m_connection->NameWithCounter();
    }
    else
    {
        arguments.connection = "No owner";
    }


    arguments.numSubscribers = state.m_subscriptions.size();
}

void EntityStats::ProcessState(const Safir::Dob::Typesystem::Int64 instance,
                               const Safir::Dob::Internal::UpgradeableStateResult& statePtrResult,
                               Arguments& arguments)
{
    ++arguments.numStates;
    if (statePtrResult.second)
    {
        ++arguments.downgraded;
    }
    arguments.instanceId = instance;

    const QString instanceString = boost::lexical_cast<std::string>(instance).c_str();
    int row = 0;
    QTableWidgetItem* findResult = NULL;
    for (int i = 0; i < instances->rowCount(); ++i)
    {
        if (instances->item(i,INSTANCE_ID_COLUMN)->text() == instanceString)
        {
            assert (findResult == NULL); //just make a stupidity check
            findResult = instances->item(i,INSTANCE_ID_COLUMN);
        }
    }

    if (findResult != NULL)
    {
        row = findResult->row();
        arguments.getInfo = instances->item(row,CHECK_COLUMN)->checkState() == Qt::Checked;
        m_removeInstances.erase(findResult);
    }
    else
    {
        instances->insertRow(0);
        QTableWidgetItem * item = new QTableWidgetItem();
        item->setFlags(Qt::ItemIsUserCheckable|Qt::ItemIsEnabled);
        item->setCheckState(Qt::Unchecked);
        instances->setItem(0,CHECK_COLUMN,item);
        instances->setItem(0,INSTANCE_ID_COLUMN,new QTableWidgetItem(instanceString));
        instances->setItem(0,CONNECTION_COLUMN,new QTableWidgetItem());
        instances->setItem(0,ENTITY_STATE_KIND_COLUMN,new QTableWidgetItem());
        instances->setItem(0,HANDLER_ID_COLUMN,new QTableWidgetItem());
        instances->setItem(0,NUM_SUBS_COLUMN,new QTableWidgetItem());

        arguments.getInfo = false;
    }

    arguments.info.clear();
    arguments.connection.clear();
    arguments.kind.clear();
    arguments.handler.clear();

    StatisticsCollector(*statePtrResult.first, &arguments);

    if(!arguments.info.empty())
    {
        information->append(Safir::Dob::Typesystem::Utilities::ToUtf8(arguments.info).c_str());
    }

    instances->item(row,CONNECTION_COLUMN)->setText(arguments.connection.c_str());
    instances->item(row,ENTITY_STATE_KIND_COLUMN)->setText(arguments.kind.c_str());
    instances->item(row,HANDLER_ID_COLUMN)->setText(arguments.handler.c_str());
    instances->item(row,NUM_SUBS_COLUMN)->setText(boost::lexical_cast<std::string>(arguments.numSubscribers).c_str());
}

void Safir::Dob::Internal::StatisticsCollector(Safir::Dob::Internal::StateContainer& stateContainer, void* arg)
{
    Arguments& arguments = *static_cast<Arguments*>(arg);
    stateContainer.ForEachState(boost::bind(&EntityStats::ProcessState,
                                            arguments._this,
                                            _1,
                                            _2,
                                            boost::ref(arguments)),
                                true);
}


void EntityStats::UpdateStatistics()
{
    instances->setSortingEnabled(false);
    information->clear();
    assert(m_removeInstances.empty());

    for (int i = 0; i < instances->rowCount(); ++i)
    {
        m_removeInstances.insert(instances->item(i,INSTANCE_ID_COLUMN));
    }

    Arguments arg;
    arg.typeId = m_typeId;
    arg._this = this;
    StatisticsCollector(Safir::Dob::Internal::EntityTypes::Instance(), &arg);

    numStates->setText(boost::lexical_cast<std::string>(arg.numStates).c_str());
    numDowngraded->setText(boost::lexical_cast<std::string>(arg.downgraded).c_str());
    numReal->setText(boost::lexical_cast<std::string>(arg.realStates).c_str());
    numGhost->setText(boost::lexical_cast<std::string>(arg.ghostStates).c_str());
    numInjections->setText(boost::lexical_cast<std::string>(arg.injectionStates).c_str());

    while(!m_removeInstances.empty())
    {
        const int row = (*m_removeInstances.begin())->row();
        delete instances->item(row,CHECK_COLUMN);
        delete instances->item(row,INSTANCE_ID_COLUMN);
        delete instances->item(row,CONNECTION_COLUMN);
        delete instances->item(row,ENTITY_STATE_KIND_COLUMN);
        delete instances->item(row,HANDLER_ID_COLUMN);
        delete instances->item(row,NUM_SUBS_COLUMN);

        instances->removeRow(row);
        m_removeInstances.erase(m_removeInstances.begin());
    }

    instances->resizeColumnsToContents();
    instances->setSortingEnabled(true);
}

