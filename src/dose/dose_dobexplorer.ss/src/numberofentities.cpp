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
#include "numberofentities.h"
#include "common_header.h"
#include "dosecom_stuff.h"
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/EntityType.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Qt>

const int NAME_COLUMN = 0;
const int CONTEXT_NUM_STATES_COLUMN = 1;
const int CONTEXT_NUM_DOWNGRADED_COLUMN = 2;
const int CONTEXT_NUM_SUBSCRIBERS_COLUMN = 3;
const int CONTEXT_NUM_REAL_STATES_COLUMN = 4;
const int CONTEXT_NUM_GHOST_STATES_COLUMN = 5;
const int CONTEXT_NUM_INJECTION_STATES_COLUMN = 6;


NumberOfEntities::NumberOfEntities(QWidget * /*parent*/):
m_timer(this)
{
    setupUi(this); // this sets up GUI

    connect(&m_timer, SIGNAL(timeout()), this, SLOT(Update()));
    m_timer.start(3000);

    GetEntities();

    Safir::Dob::Internal::EntityTypes::Initialize();

    Update();
}

void NumberOfEntities::Update()
{
    Safir::Dob::Internal::EntityTypes &entityTypes = Safir::Dob::Internal::EntityTypes::Instance();
    Safir::Dob::Internal::Arguments arguments;
    arguments.simple = true;
    arguments.getInfo = false;
    arguments.context = 0;

    for (int row=0; row<entityTable->rowCount(); row++)
    {
        QTableWidgetItem *item = entityTable->item(row,NAME_COLUMN); 
        arguments.ResetOutParam();
        arguments.typeId = (Safir::Dob::Typesystem::TypeId)item->data(Qt::UserRole).toLongLong();
        arguments._this = this;
        StatisticsCollector(entityTypes, &arguments);

        entityTable->item(row,CONTEXT_NUM_STATES_COLUMN)->setData(Qt::DisplayRole ,static_cast<qulonglong>(arguments.numStates));
        entityTable->item(row,CONTEXT_NUM_DOWNGRADED_COLUMN)->setData(Qt::DisplayRole ,static_cast<qulonglong>(arguments.downgraded));
        entityTable->item(row,CONTEXT_NUM_SUBSCRIBERS_COLUMN)->setData(Qt::DisplayRole ,arguments.numSubscribers);
        entityTable->item(row,CONTEXT_NUM_REAL_STATES_COLUMN)->setData(Qt::DisplayRole ,static_cast<qulonglong>(arguments.realStates));
        entityTable->item(row,CONTEXT_NUM_GHOST_STATES_COLUMN)->setData(Qt::DisplayRole ,static_cast<qulonglong>(arguments.ghostStates));
        entityTable->item(row,CONTEXT_NUM_INJECTION_STATES_COLUMN)->setData(Qt::DisplayRole ,static_cast<qulonglong>(arguments.injectionStates));

    }
}

void NumberOfEntities::GetEntities()
{
    using namespace Safir::Dob::Typesystem;
    using namespace Safir::Dob::Typesystem::Operations;
    using namespace Safir::Dob::Typesystem::Utilities;

    TypeIdVector ent = GetClassTree(Safir::Dob::Entity::ClassTypeId);

    int i = 0;
    for (TypeIdVector::iterator it = ent.begin(); it != ent.end(); ++it)
    {
        if (*it == Safir::Dob::Entity::ClassTypeId)
        {
            continue;
        }
        else
        {
            const std::wstring name = GetName(*it);
            entityTable->insertRow(i);
            QTableWidgetItem *item = new QTableWidgetItem(ToUtf8(name).c_str());
            item->setData(Qt::UserRole, static_cast<qlonglong>(*it));
            entityTable->setItem(i, NAME_COLUMN, item);
            entityTable->setItem(i, CONTEXT_NUM_STATES_COLUMN, new QTableWidgetItem());
            entityTable->setItem(i, CONTEXT_NUM_DOWNGRADED_COLUMN, new QTableWidgetItem());            
            entityTable->setItem(i, CONTEXT_NUM_SUBSCRIBERS_COLUMN, new QTableWidgetItem());
            entityTable->setItem(i, CONTEXT_NUM_REAL_STATES_COLUMN, new QTableWidgetItem());
            entityTable->setItem(i, CONTEXT_NUM_GHOST_STATES_COLUMN, new QTableWidgetItem());
            entityTable->setItem(i, CONTEXT_NUM_INJECTION_STATES_COLUMN, new QTableWidgetItem());

            ++i;
        }
    }

    entityTable->resizeColumnsToContents();

}

void NumberOfEntities::ProcessState(const Safir::Dob::Typesystem::Int64 /*instance*/,
                                const Safir::Dob::Internal::StateSharedPtr& statePtr,
                               Safir::Dob::Internal::Arguments& arguments)
{
    ++arguments.numStates;
    if (statePtr->IsReleased())
    {
        ++arguments.downgraded;
    }

    StatisticsCollector(*statePtr, &arguments);
}
