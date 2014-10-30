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
#ifndef NUMBEROFENTITIES_H
#define NUMBEROFENTITIES_H
#include "common_header.h"
#include "internalfunctions.h"
#include "ui_numberOfEntities.h"
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/StateDeleter.h>

class NumberOfEntities :
  public QWidget,
  private Ui::NumberOfEntities,
  private Safir::Dob::Internal::IStatisticsCollector
{
    Q_OBJECT

public:
    NumberOfEntities(QWidget *parent = 0);

    // override Safir::Dob::Internal::IStatisticsCollector
    void ProcessState(const Safir::Dob::Typesystem::Int64 instance,
        const Safir::Dob::Internal::StateSharedPtr& statePtr,
        Safir::Dob::Internal::Arguments& arguments);
    void AddContextRow(Safir::Dob::Internal::EntityType& entityType, Safir::Dob::Internal::Arguments& argumentsXS) {};
    void InitRemoveInstances(Safir::Dob::Internal::Arguments& arguments)  {};
    void RemoveInstances()  {};
    void AddContextGlobalData(Safir::Dob::Internal::Arguments& arguments)  {};


public slots:
    void Update();

private:
    void GetEntities();

    QTimer m_timer;

};


#endif // NUMBEROFENTITIES_H
