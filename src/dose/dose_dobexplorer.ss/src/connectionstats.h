/******************************************************************************
*
* Copyright Saab AB, 2008-2023 (http://safirsdkcore.com)
*
* Created by: Anders Widén / stawi
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
#ifndef CONNECTIONSTATS_H
#define CONNECTIONSTATS_H
#include "common_header.h"
#include "ui_connectionstats.h"

#include <Safir/Dob/Internal/Connection.h>

struct Stat;

class ConnectionStats :
  public QWidget,
  private Ui::ConnectionStatistics
{
    Q_OBJECT

public:
    ConnectionStats(QWidget *parent, const QString& connectionName);


public slots:
    void UpdateStatistics(const bool ignoreVisible = false);
private:

    QTimer m_timer;
    Safir::Dob::Internal::ConnectionId m_connectionId;
};


#endif
