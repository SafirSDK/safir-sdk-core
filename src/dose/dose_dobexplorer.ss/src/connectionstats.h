/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
#include "ui_connectionstats.h"
#include <QtGui>

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
    void UpdateStatistics();
private:

    QTimer m_timer;
    Safir::Dob::Internal::ConnectionId m_connectionId;

    static void ProcessConnection(const Safir::Dob::Internal::ConnectionPtr& connection, Stat& stat, bool& exist);
    static void ProcessReqInQ(const Safir::Dob::Internal::ConsumerId& consumer,
                              Safir::Dob::Internal::RequestInQueue&   queue,
                              Stat&                                   stat);
    static void ProcessMsgInQ(const Safir::Dob::Internal::ConsumerId& consumer,
                              Safir::Dob::Internal::MessageQueue&     queue,
                              Stat&                                   stat);
};


#endif
