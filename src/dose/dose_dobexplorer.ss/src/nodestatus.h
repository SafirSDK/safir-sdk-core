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
#ifndef NODESTATUS_H
#define NODESTATUS_H
#include "common_header.h"
#include "ui_nodestatus.h"

class NodeStatus :
  public QWidget,
  private Ui::NodeStatus
{
    Q_OBJECT

public:
    NodeStatus(QWidget * parent = 0);


public slots:
    void UpdateTable();
private:

    QTimer m_timer;

    QBrush m_defaultBrush;
    QBrush m_yellowBrush;
    QBrush m_redBrush;
    QBrush m_greenBrush;
    QBrush m_blueBrush;
    QBrush m_grayBrush;

};


#endif
