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
#ifndef DOSECOMINFO_H
#define DOSECOMINFO_H
#include "common_header.h"
#include "ui_dosecominfo.h"

class DoseComInfo :
  public QWidget,
  private Ui::DoseComInfo
{
    Q_OBJECT

public:
    DoseComInfo(QWidget *parent = 0);


public slots:
    void Update();
    void DebugLevelChanged(double newValue);
private:

    QTimer m_timer;

};


#endif
