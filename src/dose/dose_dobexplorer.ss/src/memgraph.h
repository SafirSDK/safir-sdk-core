/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
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
#ifndef MEMGRAPH_H
#define MEMGRAPH_H
#include "common_header.h"
#include "graphwidget.h"
#include "ui_memgraph.h"
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <boost/noncopyable.hpp>

class MemGraph :
  public QWidget,
  private Ui::MemoryGraph,
  public Safir::Dob::Internal::SharedMemoryObject,
  private boost::noncopyable
{
    Q_OBJECT

public:
    MemGraph(QWidget *parent = 0);


public slots:
    void PeriodChanged(double newPeriod);
    void Timeout();
    void HistoryChanged(double newValue);
    void ScaleChanged(int newValue);
private:
    QTimer m_timer;
    const size_t m_capacity;
};


#endif
