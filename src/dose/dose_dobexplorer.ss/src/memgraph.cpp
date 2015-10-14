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

#include "common_header.h"
#include "memgraph.h"
#include <sstream>
#include <math.h>

MemGraph::MemGraph(QWidget* /*parent*/):
    m_timer(this),
    m_capacity(GetSharedMemory().get_size())
{
    setupUi(this); // this sets up GUI

    // signals/slots mechanism in action
    connect(historyLength, SIGNAL(valueChanged(double)), this, SLOT(HistoryChanged(double)) );

    connect(updatePeriod, SIGNAL(valueChanged(double)), this, SLOT(PeriodChanged(double)));
    connect(verticalScale, SIGNAL(valueChanged(int)), this, SLOT(ScaleChanged(int)));
    connect(&m_timer, SIGNAL(timeout()), this, SLOT(Timeout()));
    m_timer.setSingleShot(true);
    m_timer.start(static_cast<int>(updatePeriod->value()*1000));
    graph->SetHistoryLength(static_cast<int>(historyLength->value()*60));

    total->setText(QString::number(m_capacity / 1024 / 1024));
}


void MemGraph::HistoryChanged(double newValue)
{
    graph->SetHistoryLength(static_cast<int>(newValue*60));
}

void MemGraph::Timeout()
{
    m_timer.start(static_cast<int>(updatePeriod->value()*1000));
    const size_t allocated = m_capacity - GetSharedMemory().get_free_memory();
    const double ratio = allocated/(double)m_capacity;
    graph->AddData(QDateTime::currentDateTime(),ratio);

    current->setText(QString::number(allocated / 1024 /1024));
    currentPercent->setText(QString::number(static_cast<int>(ratio * 100)));
    
    std::ostringstream ostr;
    ostr.precision(2);
    ostr << static_cast<double>((allocated)/(double)m_capacity)*100 << " %" << " (" << (allocated)/1048576 << "/" << m_capacity/1048576 << "Mb)";

    memLabel->setText(ostr.str().c_str());
}

void MemGraph::PeriodChanged(double newPeriod)
{
    m_timer.stop();
    m_timer.start(static_cast<int>(newPeriod*1000));
}

void MemGraph::ScaleChanged(int newValue)
{
    std::ostringstream ostr;
    ostr << newValue << " %";
    scalePercent->setText(ostr.str().c_str());
    graph->SetVerticalScale(newValue/100.0);
}
