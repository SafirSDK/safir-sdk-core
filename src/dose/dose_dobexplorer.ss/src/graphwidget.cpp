/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagstr�m / stlrha
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

#include "graphwidget.h"
#include <algorithm>
#include <QPaintEvent>
#include <QDateTime>
#include <QPainter>
#include <QPainterPath>

GraphWidget::GraphWidget(QWidget * parent):
    QFrame(parent),
    m_historySeconds(20*60),
    m_scale(1.0)
{

}

void GraphWidget::paintEvent(QPaintEvent* event)
{
    QFrame::paintEvent(event);

    if (m_data.empty())
    {
        return;
    }
    QPainter painter(this);

    painter.setClipRect(contentsRect());
    painter.setViewport(contentsRect());

    const QDateTime now = QDateTime::currentDateTime();
    const QDateTime start = now.addSecs(-m_historySeconds);

    painter.translate(0,height());
    painter.scale(1.0/m_historySeconds*width(),-1.0*height()/m_scale);

    QPainterPath path;

    path.moveTo(static_cast<qreal>(start.secsTo(m_data.begin()->first)), m_data.begin()->second);
    for (PlotData::iterator it = ++m_data.begin();
         it != m_data.end(); ++it)
    {
        path.lineTo(static_cast<qreal>(start.secsTo(it->first)),it->second);
    }

    painter.drawPath(path);
}

void GraphWidget::AddData(const QDateTime& time, const float value)
{
    m_data.insert(std::make_pair(time,value));
    PurgeOldData();
    update();
}

void GraphWidget::SetHistoryLength(const int seconds)
{
    m_historySeconds = seconds;
    PurgeOldData();
    update();
}

void GraphWidget::PurgeOldData()
{
    const QDateTime limit = QDateTime::currentDateTime().addSecs(-m_historySeconds);
    m_data.erase(m_data.begin(),m_data.upper_bound(limit));
}

void GraphWidget::SetVerticalScale(const float scale)
{
    m_scale = scale; update();
}
