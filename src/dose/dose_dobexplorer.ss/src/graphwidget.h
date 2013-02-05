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
#ifndef GRAPHWIDGET_H
#define GRAPHWIDGET_H
#include "common_header.h"
#include <map>

class GraphWidget : public QFrame
{
    Q_OBJECT

public:
    GraphWidget(QWidget *parent = 0);

    void AddData(const QDateTime& time, const float value);

    void SetHistoryLength(const int seconds);

    void SetVerticalScale(const float scale);

protected:
    void paintEvent(QPaintEvent* event);

private:
    void PurgeOldData();


    typedef std::map<QDateTime, float> PlotData;

    PlotData m_data;
    int m_historySeconds;
    float m_scale;
};


#endif
