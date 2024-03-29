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
#ifndef GRAPHWIDGET_H
#define GRAPHWIDGET_H
#include "common_header.h"
#include <map>

class GraphWidget : public QFrame
{
    Q_OBJECT

public:
    GraphWidget(QWidget *parent = 0);

    void AddData(const QDateTime& time, const double value);

    void SetHistoryLength(const int seconds);

    void SetVerticalScale(const double scale);

protected:
    void paintEvent(QPaintEvent* event) override;

private:
    void PurgeOldData();


    typedef std::map<QDateTime, double> PlotData;

    PlotData m_data;
    int m_historySeconds;
    double m_scale;
};


#endif
