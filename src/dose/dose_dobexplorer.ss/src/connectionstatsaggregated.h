/******************************************************************************
*
* Copyright Saab AB, 2023 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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
#ifndef CONNECTIONSTATSAGGREGATED_H
#define CONNECTIONSTATSAGGREGATED_H

#include <QWidget>
#include <QTimer>

#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Consumer.h>

namespace Ui {
class connectionstatsaggregated;
}

class ConnectionStatsAggregated : public QWidget,
        public Safir::Dob::Dispatcher,
        public Safir::Dob::StopHandler,
        public Safir::Dob::Requestor
{
    Q_OBJECT

public:
    explicit ConnectionStatsAggregated(QWidget *parent = nullptr);
    ~ConnectionStatsAggregated();

signals:
    void ConnectedToDobSignal();
    void DispatchSignal();

private slots:
    void UpdateGuiLocal();
    void RemoteNodesCheckbox(bool checked);
    void SaveToFile();
    void ConnectedToDob();
    void Dispatch();

private:
    Ui::connectionstatsaggregated *ui;
    QTimer m_updateLocalTimer;

    Safir::Dob::Connection m_dobConnection;

    void OnDoDispatch() override;
    void OnStopOrder() override;
    void OnResponse(const Safir::Dob::ResponseProxy responseProxy) override;
    void OnNotRequestOverflow() override;

    void ConnectToDob();
    void SendStatisticsRequest();
};

#endif // CONNECTIONSTATSAGGREGATED_H
