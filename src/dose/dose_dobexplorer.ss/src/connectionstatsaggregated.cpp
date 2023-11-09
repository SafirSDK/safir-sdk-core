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
#include "connectionstatsaggregated.h"
#include "ui_connectionstatsaggregated.h"
#include "ConnectionStatisticsCollector.h"

#include <QFile>
#include <QFileDialog>
#include <QMessageBox>
#include <QtConcurrent/QtConcurrent>

#include <Safir/Dob/Internal/Connections.h>

#include <Safir/Control/GetConnectionStatisticsAllNodes.h>
#include <Safir/Control/ConnectionStatisticsAllNodesResponse.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Utilities/Internal/Expansion.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/Serialization.h>

namespace
{
    // Create read-only table item
    QTableWidgetItem* TableItem(const QString& text, bool alignCenter = false)
    {
        auto item = new QTableWidgetItem(text);
        item->setFlags(item->flags() &  ~Qt::ItemIsEditable);
        if (alignCenter)
        {
            item->setTextAlignment(Qt::AlignCenter);
        }
        return item;
    }

    std::pair<int, int> GetReqInQAccumulated(const std::vector<ConnectionStatisticsCollector::ReqQStat>& reqInQStat)
    {
        int recv = 0;
        int overflows = 0;
        for (const auto& v : reqInQStat)
        {
            recv += v.noPushedRequests;
            overflows += v.noOverflows;
        }
        return std::make_pair(recv, overflows);
    }

    std::pair<int, int> GetMsgInQAccumulated(const std::vector<ConnectionStatisticsCollector::MsgQStat>& msgQStat)
    {
        int recv = 0;
        int overflows = 0;
        for (const auto& v : msgQStat)
        {
            recv += v.noPushedMsg;
            overflows += v.noOverflows;
        }
        return std::make_pair(recv, overflows);
    }

    QString ShortConnectionName(const std::string& connectionName)
    {
        return QString::fromStdString(std::string(std::find(std::find(connectionName.begin(), connectionName.end(), ';') + 1, connectionName.end(), ';') + 1, connectionName.end()));
    }

    QString ShortConnectionName(const std::wstring& connectionName)
    {
        return QString::fromStdWString(std::wstring(std::find(std::find(connectionName.begin(), connectionName.end(), ';') + 1, connectionName.end(), ';') + 1, connectionName.end()));
    }

    void TableToCsv(const QString& file, QTableWidget * table)
    {
        QFile f(file);

        if (f.open(QFile::WriteOnly | QFile::Truncate))
        {
            QTextStream data(&f);
            QStringList strList;

            // Write column headers
            for(int col = 0; col < table->columnCount(); ++col)
            {
                strList << table->horizontalHeaderItem(col)->data(Qt::DisplayRole).toString();
            }
            data << strList.join(",") << "\n";

            // Write data rows
            for( int row= 0; row < table->rowCount(); ++row )
            {
                strList.clear();
                for (int col = 0; col < table->columnCount(); ++col)
                {
                    strList << table->item(row, col)->text();
                }
                data << strList.join( "," ) + "\n";
            }
            f.close();
        }
    }
}

ConnectionStatsAggregated::ConnectionStatsAggregated(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::connectionstatsaggregated),
    m_updateLocalTimer(this)
{
    ui->setupUi(this);

    ui->statusLabel->clear();

    ui->tableWidget->resizeRowsToContents();
    ui->tableWidget->resizeColumnsToContents();
    ui->tableWidget->setAlternatingRowColors(true);

    connect(ui->saveButton, &QPushButton::clicked, this, &ConnectionStatsAggregated::SaveToFile);
    connect(ui->remoteNodesCheckbox, &QCheckBox::toggled, this, &ConnectionStatsAggregated::RemoteNodesCheckbox);

    connect(this, SIGNAL(DispatchSignal()), this, SLOT(Dispatch()));
    connect(this, SIGNAL(ConnectedToDobSignal()), this, SLOT(ConnectedToDob()));

    connect(&m_updateLocalTimer, &QTimer::timeout, this, &ConnectionStatsAggregated::UpdateGuiLocal);
    if (!ui->remoteNodesCheckbox->isChecked())
    {
        m_updateLocalTimer.start(3000);
        UpdateGuiLocal();
    }

    ui->tableWidget->resizeColumnsToContents();
}

ConnectionStatsAggregated::~ConnectionStatsAggregated()
{
    if (m_dobConnection.IsOpen())
    {
        m_dobConnection.Close();
    }
    delete ui;
}

void ConnectionStatsAggregated::ConnectToDob()
{
    // Connect to DOB in another thread. Signals the ConnectedToDobSignal when done.
    ui->statusLabel->setText("Trying to connect to DOB...");
    auto dummy = QtConcurrent::run([this]
    {
        int instancePart = 0;
        while (true)
        {
            try
            {
                m_dobConnection.Open(L"DobExplorer", std::to_wstring(instancePart), 0, this, this);
                break;
            }
            catch (const Safir::Dob::NotOpenException&)
            {
                ++instancePart;
            }
        }

        emit ConnectedToDobSignal();
    });
}

void ConnectionStatsAggregated::SendStatisticsRequest()
{
    try
    {
        auto request = Safir::Control::GetConnectionStatisticsAllNodes::Create();
        m_dobConnection.ServiceRequest(request, Safir::Dob::Typesystem::HandlerId(), this);
    }
    catch (const Safir::Dob::OverflowException&){ /*Retry in OnNotOverflow*/ }
    catch (const Safir::Dob::NotOpenException&)
    {
        // not connected to dob anymore
        ui->remoteNodesCheckbox->setChecked(false);
    }
}

//---------------------------------------------------------
// SLOTS
//---------------------------------------------------------
void ConnectionStatsAggregated::UpdateGuiLocal()
{
    std::vector<ConnectionStatisticsCollector::Stat> connectionStatistics;

    Safir::Dob::Internal::Connections::Instance().ForEachConnectionPtr([&connectionStatistics](const Safir::Dob::Internal::ConnectionPtr& con)
    {
        if (con->IsLocal())
        {
            connectionStatistics.emplace_back();
            auto& stat = connectionStatistics.back();
            ConnectionStatisticsCollector::GetStatistics(con, stat);
        }
    });

    ui->tableWidget->setSortingEnabled(false);
    ui->tableWidget->setRowCount(static_cast<int>(connectionStatistics.size()));
    int row = 0;
    for (const auto& s : connectionStatistics)
    {
        auto reqInQ = GetReqInQAccumulated(s.reqInQStat);
        auto msgInQ = GetMsgInQAccumulated(s.msgInQStat);

        ui->tableWidget->setItem(row, 0, TableItem(QString::fromStdWString(Safir::Dob::ThisNodeParameters::Name())));
        ui->tableWidget->setItem(row, 1, TableItem(ShortConnectionName(s.connectionName)));
        ui->tableWidget->setItem(row, 2, TableItem(QString::number(s.reqOutQStat.noPushedRequests), true));
        ui->tableWidget->setItem(row, 3, TableItem(QString::number(s.reqOutQStat.noOverflows), true));
        ui->tableWidget->setItem(row, 4, TableItem(QString::number(s.reqOutQStat.noTimeouts), true));
        ui->tableWidget->setItem(row, 5, TableItem(QString::number(reqInQ.first), true));
        ui->tableWidget->setItem(row, 6, TableItem(QString::number(reqInQ.second), true));
        ui->tableWidget->setItem(row, 7, TableItem(QString::number(s.msgOutQStat.noPushedMsg), true));
        ui->tableWidget->setItem(row, 8, TableItem(QString::number(s.msgOutQStat.noOverflows), true));
        ui->tableWidget->setItem(row, 9, TableItem(QString::number(msgInQ.first), true));
        ui->tableWidget->setItem(row, 10, TableItem(QString::number(msgInQ.second), true));

        ++row;
    }
    ui->tableWidget->setSortingEnabled(true);
}

void ConnectionStatsAggregated::Dispatch()
{
    m_dobConnection.Dispatch();
}

void ConnectionStatsAggregated::ConnectedToDob()
{
    m_updateLocalTimer.stop(); // Stop doing the local update. All data, including this node, will be received from the service request.

    ui->statusLabel->setText("Connected to DOB, SAFIR_INSTANCE=" + QString::number(Safir::Utilities::Internal::Expansion::GetSafirInstance()));
    QTimer::singleShot(1500, [this]{ SendStatisticsRequest(); });
}

void ConnectionStatsAggregated::RemoteNodesCheckbox(bool checked)
{
    if (checked)
    {
        ConnectToDob();
    }
    else
    {
        // If checkbox was unchecked, disconnect from DOB
        if (m_dobConnection.IsOpen())
        {
            m_dobConnection.Close();
        }
        ui->statusLabel->clear();
        UpdateGuiLocal();
        m_updateLocalTimer.start(3000);
        return;
    }
}

void ConnectionStatsAggregated::SaveToFile()
{
    auto file = QFileDialog::getSaveFileName(this, "Save table as csv");
    TableToCsv(file, ui->tableWidget);
}

//---------------------------------------------------------
// DOB callbacks
//---------------------------------------------------------
void ConnectionStatsAggregated::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
{
    if (!ui->remoteNodesCheckbox->isChecked())
    {
        return;
    }
    if (responseProxy.IsSuccess())
    {
        auto resp = std::dynamic_pointer_cast<Safir::Control::ConnectionStatisticsAllNodesResponse>(responseProxy.GetResponse());
        if (!resp || resp->NodeConnectionStatistics().IsNull())
        {
            return;
        }

        int numRows = 0;
        for (const auto& n : resp->NodeConnectionStatistics())
        {
            numRows += n->ConnectionStatistics().size();
        }

        ui->tableWidget->setSortingEnabled(false);
        ui->tableWidget->setRowCount(numRows);
        int row = 0;
        for (const auto& n : resp->NodeConnectionStatistics())
        {
            for (const auto& s : n->ConnectionStatistics())
            {
                // Fill table with statistics
                ui->tableWidget->setItem(row, 0, TableItem(QString::fromStdWString(n->NodeName())));
                ui->tableWidget->setItem(row, 1, TableItem(ShortConnectionName(s->ConnectionName().GetVal())));
                ui->tableWidget->setItem(row, 2, TableItem(QString::number(s->NumberOfSentRequests()), true));
                ui->tableWidget->setItem(row, 3, TableItem(QString::number(s->NumberOfSendRequestOverflows()), true));
                ui->tableWidget->setItem(row, 4, TableItem(QString::number(s->NumberOfSendRequestTimeouts()), true));
                ui->tableWidget->setItem(row, 5, TableItem(QString::number(s->NumberOfReceivedRequests()), true));
                ui->tableWidget->setItem(row, 6, TableItem(QString::number(s->NumberOfReceiveRequestOverflows()), true));
                ui->tableWidget->setItem(row, 7, TableItem(QString::number(s->NumberOfSentMessages()), true));
                ui->tableWidget->setItem(row, 8, TableItem(QString::number(s->NumberOfSendMessageOverflows()), true));
                ui->tableWidget->setItem(row, 9, TableItem(QString::number(s->NumberOfReceivedMessages()), true));
                ui->tableWidget->setItem(row, 10, TableItem(QString::number(s->NumberOfReceiveMessageOverflows()), true));
                ++row;
            }
        }
        ui->tableWidget->resizeColumnsToContents();
        ui->tableWidget->setSortingEnabled(true);

        QTimer::singleShot(6000, [this]{ SendStatisticsRequest(); });
    }
    else
    {
        auto err = std::dynamic_pointer_cast<Safir::Dob::ErrorResponse>(responseProxy.GetResponse());
        if (err && !err->Code().IsNull() && err->Code() == Safir::Dob::ResponseGeneralErrorCodes::SafirNotRegistered())
        {
            QMessageBox msgBox;
            msgBox.setText("'GetConnectionStatisticsAllNodes' not registered! Is safir_status running?");
            msgBox.exec();
            QTimer::singleShot(0, [this]{ ui->remoteNodesCheckbox->setChecked(false); });
        }
        else
        {
            QMessageBox msgBox;
            msgBox.setText(tr("Error while updating remote node statistics!\n\n") +
                           QString::fromStdWString(Safir::Dob::Typesystem::Serialization::ToJson(responseProxy.GetResponse())));
            msgBox.exec();
            QTimer::singleShot(0, [this]{ ui->remoteNodesCheckbox->setChecked(false); });
        }
    }
}

void ConnectionStatsAggregated::OnNotRequestOverflow()
{
    SendStatisticsRequest();
}

void ConnectionStatsAggregated::OnDoDispatch()
{
    emit DispatchSignal();
}

void ConnectionStatsAggregated::OnStopOrder()
{
    QTimer::singleShot(0, [this]{ ui->remoteNodesCheckbox->setChecked(false); });
}
