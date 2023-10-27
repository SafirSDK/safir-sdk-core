#include "connectionstatsaggregated.h"
#include "ui_connectionstatsaggregated.h"
#include "ConnectionStatisticsCollector.h"

#include <Safir/Dob/Internal/Connections.h>

namespace
{
    // Create read-only table item
    QTableWidgetItem* TableItem(QString text, bool alignCenter = false)
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
}

ConnectionStatsAggregated::ConnectionStatsAggregated(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::connectionstatsaggregated),
    m_timer(this)
{
    ui->setupUi(this);

    ui->tableWidget->resizeRowsToContents();
    ui->tableWidget->resizeColumnsToContents();
    ui->tableWidget->setAlternatingRowColors(true);

    connect(&m_timer, SIGNAL(timeout()), this, SLOT(Update()));
    m_timer.start(3000);
    Update();
    ui->tableWidget->resizeColumnsToContents();
}

ConnectionStatsAggregated::~ConnectionStatsAggregated()
{
    delete ui;
}


void ConnectionStatsAggregated::Update()
{
    std::vector<std::pair<QString, ConnectionStatisticsCollector::Stat>> connectionStatistics;

    Safir::Dob::Internal::Connections::Instance().ForEachConnectionPtr([&connectionStatistics](const Safir::Dob::Internal::ConnectionPtr& con)
    {
        auto& stat = connectionStatistics.emplace_back();
        stat.first = con->NameWithCounter();
        ConnectionStatisticsCollector::GetStatistics(con, stat.second);
    });


    ui->tableWidget->setSortingEnabled(false);
    ui->tableWidget->setRowCount(static_cast<int>(connectionStatistics.size()));
    int row = 0;
    for (const auto& s : connectionStatistics)
    {
        auto reqInQ = GetReqInQAccumulated(s.second.reqInQStat);
        auto msgInQ = GetMsgInQAccumulated(s.second.msgInQStat);

        ui->tableWidget->setItem(row, 0, TableItem(s.first));
        ui->tableWidget->setItem(row, 1, TableItem(QString::number(s.second.reqOutQStat.noPushedRequests), true));
        ui->tableWidget->setItem(row, 2, TableItem(QString::number(s.second.reqOutQStat.noOverflows), true));
        ui->tableWidget->setItem(row, 3, TableItem(QString::number(s.second.reqOutQStat.noTimeouts), true));
        ui->tableWidget->setItem(row, 4, TableItem(QString::number(reqInQ.first), true));
        ui->tableWidget->setItem(row, 5, TableItem(QString::number(reqInQ.second), true));
        ui->tableWidget->setItem(row, 6, TableItem(QString::number(s.second.msgOutQStat.noPushedMsg), true));
        ui->tableWidget->setItem(row, 7, TableItem(QString::number(s.second.msgOutQStat.noOverflows), true));
        ui->tableWidget->setItem(row, 8, TableItem(QString::number(msgInQ.first), true));
        ui->tableWidget->setItem(row, 9, TableItem(QString::number(msgInQ.second), true));

        ++row;
    }
    ui->tableWidget->setSortingEnabled(true);

}
