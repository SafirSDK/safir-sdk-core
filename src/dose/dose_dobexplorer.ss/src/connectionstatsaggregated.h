#ifndef CONNECTIONSTATSAGGREGATED_H
#define CONNECTIONSTATSAGGREGATED_H

#include <QWidget>
#include <QTimer>

namespace Ui {
class connectionstatsaggregated;
}

class ConnectionStatsAggregated : public QWidget
{
    Q_OBJECT

public:
    explicit ConnectionStatsAggregated(QWidget *parent = nullptr);
    ~ConnectionStatsAggregated();

public slots:
    void Update();

private:
    struct ConnectionsStats
    {
        QString name;
        int sentReq;
        int sentReqOverflows;
        int sentReqTimeouts;
        int handledReq;
        int handledReqOverflows;
        int msgSent;
        int msgSentOverflows;
        int msgRecv;
        int msgRecvOverflows;
    };

    Ui::connectionstatsaggregated *ui;
    QTimer m_timer;
};

#endif // CONNECTIONSTATSAGGREGATED_H
