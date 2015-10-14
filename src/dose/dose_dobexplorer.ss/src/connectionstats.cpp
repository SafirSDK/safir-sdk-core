/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
*
* Created by: Anders Widén / stawi
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
#include "connectionstats.h"
//#include <iostream>
#include <sstream>
#include <math.h>

#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/RequestOutQueue.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4702)
#endif

#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif


struct ReqQStat
{
    Safir::Dob::Typesystem::Int32 noPushedRequests;
    Safir::Dob::Typesystem::Int32 noOverflows;
    Safir::Dob::Typesystem::Int32 noDispatchedRequests;
    Safir::Dob::Typesystem::Int32 noAttachedResponses;
    Safir::Dob::Typesystem::Int32 noDispatchedResponses;
    size_t capacity;
    size_t size;

    Safir::Dob::Internal::ConsumerId    consumerId; // valid only for in queues
    Safir::Dob::Typesystem::Int32       noTimeouts; // valid only for out queues
};

struct MsgQStat
{
    Safir::Dob::Typesystem::Int32 noPushedMsg;
    Safir::Dob::Typesystem::Int32 noOverflows;
    size_t capacity;
    size_t size;

    Safir::Dob::Internal::ConsumerId    consumerId; // valid only for in queues
};

struct Stat
{
    ReqQStat                    reqOutQStat;
    std::vector<ReqQStat>       reqInQStat;
    MsgQStat                    msgOutQStat;
    std::vector<MsgQStat>       msgInQStat;

};

inline void Safir::Dob::Internal::StatisticsCollector(Safir::Dob::Internal::RequestOutQueue& requestOutQueue, void* ptr)
{
    ReqQStat* p = static_cast<ReqQStat*>(ptr);

    p->noTimeouts = requestOutQueue.m_noTimeouts;
    p->noPushedRequests = requestOutQueue.m_noPushedRequests;
    p->noOverflows = requestOutQueue.m_noOverflows;
    p->noDispatchedRequests = requestOutQueue.m_noDispatchedRequests;
    p->noAttachedResponses = requestOutQueue.m_noAttachedResponses;
    p->noDispatchedResponses = requestOutQueue.m_noDispatchedResponses;
    p->size = requestOutQueue.size();
    p->capacity = requestOutQueue.capacity();
}

inline void Safir::Dob::Internal::StatisticsCollector(Safir::Dob::Internal::MessageQueue& messageQueue, void* ptr)
{
    MsgQStat* p = static_cast<MsgQStat*>(ptr);

    p->noPushedMsg = messageQueue.m_noPushed;
    p->noOverflows = messageQueue.m_noOverflows;
    p->size = messageQueue.size();
    p->capacity = messageQueue.capacity();
}

inline void Safir::Dob::Internal::StatisticsCollector(Safir::Dob::Internal::RequestInQueue& requestInQueue, void* ptr)
{
    ReqQStat* p = static_cast<ReqQStat*>(ptr);

    p->noPushedRequests = requestInQueue.m_noPushedRequests;
    p->noOverflows = requestInQueue.m_noOverflows;
    p->noDispatchedRequests = requestInQueue.m_noDispatchedRequests;
    p->noAttachedResponses = requestInQueue.m_noAttachedResponses;
    p->noDispatchedResponses = requestInQueue.m_noDispatchedResponses;
    p->size = requestInQueue.size();
    p->capacity = requestInQueue.capacity();
}


ConnectionStats::ConnectionStats(QWidget* /*parent*/,  const QString& connectionName):
    m_timer(this),
    m_connectionId()
{
    setupUi(this); // this sets up GUI
    missingConnectionLabel->hide();

    // Adjust the look of the request out queue table.
    reqOutQGroupBox->layout()->setAlignment(reqOutQTableWidget, Qt::AlignTop);

    reqOutQTableWidget->setRowCount(1);
    reqOutQTableWidget->verticalHeader()->hide();
    reqOutQTableWidget->resizeRowsToContents();
    reqOutQTableWidget->resizeColumnsToContents();
    reqOutQTableWidget->setAlternatingRowColors(true);

    for (int col = 0; col < reqOutQTableWidget->columnCount(); ++col)
    {
        QTableWidgetItem *newItem = new QTableWidgetItem("");
        newItem->setTextAlignment(Qt::AlignHCenter);
        reqOutQTableWidget->setItem(0, col, newItem);
    }

    // Adjust the look of the request in queues table.
    reqInQGroupBox->layout()->setAlignment(reqInQTableWidget, Qt::AlignTop);

    reqInQTableWidget->verticalHeader()->hide();
    reqInQTableWidget->resizeRowsToContents();
    reqInQTableWidget->resizeColumnsToContents();
    reqInQTableWidget->setAlternatingRowColors(true);

    // Adjust the look of the message out queue table.
    msgOutQGroupBox->layout()->setAlignment(msgOutQTableWidget, Qt::AlignTop);

    msgOutQTableWidget->setRowCount(1);
    msgOutQTableWidget->verticalHeader()->hide();
    msgOutQTableWidget->resizeRowsToContents();
    msgOutQTableWidget->resizeColumnsToContents();
    msgOutQTableWidget->setAlternatingRowColors(true);

    for (int col = 0; col < msgOutQTableWidget->columnCount(); ++col)
    {
        QTableWidgetItem *newItem = new QTableWidgetItem("");
        newItem->setTextAlignment(Qt::AlignHCenter);
        msgOutQTableWidget->setItem(0, col, newItem);
    }

    // Adjust the look of the message in queues table.
    msgInQGroupBox->layout()->setAlignment(msgInQTableWidget, Qt::AlignTop);

    msgInQTableWidget->verticalHeader()->hide();
    msgInQTableWidget->resizeRowsToContents();
    msgInQTableWidget->resizeColumnsToContents();
    msgInQTableWidget->setAlternatingRowColors(true);

    try
    {
        m_connectionId = Safir::Dob::Internal::Connections::Instance().
                                    GetConnectionByName(connectionName.toUtf8().constData())->Id();
    }
    catch (const Safir::Dob::Typesystem::SoftwareViolationException&)
    {
       // The connection doesn't exist! This will be handled in the UpdateStatistics() method.
    }

    connect(&m_timer, SIGNAL(timeout()), this, SLOT(UpdateStatistics()));
    m_timer.start(3000);

    UpdateStatistics(true);
}



void ConnectionStats::UpdateStatistics(const bool ignoreVisible)
{
    if (!ignoreVisible && !isVisible())
    {
        return;
    }

    bool connectionExists = false;
    Stat stat;

    Safir::Dob::Internal::Connections::Instance().ForSpecificConnection
        (m_connectionId,
         boost::bind(&ProcessConnection,
                     _1,
                     boost::ref(stat),
                     boost::ref(connectionExists)));

    if (connectionExists)
    {
        missingConnectionLabel->hide();

        // Request out queue
        reqOutQTableWidget->item(0,0)->setText(boost::lexical_cast<std::string>(stat.reqOutQStat.noPushedRequests).c_str());
        reqOutQTableWidget->item(0,1)->setText(boost::lexical_cast<std::string>(stat.reqOutQStat.noOverflows).c_str());
        reqOutQTableWidget->item(0,2)->setText(boost::lexical_cast<std::string>(stat.reqOutQStat.noDispatchedRequests).c_str());
        reqOutQTableWidget->item(0,3)->setText(boost::lexical_cast<std::string>(stat.reqOutQStat.noAttachedResponses).c_str());
        reqOutQTableWidget->item(0,4)->setText(boost::lexical_cast<std::string>(stat.reqOutQStat.noDispatchedResponses).c_str());
        reqOutQTableWidget->item(0,5)->setText(boost::lexical_cast<std::string>(stat.reqOutQStat.noTimeouts).c_str());
        reqOutQTableWidget->item(0,6)->setText(boost::lexical_cast<std::string>(stat.reqOutQStat.capacity).c_str());
        reqOutQTableWidget->item(0,7)->setText(boost::lexical_cast<std::string>(stat.reqOutQStat.size).c_str());

        //empty the table
        reqInQTableWidget->clearContents();
        for(int row = 0; row < reqInQTableWidget->rowCount(); ++row)
        {
            reqInQTableWidget->removeRow(row);
        }
        // Request in queues
        for (unsigned int row = 0; row < stat.reqInQStat.size(); ++row)
        {
            reqInQTableWidget->insertRow(row);

            for (int col = 0; col < reqInQTableWidget->columnCount(); ++col)
            {
                QTableWidgetItem *newItem = new QTableWidgetItem();
                newItem->setTextAlignment(Qt::AlignHCenter);

                switch (col)
                {
                case 0:
                    {
                        std::ostringstream ostr;
                        ostr << std::showbase << std::hex << (unsigned long)(stat.reqInQStat[row].consumerId.consumer);
                        newItem->setText(ostr.str().c_str());
                    }
                    break;
                case 1:
                    {
                        newItem->setText(boost::lexical_cast<std::string>(stat.reqInQStat[row].noPushedRequests).c_str());
                    }
                    break;
                 case 2:
                    {
                        newItem->setText(boost::lexical_cast<std::string>(stat.reqInQStat[row].noOverflows).c_str());
                    }
                    break;
                 case 3:
                    {
                        newItem->setText(boost::lexical_cast<std::string>(stat.reqInQStat[row].noDispatchedRequests).c_str());
                    }
                    break;
                 case 4:
                    {
                        newItem->setText(boost::lexical_cast<std::string>(stat.reqInQStat[row].noAttachedResponses).c_str());
                    }
                    break;
                 case 5:
                    {
                        newItem->setText(boost::lexical_cast<std::string>(stat.reqInQStat[row].noDispatchedResponses).c_str());
                    }
                    break;
                 case 6:
                    {
                        newItem->setText(boost::lexical_cast<std::string>(stat.reqInQStat[row].capacity).c_str());
                    }
                    break;
                 case 7:
                    {
                        newItem->setText(boost::lexical_cast<std::string>(stat.reqInQStat[row].size).c_str());
                    }
                    break;
                }

                reqInQTableWidget->setItem(row, col, newItem);
            }
        }

        // Message out queue
        msgOutQTableWidget->item(0,0)->setText(boost::lexical_cast<std::string>(stat.msgOutQStat.noPushedMsg).c_str());
        msgOutQTableWidget->item(0,1)->setText(boost::lexical_cast<std::string>(stat.msgOutQStat.noOverflows).c_str());
        msgOutQTableWidget->item(0,2)->setText(boost::lexical_cast<std::string>(stat.msgOutQStat.capacity).c_str());
        msgOutQTableWidget->item(0,3)->setText(boost::lexical_cast<std::string>(stat.msgOutQStat.size).c_str());

        //empty the table
        msgInQTableWidget->clearContents();
        for(int row = 0; row < msgInQTableWidget->rowCount(); ++row)
        {
            msgInQTableWidget->removeRow(row);
        }

        // Message in queues
        for (unsigned int row = 0; row < stat.msgInQStat.size(); ++row)
        {
            msgInQTableWidget->insertRow(row);

            for (int col = 0; col < msgInQTableWidget->columnCount(); ++col)
            {
                QTableWidgetItem *newItem = new QTableWidgetItem();
                newItem->setTextAlignment(Qt::AlignHCenter);

                switch (col)
                {
                case 0:
                    {
                        std::ostringstream ostr;
                        ostr << std::showbase << std::hex << (unsigned long)(stat.msgInQStat[row].consumerId.consumer);
                        newItem->setText(ostr.str().c_str());
                    }
                    break;
                case 1:
                    {
                        newItem->setText(boost::lexical_cast<std::string>(stat.msgInQStat[row].noPushedMsg).c_str());
                    }
                    break;
                case 2:
                    {
                        newItem->setText(boost::lexical_cast<std::string>(stat.msgInQStat[row].noOverflows).c_str());
                    }
                    break;
                case 3:
                    {
                        newItem->setText(boost::lexical_cast<std::string>(stat.msgInQStat[row].capacity).c_str());
                    }
                    break;
                case 4:
                    {
                        newItem->setText(boost::lexical_cast<std::string>(stat.msgInQStat[row].size).c_str());
                    }
                    break;
                }

                msgInQTableWidget->setItem(row, col, newItem);
            }
        }
    }
    else
    {
        missingConnectionLabel->show();
    }

}

void ConnectionStats::ProcessConnection(const Safir::Dob::Internal::ConnectionPtr& connection, Stat& stat, bool& exist)
{
    exist = true;

    Safir::Dob::Internal::RequestOutQueue& reqOutQ = connection->GetRequestOutQueue();
    StatisticsCollector(reqOutQ, &stat.reqOutQStat);

    connection->ForEachRequestInQueue(boost::bind(&ProcessReqInQ,
                                                  _1,
                                                  _2,
                                                  boost::ref(stat)));

    Safir::Dob::Internal::MessageQueue& msgOutQ = connection->GetMessageOutQueue();
    StatisticsCollector(msgOutQ, &stat.msgOutQStat);

    connection->ForEachMessageInQueue(boost::bind(&ProcessMsgInQ,
                                                  _1,
                                                  _2,
                                                  boost::ref(stat)));


}

void ConnectionStats::ProcessReqInQ(const Safir::Dob::Internal::ConsumerId& consumer,
                                    Safir::Dob::Internal::RequestInQueue&   queue,
                                    Stat&                                   stat)
{
    ReqQStat reqInQStat;

    reqInQStat.consumerId = consumer;

    StatisticsCollector(queue, &reqInQStat);

    stat.reqInQStat.push_back(reqInQStat);
}

void ConnectionStats::ProcessMsgInQ(const Safir::Dob::Internal::ConsumerId& consumer,
                                    Safir::Dob::Internal::MessageQueue&     queue,
                                    Stat&                                   stat)
{
    MsgQStat msgInQStat;

    msgInQStat.consumerId = consumer;

    StatisticsCollector(queue, &msgInQStat);

    stat.msgInQStat.push_back(msgInQStat);
}

