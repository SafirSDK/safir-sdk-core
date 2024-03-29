/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include "RawStatisticsPage.h"
#include <iostream>
#include <set>
#include <boost/make_shared.hpp>
namespace
{
    enum TableColumn
    {
        COLUMN_NAME,
        COLUMN_ID,
        COLUMN_NODE_TYPE,
        COLUMN_CONTROL_ADDRESS,
        COLUMN_DATA_ADDRESS,
        COLUMN_CONTROL_RECEIVE_COUNT,
        COLUMN_CONTROL_DUPLICATE_COUNT,
        COLUMN_CONTROL_RETRANSMIT_COUNT,
        COLUMN_DATA_RECEIVE_COUNT,
        COLUMN_DATA_DUPLICATE_COUNT,
        COLUMN_DATA_RETRANSMIT_COUNT,

        COLUMN_INCARNATION_ID, //this is only in the local table

        NUM_COLUMNS
    };

    void SetDead(QTableWidget* table, const int row)
    {
        if (table->item(row, COLUMN_NAME)->background() != QColor(200,200,200))
        {
            //std::wcout << "Set row " << row << " to dead" << std::endl;
            for (int column = 0; column < table->columnCount(); ++column)
            {
                table->item(row, column)->setBackground(QColor(200,200,200));
                table->item(row, column)->setToolTip("Node is dead");
            }
        }
    }

    template <class T>
    void SetText(T* widget, const std::string& str_)
    {
        const QString str = QString::fromUtf8(str_.c_str());
        if (widget->text() != str)
        {
            widget->setText(str);
        }

    }

    template <class T>
    void SetText(T* widget, const char* str_)
    {
        const QString str = QString::fromUtf8(str_);
        if (widget->text() != str)
        {
            widget->setText(str);
        }
    }

    template <class T, class U>
    void SetText(T* widget, const U number)
    {
        const QString str = QString::number(number);
        if (widget->text() != str)
        {
            widget->setText(str);
        }
    }


    std::map<std::int64_t, int> GetAllIds(const Safir::Dob::Internal::SP::RawStatistics& statistics)
    {
        std::map<std::int64_t,int> ids;
        const int size = statistics.Size();
        for (int i = 0; i < size; ++i)
        {
            ids.insert(std::make_pair(statistics.Id(i),i));
        }
        return ids;
    }

}

RawStatisticsPage::RawStatisticsPage(QWidget* /*parent*/)
    : m_systemPicture(Safir::Dob::Internal::SP::subscriber_tag,
                      m_ioService)
{
    m_systemPicture.StartRawSubscription([this](const Safir::Dob::Internal::SP::RawStatistics& d)
                                         {UpdatedStatistics(d);});

    setupUi(this);
    connect(localTable, SIGNAL(itemSelectionChanged()), this, SLOT(LocalTableSelectionChanged()));

    localTable->horizontalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
    localTable->verticalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
    localTable->sortItems(COLUMN_NAME);

    remoteTable->horizontalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
    remoteTable->verticalHeader()->setSectionResizeMode(QHeaderView::ResizeToContents);
    remoteTable->sortItems(COLUMN_NAME);

    tableSplitter->setStretchFactor(0,2);
    tableSplitter->setStretchFactor(1,1);

    connect(&m_ioServicePollTimer,SIGNAL(timeout()), this, SLOT(PollIoService()));
    m_ioServicePollTimer.start(100);
    PollIoService();
}


void RawStatisticsPage::closeEvent(QCloseEvent* event)
{
    m_systemPicture.Stop();

    //run the ioservice until all SystemPicture stuff is completed.
    m_ioService.reset();
    m_ioService.run();

    event->accept();
}


void RawStatisticsPage::UpdatedStatistics(const Safir::Dob::Internal::SP::RawStatistics& d)
{
    m_statistics = d;

    SetText(name,d.Name());
    SetText(address,d.ControlAddress());
    SetText(id,d.Id());

    SetText(incarnationId, d.IncarnationId());
    SetText(electionId, d.ElectionId());

    localTable->setSortingEnabled(false);
    UpdateLocalTable();
    localTable->setSortingEnabled(true);

    remoteTable->setSortingEnabled(false);
    UpdateRemoteTable();
    remoteTable->setSortingEnabled(true);
}


void RawStatisticsPage::UpdateLocalTable()
{
    //starts off containing all ids in the statistics message
    //but in the first loop we remove all that we already have in the table
    auto ids = GetAllIds(m_statistics);

    //start by removing rows that are no longer in statistics
    //and updating existing rows
    for (int row = localTable->rowCount() - 1; row >= 0; --row)
    {
        const qlonglong id_ = localTable->item(row, COLUMN_ID)->text().toLongLong();

        const auto findIt = ids.find(id_);
        if (findIt == ids.end())
        {
            localTable->removeRow(row);
        }
        else
        {
            SetText(localTable->item(row,COLUMN_CONTROL_RECEIVE_COUNT),
                    m_statistics.ControlReceiveCount(findIt->second));

            SetText(localTable->item(row,COLUMN_CONTROL_DUPLICATE_COUNT),
                    m_statistics.ControlDuplicateCount(findIt->second));

            SetText(localTable->item(row,COLUMN_CONTROL_RETRANSMIT_COUNT),
                    m_statistics.ControlRetransmitCount(findIt->second));

            SetText(localTable->item(row,COLUMN_DATA_RECEIVE_COUNT),
                    m_statistics.DataReceiveCount(findIt->second));

            SetText(localTable->item(row,COLUMN_DATA_DUPLICATE_COUNT),
                    m_statistics.DataDuplicateCount(findIt->second));

            SetText(localTable->item(row,COLUMN_DATA_RETRANSMIT_COUNT),
                    m_statistics.DataRetransmitCount(findIt->second));

            if (m_statistics.HasRemoteStatistics(findIt->second))
            {
                SetText(localTable->item(row,COLUMN_INCARNATION_ID),
                        m_statistics.RemoteStatistics(findIt->second).IncarnationId());
            }
            else
            {
                SetText(localTable->item(row,COLUMN_INCARNATION_ID), "");
            }

            if (m_statistics.IsDead(findIt->second))
            {
                SetDead(localTable, row);
            }

            ids.erase(id_);
        }
    }

    //add new ids
    for(auto it = ids.begin(); it != ids.end(); ++it)
    {
        const int row = localTable->rowCount();
        localTable->insertRow(row);
        localTable->setItem(row,
                            COLUMN_NAME,
                            new QTableWidgetItem(QString::fromUtf8(m_statistics.Name(it->second).c_str())));
        localTable->setItem(row,
                            COLUMN_ID,
                            new QTableWidgetItem(QString::number(it->first)));
        localTable->item(row,COLUMN_ID)->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);

        localTable->setItem(row,
                            COLUMN_NODE_TYPE,
                            new QTableWidgetItem(QString::number(m_statistics.NodeTypeId(it->second))));
        localTable->setItem(row,
                            COLUMN_CONTROL_ADDRESS,
                            new QTableWidgetItem(QString::fromUtf8(m_statistics.ControlAddress(it->second).c_str())));
        localTable->setItem(row,
                            COLUMN_DATA_ADDRESS,
                            new QTableWidgetItem(QString::fromUtf8(m_statistics.DataAddress(it->second).c_str())));

        localTable->setItem(row,
                             COLUMN_CONTROL_RECEIVE_COUNT,
                             new QTableWidgetItem(QString::number(m_statistics.ControlReceiveCount(it->second))));
        localTable->setItem(row,
                             COLUMN_CONTROL_DUPLICATE_COUNT,
                             new QTableWidgetItem(QString::number(m_statistics.ControlDuplicateCount(it->second))));
        localTable->setItem(row,
                             COLUMN_CONTROL_RETRANSMIT_COUNT,
                             new QTableWidgetItem(QString::number(m_statistics.ControlRetransmitCount(it->second))));

        localTable->setItem(row,
                             COLUMN_DATA_RECEIVE_COUNT,
                             new QTableWidgetItem(QString::number(m_statistics.DataReceiveCount(it->second))));
        localTable->setItem(row,
                             COLUMN_DATA_DUPLICATE_COUNT,
                             new QTableWidgetItem(QString::number(m_statistics.DataDuplicateCount(it->second))));
        localTable->setItem(row,
                             COLUMN_DATA_RETRANSMIT_COUNT,
                             new QTableWidgetItem(QString::number(m_statistics.DataRetransmitCount(it->second))));

        localTable->setItem(row,
                            COLUMN_INCARNATION_ID,
                            new QTableWidgetItem(QString()));

        for (int column = 0; column < localTable->columnCount(); ++column)
        {
            localTable->item(row, column)->setToolTip("Node is alive");
        }

        if (m_statistics.IsDead(it->second))
        {
            SetDead(localTable,row);
        }
    }
}


void RawStatisticsPage::LocalTableSelectionChanged()
{
    if (m_statistics.Valid())
    {
        //remoteTable->clear();
        remoteTable->setSortingEnabled(false);
        UpdateRemoteTable();
        remoteTable->setSortingEnabled(true);

    }
}

void RawStatisticsPage::UpdateRemoteTable()
{
    const auto selection = localTable->selectedItems();
    if (selection.empty())
    {
        while(remoteTable->rowCount() > 0)
        {
            remoteTable->removeRow(0);
        }
    }
    else
    {
        const int selectedRow = selection[0]->row();
        const qlonglong selectedId = localTable->item(selectedRow, COLUMN_ID)->text().toLongLong();

        const int size = m_statistics.Size();
        for (int i = 0; i < size; ++i)
        {
            if (selectedId == m_statistics.Id(i))
            {
                if(!m_statistics.HasRemoteStatistics(i))
                {
                    remoteTable->setRowCount(0);
                    remoteTable->setEnabled(false);
                    remoteTable->setToolTip("There is no remote data for the selected node");
                    return;
                }
                remoteTable->setEnabled(true);
                remoteTable->setToolTip("");

                const auto statistics = m_statistics.RemoteStatistics(i);

                //starts off containing all ids in the statistics message
                //but in the first loop we remove all that we already have in the table
                auto ids = GetAllIds(statistics);

                //start by removing rows that are no longer in statistics
                //and updating existing rows
                for (int row = remoteTable->rowCount() - 1; row >= 0; --row)
                {
                    const qlonglong id_ = remoteTable->item(row, COLUMN_ID)->text().toLongLong();

                    const auto findIt = ids.find(id_);
                    if (findIt == ids.end())
                    {
                        remoteTable->removeRow(row);
                    }
                    else
                    {
                        SetText(remoteTable->item(row,COLUMN_CONTROL_RECEIVE_COUNT),
                                statistics.ControlReceiveCount(findIt->second));

                        SetText(remoteTable->item(row,COLUMN_CONTROL_DUPLICATE_COUNT),
                                statistics.ControlDuplicateCount(findIt->second));

                        SetText(remoteTable->item(row,COLUMN_CONTROL_RETRANSMIT_COUNT),
                                statistics.ControlRetransmitCount(findIt->second));

                        SetText(remoteTable->item(row,COLUMN_DATA_RECEIVE_COUNT),
                                statistics.DataReceiveCount(findIt->second));

                        SetText(remoteTable->item(row,COLUMN_DATA_DUPLICATE_COUNT),
                                statistics.DataDuplicateCount(findIt->second));

                        SetText(remoteTable->item(row,COLUMN_DATA_RETRANSMIT_COUNT),
                                statistics.DataRetransmitCount(findIt->second));


                        if (statistics.IsDead(findIt->second))
                        {
                            SetDead(remoteTable, row);
                        }

                        ids.erase(id_);
                    }
                }

                //add new ids
                for(auto it = ids.begin(); it != ids.end(); ++it)
                {
                    const int row = remoteTable->rowCount();
                    remoteTable->insertRow(row);
                    remoteTable->setItem(row,
                                         COLUMN_NAME,
                                         new QTableWidgetItem(QString::fromUtf8(statistics.Name(it->second).c_str())));
                    remoteTable->setItem(row,
                                         COLUMN_ID,
                                         new QTableWidgetItem(QString::number(it->first)));
                    remoteTable->item(row,COLUMN_ID)->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);

                    remoteTable->setItem(row,
                                         COLUMN_NODE_TYPE,
                                         new QTableWidgetItem(QString::number(statistics.NodeTypeId(it->second))));
                    remoteTable->setItem(row,
                                         COLUMN_CONTROL_ADDRESS,
                                         new QTableWidgetItem(QString::fromUtf8(statistics.ControlAddress(it->second).c_str())));
                    remoteTable->setItem(row,
                                         COLUMN_DATA_ADDRESS,
                                         new QTableWidgetItem(QString::fromUtf8(statistics.DataAddress(it->second).c_str())));

                    remoteTable->setItem(row,
                                         COLUMN_CONTROL_RECEIVE_COUNT,
                                         new QTableWidgetItem(QString::number(statistics.ControlReceiveCount(it->second))));
                    remoteTable->setItem(row,
                                         COLUMN_CONTROL_DUPLICATE_COUNT,
                                         new QTableWidgetItem(QString::number(statistics.ControlDuplicateCount(it->second))));
                    remoteTable->setItem(row,
                                         COLUMN_CONTROL_RETRANSMIT_COUNT,
                                         new QTableWidgetItem(QString::number(statistics.ControlRetransmitCount(it->second))));
                    remoteTable->setItem(row,
                                         COLUMN_DATA_RECEIVE_COUNT,
                                         new QTableWidgetItem(QString::number(statistics.DataReceiveCount(it->second))));
                    remoteTable->setItem(row,
                                         COLUMN_DATA_DUPLICATE_COUNT,
                                         new QTableWidgetItem(QString::number(statistics.DataDuplicateCount(it->second))));
                    remoteTable->setItem(row,
                                         COLUMN_DATA_RETRANSMIT_COUNT,
                                         new QTableWidgetItem(QString::number(statistics.DataRetransmitCount(it->second))));

                    for (int column = 0; column < remoteTable->columnCount(); ++column)
                    {
                        remoteTable->item(row, column)->setToolTip("Node is alive");
                    }


                    if (statistics.IsDead(it->second))
                    {
                        SetDead(remoteTable,row);
                    }
                }
            }
        }

    }
}

void RawStatisticsPage::PollIoService()
{
    m_ioService.poll_one();
    m_ioService.reset();
}
