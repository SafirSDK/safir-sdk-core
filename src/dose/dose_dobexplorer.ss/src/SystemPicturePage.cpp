/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
//need this for setResizeMode
#define QT_DISABLE_DEPRECATED_BEFORE 0x000000

#include "common_header.h"
#include "SystemPicturePage.h"
#include <iostream>
#include <set>

namespace
{
    enum SystemTableColumn
    {
        COLUMN_NAME,
        COLUMN_ID,
        COLUMN_NODE_TYPE,
        COLUMN_CONTROL_ADDRESS,
        COLUMN_DATA_ADDRESS,

        NUM_COLUMNS
    };

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

    std::map<boost::int64_t, int> GetAllIds(const Safir::Dob::Internal::SP::SystemState& statistics)
    {
        std::map<boost::int64_t,int> ids;
        const int size = statistics.Size();
        for (int i = 0; i < size; ++i)
        {
            ids.insert(std::make_pair(statistics.Id(i),i));
        }
        return ids;
    }

}

SystemPicturePage::SystemPicturePage( QWidget* /*parent*/)
    : m_systemPicture(Safir::Dob::Internal::SP::subscriber_tag,
                      m_ioService)
{
    m_systemPicture.StartStateSubscription([this](const Safir::Dob::Internal::SP::SystemState& state)
                                           {UpdatedState(state);});

    setupUi(this);

    systemTable->horizontalHeader()->setResizeMode(QHeaderView::ResizeToContents);
    systemTable->verticalHeader()->setResizeMode(QHeaderView::ResizeToContents);
    systemTable->sortItems(COLUMN_NAME);

    connect(&m_ioServicePollTimer,SIGNAL(timeout()), this, SLOT(PollIoService()));
    m_ioServicePollTimer.start(100);
    PollIoService();
}


void SystemPicturePage::closeEvent(QCloseEvent* event)
{
    m_systemPicture.Stop();

    //run the ioservice until all SystemPicture stuff is completed.
    m_ioService.reset();
    m_ioService.run();

    event->accept();
}

void SystemPicturePage::UpdatedState(const Safir::Dob::Internal::SP::SystemState& data)
{
    SetText(electedId,data.ElectedId());

    systemTable->setSortingEnabled(false);
    UpdateSystemTable(data);
    systemTable->setSortingEnabled(true);
}

void UpdateRowState(QTableWidget* systemTable,
                    const int row,
                    const bool isElected,
                    const bool isDead)
{
    QColor expectedColor = Qt::transparent;
    QString tooltip = "Node is alive";

    if (isElected && isDead)
    {
        expectedColor = QColor(240,100,100);
        tooltip = "Elected coordinator is dead!";
    }
    else if (isElected)
    {
        expectedColor = QColor(230,115,57);
        tooltip = "Elected coordinator";
    }
    else if (isDead)
    {
        expectedColor = QColor(200,200,200);
        tooltip = "Node died recently";
    }

    if (systemTable->item(row, COLUMN_NAME)->background() != expectedColor)
    {
        for (int column = 0; column < NUM_COLUMNS; ++column)
        {
            systemTable->item(row, column)->setBackground(expectedColor);
            systemTable->item(row, column)->setToolTip(tooltip);
        }
    }
}

void SystemPicturePage::UpdateSystemTable(const Safir::Dob::Internal::SP::SystemState& statistics)
{
    //starts off containing all ids in the statistics message
    //but in the first loop we remove all that we already have in the table
    auto ids = GetAllIds(statistics);

    //start by removing rows that are no longer in statistics
    for (int row = systemTable->rowCount() - 1; row >= 0; --row)
    {
        const qlonglong id = systemTable->item(row, COLUMN_ID)->text().toLongLong();

        const auto findIt = ids.find(id);
        if (findIt == ids.end())
        {
            systemTable->removeRow(row);
        }
        else
        {
            ids.erase(id);

            UpdateRowState(systemTable,
                           row,
                           id == statistics.ElectedId(), //isElected
                           statistics.IsDead(findIt->second)); //isDead
        }
    }

    //add new ids
    for(auto it = ids.begin(); it != ids.end(); ++it)
    {
        const int row = systemTable->rowCount();
        systemTable->insertRow(row);
        systemTable->setItem(row,
                             COLUMN_NAME,
                             new QTableWidgetItem(QString::fromUtf8(statistics.Name(it->second).c_str())));
        systemTable->setItem(row,
                             COLUMN_ID,
                             new QTableWidgetItem(QString::number(it->first)));
        systemTable->item(row,COLUMN_ID)->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);
        systemTable->setItem(row,
                             COLUMN_NODE_TYPE,
                             new QTableWidgetItem(QString::number(statistics.NodeTypeId(it->second)))); //TODO: use name instead of id
        systemTable->setItem(row,
                             COLUMN_CONTROL_ADDRESS,
                             new QTableWidgetItem(QString::fromUtf8(statistics.ControlAddress(it->second).c_str())));
        systemTable->setItem(row,
                             COLUMN_DATA_ADDRESS,
                             new QTableWidgetItem(QString::fromUtf8(statistics.DataAddress(it->second).c_str())));

        UpdateRowState(systemTable,
                       row,
                       it->first == statistics.ElectedId(), //isElected
                       statistics.IsDead(it->second)); //isDead

    }
}

void SystemPicturePage::PollIoService()
{
    m_ioService.poll_one();
    m_ioService.reset();
}
