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
#include "SystemPicture.h"
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

SystemPicture::SystemPicture(boost::asio::io_service& ioService, QWidget* /*parent*/)
    : m_ioService(ioService)
{
    namespace SP = Safir::Dob::Internal::SP;
    
    SP::SystemPicture sp(SP::slave_tag);
    m_systemStateSubscriber = sp.GetSystemState();

    m_systemStateSubscriber->Start(ioService, [this](const SP::SystemState& state){UpdatedState(state);});

    setupUi(this);

    systemTable->horizontalHeader()->setResizeMode(QHeaderView::ResizeToContents);
    systemTable->verticalHeader()->setResizeMode(QHeaderView::ResizeToContents);
    systemTable->sortItems(COLUMN_NAME);
}

SystemPicture::~SystemPicture()
{
    m_systemStateSubscriber->Stop();
}

void SystemPicture::UpdatedState(const Safir::Dob::Internal::SP::SystemState& data)
{
    SetText(electedId,data.ElectedId());
    
    systemTable->setSortingEnabled(false);
    UpdateSystemTable(data);
    systemTable->setSortingEnabled(true);
}


void SystemPicture::UpdateSystemTable(const Safir::Dob::Internal::SP::SystemState& statistics)
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
        systemTable->setItem(row,
                             COLUMN_NODE_TYPE,
                             new QTableWidgetItem(QString::number(statistics.NodeTypeId(it->second)))); //TODO: use name instead of id
        systemTable->setItem(row,
                             COLUMN_CONTROL_ADDRESS,
                             new QTableWidgetItem(QString::fromUtf8(statistics.ControlAddress(it->second).c_str())));
        systemTable->setItem(row,
                             COLUMN_DATA_ADDRESS,
                             new QTableWidgetItem(QString::fromUtf8(statistics.DataAddress(it->second).c_str())));

        //set elected coordinator 
        if (it->first == statistics.ElectedId())
        {
            if (systemTable->item(row, COLUMN_NAME)->background() != QColor(230,115,57))
            {
                for (int column = 0; column < NUM_COLUMNS; ++column)
                {
                    systemTable->item(row, column)->setBackground(QColor(230,115,57));
                    systemTable->item(row, column)->setToolTip("Elected coordinator");
                }
            }
        }
        else
        {
            if (systemTable->item(row, COLUMN_NAME)->background() != Qt::transparent)
            {
                for (int column = 0; column < NUM_COLUMNS; ++column)
                {
                    systemTable->item(row, column)->setBackground(Qt::transparent);
                    systemTable->item(row, column)->setToolTip("");
                }
            }

        }

    }
}

