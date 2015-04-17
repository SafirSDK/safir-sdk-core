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
#ifndef __DOBEXPLORER_RAW_STATISTICS_H__
#define __DOBEXPLORER_RAW_STATISTICS_H__
#include "common_header.h"
#include "ui_RawStatisticsPage.h"
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <Safir/Dob/Internal/SystemPicture.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif


class RawStatisticsPage :
  public QWidget,
  private Ui::RawStatisticsPage,
  private boost::noncopyable
{
    Q_OBJECT

public:
    explicit RawStatisticsPage(QWidget *parent = 0);

public slots:
    void PollIoService();
    void LocalTableSelectionChanged();

private:
    void closeEvent(QCloseEvent* event) override;
    void UpdatedStatistics(const Safir::Dob::Internal::SP::RawStatistics& data);

    void UpdateLocalTable();
    void UpdateRemoteTable();
    
    boost::asio::io_service m_ioService;
    QTimer m_ioServicePollTimer;

    Safir::Dob::Internal::SP::SystemPicture m_systemPicture;

    //the last data we received.
    Safir::Dob::Internal::SP::RawStatistics m_statistics;
};


#endif
