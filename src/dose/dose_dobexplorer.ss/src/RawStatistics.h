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
#include "ui_RawStatistics.h"
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/asio.hpp>
#include <Safir/Dob/Internal/SystemPicture.h>

class RawStatistics :
  public QWidget,
  private Ui::RawStatistics,
  private boost::noncopyable
{
    Q_OBJECT

public:
    RawStatistics(boost::asio::io_service& ioService, QWidget *parent = 0);
    ~RawStatistics();
public slots:
    void LocalTableSelectionChanged();
private:
    void UpdatedStatistics(const Safir::Dob::Internal::SP::RawStatistics& data);

    void UpdateLocalTable();
    void UpdateRemoteTable();
    
    boost::asio::io_service& m_ioService;
    Safir::Dob::Internal::SP::SystemPicture m_systemPicture;
    Safir::Dob::Internal::SP::RawStatisticsSubscriber& m_rawStatisticsSubscriber;

    //the last data we received.
    Safir::Dob::Internal::SP::RawStatistics m_statistics;
};


#endif
