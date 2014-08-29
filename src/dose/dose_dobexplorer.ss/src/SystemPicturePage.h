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
#ifndef __DOBEXPLORER_SYSTEM_PICTURE_H__
#define __DOBEXPLORER_SYSTEM_PICTURE_H__
#include "common_header.h"
#include "ui_SystemPicturePage.h"
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/asio.hpp>
#include <Safir/Dob/Internal/SystemPicture.h>

class SystemPicturePage :
  public QWidget,
  private Ui::SystemPicturePage,
  private boost::noncopyable
{
    Q_OBJECT

public:
    SystemPicturePage(boost::asio::io_service& ioService, QWidget *parent = 0);
    ~SystemPicturePage();
private:
    void UpdateSystemTable(const Safir::Dob::Internal::SP::SystemState& statistics);

    void UpdatedState(const Safir::Dob::Internal::SP::SystemState& data);
    boost::asio::io_service& m_ioService;
    Safir::Dob::Internal::SP::SystemPicture m_systemPicture;
};


#endif
