/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
*
* Created by: Anders Wid�n/ stawi
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

#ifndef __ACTIONSENDER_H__
#define __ACTIONSENDER_H__

#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/SecondaryConnection.h>
#include <DoseTest/Action.h>
#include <boost/asio.hpp>

class ActionSender
{
public:
    explicit ActionSender(boost::asio::io_service& ioService);
    ~ActionSender();

    void Send(const DoseTest::ActionPtr& msg);

private:
    boost::asio::io_service& m_ioService;
    boost::asio::ip::udp::socket m_socket;

    Safir::Dob::Typesystem::Int32 m_seqNbr;
};

#endif


