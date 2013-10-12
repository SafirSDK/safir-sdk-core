/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
*
* Created by: Mikael Wennerberg/ stmiwn
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
#include <boost/noncopyable.hpp>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Internal/Atomic.h>
#include "requestor.h"
#include "handler.h"

#include <Safir/Utilities/AsioDispatcher.h>

class App:
    public Safir::Dob::StopHandler,
     private boost::noncopyable
{
public:
    App(const std::vector<std::string> & commandLine);
    void Run();

private:

    virtual void OnStopOrder();
    void PrintHelp();

    boost::asio::io_service m_ioService;
    Safir::Dob::Connection m_Connection;
    Safir::Utilities::AsioDispatcher   m_dispatch;
    Requestor m_requestor;
    Handler m_handler;
};
