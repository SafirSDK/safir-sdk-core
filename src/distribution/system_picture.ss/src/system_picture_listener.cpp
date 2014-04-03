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
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Utilities/Internal/AsioPeriodicTimer.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/asio.hpp>
#include <boost/make_shared.hpp>

int main()
{
    boost::asio::io_service ioService;
    
    boost::asio::signal_set signals(ioService);
    
#if defined (_WIN32)
    signals.add(SIGABRT);
    signals.add(SIGBREAK);
    signals.add(SIGINT);
    signals.add(SIGTERM);
#else
    signals.add(SIGQUIT);
    signals.add(SIGINT);
    signals.add(SIGTERM);
#endif

    Safir::Dob::Internal::SP::SystemPicture sp(Safir::Dob::Internal::SP::slave_tag);
    
    auto rawSub = sp.GetRawStatistics();
    
    rawSub->Start(ioService,
                  [](const Safir::Dob::Internal::SP::RawStatistics& data)
                  {                  
                      std::wcout << data << std::endl;
                  });

    signals.async_wait([&](const boost::system::error_code& error,
                           const int /*signal_number*/)
                       {
                           if (!!error) //double not to remove spurious vs2010 warning
                           {
                               lllog(0) << "Got a signals error: " << error << std::endl;
                           }
                           rawSub->Stop();
                       });

    boost::asio::io_service::work wk(ioService);
    ioService.run();
    return 0;
}
