/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com/)
*
* Created by: Anders Widén / anders.widen@consoden.se
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
#pragma once

#include <Safir/Dob/Internal/ControlUtilsExportDefs.h>
#include <string>
#include <boost/asio/io_service.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4275)
#pragma warning (disable: 4251)
#endif

    /**
     * Class to be used to receive Control info
     */
    class CONTROL_UTILS_API ControlInfoReceiver
    {
    public:

        typedef std::function<void(int64_t incarnationId , int64_t nodeId)> InfoCb;

        ControlInfoReceiver(boost::asio::io_service& ioService,
                            const InfoCb&            infoCb);


        ControlInfoReceiver(const ControlInfoReceiver&) = delete;
        const ControlInfoReceiver& operator=(const ControlInfoReceiver&) = delete;

        // Start command reception
        void Start();

        // Stop command reception
        void Stop();

    private:

        class Impl;

        std::shared_ptr<Impl> m_impl;
    };

    /**
     *  Class to be used to send Control info
     */
    class CONTROL_UTILS_API ControlInfoSender
    {
    public:

        ControlInfoSender(boost::asio::io_service&      ioService,
                          const std::function<void()>   receiverConnectedCb);

        ControlInfoSender(const ControlInfoSender&) = delete;
        const ControlInfoSender& operator=(const ControlInfoSender&) = delete;

        // Start sender
        void Start();

        // Stop sender
        void Stop();

        void SendInfo(int64_t  incarnationId,
                      int64_t  nodeId);

    private:
        class Impl;

        std::shared_ptr<Impl> m_impl;
    };

#ifdef _MSC_VER
#pragma warning (pop)
#endif

}
}
}
}



