/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include <string>

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
     * Class to be used by dose_main to receive commands from Control
     */
    class CONTROL_UTILS_API DoseMainCmdReceiver
        : private boost::noncopyable
    {
    public:

        typedef std::function<void(int64_t requestId,
                                   const std::string& nodeName,
                                   int64_t nodeId,
                                   int64_t nodeTypeId,
                                   const std::string& dataAddress)> InjectNodeCmdCb;

        DoseMainCmdReceiver(boost::asio::io_service& ioService,
                            const InjectNodeCmdCb&   cmdCb);

        // Start command reception
        void Start();

        // Stop command reception
        void Stop();

    private:

        class Impl;

        boost::shared_ptr<Impl> m_impl;
    };

    /**
     * Class to be used by Control to send commands to dose_main
     */
    class CONTROL_UTILS_API DoseMainCmdSender
        : private boost::noncopyable
    {
    public:

        DoseMainCmdSender(boost::asio::io_service& ioService);

        // Start sender
        void Start();

        // Stop sender
        void Stop();

        void InjectNode(int64_t requestId,
                        const std::string& nodeName,
                        int64_t nodeId,
                        int64_t nodeTypeId,
                        const std::string& dataAddress);

    private:
        class Impl;

        boost::shared_ptr<Impl> m_impl;
    };

#ifdef _MSC_VER
#pragma warning (pop)
#endif

}
}
}
}



