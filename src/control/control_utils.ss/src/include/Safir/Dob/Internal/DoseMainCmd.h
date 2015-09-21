/******************************************************************************
*
* Copyright Saab AB, 2014-2015 (http://safir.sourceforge.net)
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
#include <boost/noncopyable.hpp>
#include <string>

//forward declaration
namespace boost { namespace asio {
    class io_service;
}}

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

        typedef std::function<void(const std::string& nodeName,
                                                    int64_t nodeId,
                                                    int64_t nodeTypeId,
                                                    const std::string& dataAddress)> IncludeNodeCmdCb;

        typedef std::function<void(int64_t nodeId, int64_t nodeTypeId)> ExcludeNodeCmdCb;

        typedef std::function<void(int64_t nodeId)> StoppedNodeIndicationCb;

        typedef std::function<void()> StopDoseMainCb;

        DoseMainCmdReceiver(boost::asio::io_service&        ioService,
                            const IncludeNodeCmdCb&         startDoseMainCb,
                            const IncludeNodeCmdCb&         injectNodeCb,
                            const ExcludeNodeCmdCb&         excludeNodeCb,
                            const StoppedNodeIndicationCb&  stoppedNodeIndicationCb,
                            const StopDoseMainCb&           stopDoseMainCb);

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

        DoseMainCmdSender(boost::asio::io_service&      ioService,
                          const std::function<void()>   doseMainConnectedCb);

        // Start sender
        void Start();

        // Stop sender
        void Stop();

        // dose_main commands
        void StartDoseMain(const std::string& nodeName,
                           int64_t nodeId,
                           int64_t nodeTypeId,
                           const std::string& dataAddress);

        void InjectNode(const std::string& nodeName,
                        int64_t nodeId,
                        int64_t nodeTypeId,
                        const std::string& dataAddress);

        void ExcludeNode(int64_t nodeId,
                         int64_t nodeTypeId);

        void StoppedNodeIndication(int64_t nodeId);

        void StopDoseMain();

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



