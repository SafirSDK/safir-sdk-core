/******************************************************************************
*
* Copyright Saab AB, 2012 (http://safir.sourceforge.net)
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
#ifndef __SAFIR_DOB_INTERNAL_SYSTEMPICTURE_H__
#define __SAFIR_DOB_INTERNAL_SYSTEMPICTURE_H__

#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>
#include <Safir/Dob/Internal/RawStatistics.h>
#include <Safir/Dob/Internal/SystemState.h>
#include <Safir/Dob/Internal/SystemPictureExportDefs.h>
#include <map>

//Forward declare some asio stuff.
namespace boost
{
namespace asio
{
    class io_service;
}
}


namespace Safir
{
namespace Dob
{
namespace Internal
{

namespace Com
{
    //forward declaration.
    class Communication;
}

namespace SP
{

    struct master_tag_t {};
    const master_tag_t master_tag = master_tag_t();

    struct slave_tag_t {};
    const slave_tag_t slave_tag = slave_tag_t();

    struct NodeType
    {
        NodeType(const std::string& name,
                 const bool isLight,
                 const std::vector<std::string>& talksTo,
                 const bool canMulticast,
                 const bool deadTimeout);
    };

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4275)
#pragma warning (disable: 4251)
#endif

    class DISTRIBUTION_SYSTEM_PICTURE_API SystemPicture
        : private boost::noncopyable
    {
    public:
        SystemPicture(master_tag_t,
                      const boost::shared_ptr<boost::asio::io_service>& ioService,
                      const boost::shared_ptr<Com::Communication>& communication,
                      const std::string& name,
                      const boost::int64_t id,
                      const std::string& address,
                      const std::string& multicastAddress,
                      const std::map<std::string, NodeType>& nodeTypes);

        explicit SystemPicture(slave_tag_t);

        /** 
         * Stop the internal workings of this class. 
         * Must be called before destroying the object if you've instantiated
         * SystemPicture with a master_tag
         */
        void Stop();

        /**
         * Get the raw node statistics.
         *
         * Only available if you've instantiated SystemPicture with slave_tag.
         */
        boost::shared_ptr<RawStatisticsSubscriber> GetRawStatistics() const;

        /** 
         * Get the system picture state produced by System Picture
         *
         * Only available if you've instantiated SystemPicture with slave_tag.
         */
        boost::shared_ptr<SystemStateSubscriber> GetSystemState() const;
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

#endif

