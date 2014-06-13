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
#ifndef __SAFIR_DOB_INTERNAL_RAW_STATISTICS_H__
#define __SAFIR_DOB_INTERNAL_RAW_STATISTICS_H__

#include <Safir/Dob/Internal/SystemPictureDefs.h>
#include <functional>
#include <memory>
#include <ostream>
#include <string>

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
namespace SP
{

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4275)
#pragma warning (disable: 4251)
#endif

    /** 
     * This is an interface into the statistics that a SystemPicture instance
     * collects on the local node and receives from the remote nodes. 
     *
     * Use functionality in SystemPicture class to get an instance to one
     * of these objects.
     */
    class DISTRIBUTION_SYSTEM_PICTURE_API RawStatistics
    {
    public:
        //Create an empty statistics object (Valid will return false)
        //all other methods will yield undefined behaviour
        RawStatistics() {} 

        bool Valid() const {return m_impl != nullptr;};

        const std::string& Name() const;
        int64_t Id() const;
        int64_t NodeTypeId() const;
        const std::string& ControlAddress() const;
        const std::string& DataAddress() const;

        //last election id
        int64_t ElectionId() const;
        
        int Size() const;

        //Static fields
        const std::string& Name(const int index) const;
        int64_t Id(const int index) const;
        int64_t NodeTypeId(const int index) const;
        const std::string& ControlAddress(const int index) const;
        const std::string& DataAddress(const int index) const;

        //status fields
        bool IsDead(const int index) const;
        bool IsLongGone(const int index) const;
        uint32_t ReceiveCount(const int index) const;
        uint32_t RetransmitCount(const int index) const;
        /**
         * Check if we have any remote statistics.
         */
        bool HasRemoteStatistics(const int index) const;

        /**
         * Will only return a valid object when called on a "top level" object.
         * I.e. you can't ask a remote node about its remote nodes...
         * Throws std::logic_error on error
         */
        RawStatistics RemoteStatistics(const int index) const;

        void Print(std::wostream&) const;

    private:
        friend class RawStatisticsCreator;

        class Impl;

        explicit RawStatistics(std::shared_ptr<Impl> impl)
            : m_impl(std::move(impl)) {}

        std::shared_ptr<Impl> m_impl;
    };

#ifdef _MSC_VER
#pragma warning (pop)
#endif

    class RawStatisticsSubscriber
    {
    public:

        virtual void Start(boost::asio::io_service& ioService,
                           const std::function<void (const RawStatistics& data)>& dataCallback) = 0;

        virtual void Stop() = 0;
    };


    static inline std::wostream& operator<<(std::wostream& out, const RawStatistics& statistics)
    {
        statistics.Print(out);
        return out;
    }
}
}
}
}

#endif 

