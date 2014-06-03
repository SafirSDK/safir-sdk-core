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
#ifndef __SAFIR_DOB_INTERNAL_SYSTEM_STATE_H__
#define __SAFIR_DOB_INTERNAL_SYSTEM_STATE_H__

#include <string>
#include <ostream>
#include <boost/cstdint.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/function.hpp>
#include <Safir/Dob/Internal/SystemPictureDefs.h>

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
     * This is an interface the System State information produced by System Picture.
     *
     * Use functionality in SystemPicture class to get an instance to one
     * of these objects.
     */
    class DISTRIBUTION_SYSTEM_PICTURE_API SystemState
    {
    public:
        boost::int64_t ElectedId() const;
        
        int Size() const;

        //Static fields
        const std::string& Name(const int index) const;
        boost::int64_t Id(const int index) const;
        boost::int64_t NodeTypeId(const int index) const;
        const std::string& ControlAddress(const int index) const;
        const std::string& DataAddress(const int index) const;

        void Print(std::wostream&) const;

    private:
        friend class SystemStateCreator;

        class Impl;

        explicit SystemState(boost::shared_ptr<Impl>  impl)
            : m_impl(std::move(impl)) {}

        boost::shared_ptr<Impl> m_impl;

    };

#ifdef _MSC_VER
#pragma warning (pop)
#endif

    class SystemStateSubscriber
    {
    public:
        virtual void Start(boost::asio::io_service& ioService,
                           const boost::function<void (const SystemState& data)>& dataCallback) = 0;

        virtual void Stop() = 0;
    };



    static inline std::wostream& operator<<(std::wostream& out, const SystemState& state)
    {
        state.Print(out);
        return out;
    }
}
}
}
}

#endif

