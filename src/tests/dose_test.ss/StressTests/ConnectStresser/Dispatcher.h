/******************************************************************************
*
* Copyright Saab AB, 2006-2015 (http://safirsdkcore.com)
*
* Created by: Samuel Waxin / samuel.waxin@consoden.se
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

#ifndef DISPATCHER_H
#define DISPATCHER_H

#include <Safir/Utilities/Internal/Atomic.h>
#include <Safir/Dob/Connection.h>


class Dispatcher:
    public Safir::Dob::Dispatcher,
    private boost::noncopyable
{
public:
    Dispatcher(const boost::function<void(void)> & dispatchCallback,
               boost::asio::io_context & ioContext)
        : m_dispatchCallback(dispatchCallback)
        , m_isNotified(0)
        , m_ioContext(ioContext)
    {}

private:
    void OnDoDispatch() override
    {
        if (m_isNotified == 0)
        {
            m_isNotified = 1;
            boost::asio::post(m_ioContext, [this]{Dispatch();});
        }
    }

    virtual void Dispatch()
    {
        m_isNotified = 0;
        m_dispatchCallback();
    }

    const boost::function<void(void)> m_dispatchCallback;
    Safir::Utilities::Internal::AtomicUint32 m_isNotified;
    boost::asio::io_context & m_ioContext;
};

#endif // DISPATCHER_H
