/******************************************************************************
*
* Copyright Saab Systems AB, 2012 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
*
******************************************************************************/
#ifndef __DOSE_MAIN_SIGNAL_HANDLER_H__
#define __DOSE_MAIN_SIGNAL_HANDLER_H__

#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class SignalHandler
        : private boost::noncopyable
    {
    public:
        explicit SignalHandler(boost::asio::io_service& ioService);
    private:
        class Impl;
        boost::shared_ptr<Impl> m_impl;
    };

}
}
}

#endif

