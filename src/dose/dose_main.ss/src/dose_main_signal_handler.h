/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars@foldspace.nu
*
******************************************************************************/
#ifndef __DOSE_MAIN_SIGNAL_HANDLER_H__
#define __DOSE_MAIN_SIGNAL_HANDLER_H__

#include <boost/shared_ptr.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

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

