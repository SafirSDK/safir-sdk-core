/******************************************************************************
*
* Copyright Saab Systems AB, 2012 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
*
******************************************************************************/
#ifndef __DOSE_MAIN_CONNECTION_KILLER_H__
#define __DOSE_MAIN_CONNECTION_KILLER_H__

#include <boost/noncopyable.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class ConnectionKiller
        : private boost::noncopyable
    {
    public:
        ~ConnectionKiller();
    };
}
}
}

#endif

