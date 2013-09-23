/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safir.sourceforge.net)
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

