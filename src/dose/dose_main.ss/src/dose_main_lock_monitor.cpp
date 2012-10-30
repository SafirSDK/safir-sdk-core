/******************************************************************************
*
* Copyright Saab AB, 2012 (http://www.safirsdk.com)
*
* Created by: Anders Widén / aiwi
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

#include "dose_main_lock_monitor.h"
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/PanicLogging.h>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4127)
#endif
#include <boost/date_time/posix_time/posix_time.hpp>
#if defined _MSC_VER
  #pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    LockMonitor::LockMonitor()
        :  m_serviceTypeIds(),
           m_entityTypeIds(),
           m_loggingIsEnabled(true),
           m_checkerThread()
    {
        // Collect all service types ...
        m_serviceTypeIds = Dob::Typesystem::Operations::GetClassTree(Safir::Dob::Service::ClassTypeId);

        // ... and all entity types.
        m_entityTypeIds = Dob::Typesystem::Operations::GetClassTree(Safir::Dob::Entity::ClassTypeId);

        // Start tread
        m_checkerThread = boost::thread(&LockMonitor::Check, this);
    }

    LockMonitor::~LockMonitor()
    {
        m_checkerThread.interrupt();
        m_checkerThread.join();
    }

    void LockMonitor::Check()
    {
        try
        {
            // We use the parameter DoseMainThreadWatchdogTimeout divided by 4 to determine how long to wait for a single lock. 
            const boost::posix_time::seconds lockTimeout(Safir::Dob::NodeParameters::DoseMainThreadWatchdogTimeout() / 4);

            // Wait for the shared memory to be initialized
            for (;;)
            {
                if (Safir::Dob::Internal::ServiceTypes::IsInitialized() &&
                    Safir::Dob::Internal::EntityTypes::IsInitialized())
                {
                    break;
                }
                boost::this_thread::sleep(boost::posix_time::milliseconds(10));
            }

            for (;;)
            {

                boost::this_thread::sleep(boost::posix_time::seconds(15));

                bool serviceLocksOk = true;
                for (Dob::Typesystem::TypeIdVector::const_iterator it = m_serviceTypeIds.begin();
                    it != m_serviceTypeIds.end();
                    ++it)
                {
                    for (ContextId context = 0; context < Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
                    {
                        if (!Safir::Dob::Internal::ServiceTypes::Instance().CanAcquireContainerWriterLock(*it, context, lockTimeout))
                        {
                            if (m_loggingIsEnabled)
                            {
                                std::ostringstream ostr;
                                ostr << "Have tried " << lockTimeout.total_seconds() <<
                                    " seconds to get an exclusive lock (type lock or registration container) "
                                    "for type " << Safir::Dob::Typesystem::Utilities::ToUtf8(Typesystem::Operations::GetName(*it)) <<
                                    " in context " << context << ". Probably an application has "
                                    "terminated in an improper way leaving a locked lock behind!" << std::endl;
                                lllerr << ostr.str().c_str();
                                Safir::Utilities::Internal::PanicLogging::Log(ostr.str());
                            }
                            serviceLocksOk = false;
                        }                    
                    }
                }

                bool entityLocksOk = true;
                for (Dob::Typesystem::TypeIdVector::const_iterator it = m_entityTypeIds.begin();
                    it != m_entityTypeIds.end();
                    ++it)
                {
                    for (ContextId context = 0; context < Safir::Dob::NodeParameters::NumberOfContexts(); ++context)
                    {
                        if (!Safir::Dob::Internal::EntityTypes::Instance().CanAcquireContainerWriterLock(*it, context, lockTimeout))
                        {
                            if (m_loggingIsEnabled)
                            {
                                std::ostringstream ostr;
                                ostr << "Have tried " << lockTimeout.total_seconds() <<
                                    " seconds to get an exclusive lock (type lock, registration container lock or entity container lock) "
                                    "for type " << Safir::Dob::Typesystem::Utilities::ToUtf8(Typesystem::Operations::GetName(*it)) <<
                                    " in context " << context << ". Probably an application has "
                                    "terminated in an improper way leaving a locked lock behind!" << std::endl;
                                lllerr << ostr.str().c_str();
                                Safir::Utilities::Internal::PanicLogging::Log(ostr.str());
                            }
                            entityLocksOk = false;
                        }
                    }
                } 

                if (!serviceLocksOk || !entityLocksOk)
                {
                    std::ostringstream ostr;
                    ostr << "Seems one or more shared memory locks are abandoned in a locked state!!" << '\n';
                    if (Safir::Dob::NodeParameters::TerminateDoseMainWhenUnrecoverableError())
                    {
                        ostr << "Parameter TerminateDoseMainWhenUnrecoverableError is set to true"
                            " which means that dose_main will now be terminated!!" << std::endl;
                        lllerr << ostr.str().c_str();
                        Safir::Utilities::Internal::PanicLogging::Log(ostr.str());

                        boost::this_thread::sleep(boost::get_system_time() + boost::posix_time::seconds(5));

                        abort(); // Terminate dose_main!!!!
                    }
                    else if (m_loggingIsEnabled)
                    {
                        ostr << "Parameter TerminateDoseMainWhenUnrecoverableError is set to false"
                            " which means that dose_main will not be terminated!!" << std::endl;
                        lllerr << ostr.str().c_str();
                        Safir::Utilities::Internal::PanicLogging::Log(ostr.str());
                        m_loggingIsEnabled = false;
                    } 
                }
            }
        }
        catch (boost::thread_interrupted&)
        {
            // Thread was interrupted, which is expected behaviour. By catching this exception we make
            // sure that the whole program is not aborted, which could otherwise be the case on some platforms
            // where an unhandled exception in a thread brings down the whole program.
        }
    }
}
}
}
