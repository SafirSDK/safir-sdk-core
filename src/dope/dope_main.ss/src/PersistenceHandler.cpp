/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrha
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
#include "PersistenceHandler.h"
#include <Safir/Dob/PersistenceParameters.h>
#include <Safir/Dob/InjectionProperty.h>
#include <Safir/Dob/InjectionOverrideProperty.h>
#include <Safir/Dob/PersistenceThrottlingProperty.h>
#include <Safir/Dob/PersistenceThrottlingOverrideProperty.h>
#include <Safir/Dob/PersistentDataReady.h>
#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Logging/Log.h>
#include <Safir/Dob/ConnectionAspectInjector.h>
#include <Safir/Dob/Typesystem/Internal/BlobOperations.h>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/NotFoundException.h>

//Dope uses context 0 to connect to the dob. The strange looking negative number
//is a way to indicate that this is a connection with special privileges.
const Safir::Dob::Typesystem::Int32 PERSISTENCE_CONTEXT = -1000000;

//-------------------------------------------------------
PersistenceHandler::PersistenceHandler(boost::asio::io_service& ioService,
                                       const bool ignorePersistenceProperties)
    : m_writeTimer(ioService),
      m_nextTimeout(boost::chrono::steady_clock::time_point::max()),
      m_dispatcher(m_dobConnection, ioService),
      m_debug(L"PersistenceHandler"),
      m_started(false)
{
    const Safir::Dob::Typesystem::TypeIdVector types =
        Safir::Dob::Typesystem::Operations::GetClassTree(Safir::Dob::Entity::ClassTypeId);
    m_debug << "-----These classes will be persisted--------" <<std::endl;


    for (const auto& type : types)
    {
        const boost::chrono::milliseconds writePeriod = GetWritePeriod(type);

        if (ShouldPersist(type))
        {
            if (ignorePersistenceProperties)
            {
                continue;
            }

            m_persistentTypes.insert(type);

            std::wostringstream os;

            if (m_debug.IsEnabled())
            {
                os << L"  " << Safir::Dob::Typesystem::Operations::GetName(type);
            }

            if (writePeriod > boost::chrono::milliseconds(0))
            {
                if (m_debug.IsEnabled())
                {
                    os << L" (will be stored no more frequently than every " << writePeriod << ")";
                }

                m_writePeriod[type] = writePeriod;
            }
            else
            {
                if (m_debug.IsEnabled())
                {
                    os << std::endl;
                }
            }

            if (m_debug.IsEnabled())
            {
                m_debug << os.str() << std::endl;
            }
        }
        else
        {
            // A non synchronous permanent type. Put out a warning if it happens to have
            // a PersistenceThrottlingProperty
            if (writePeriod > boost::chrono::milliseconds(0))
            {
                std::wostringstream os;
                os << "Ignoring PersistenceThrottlingProperty since "
                   << Safir::Dob::Typesystem::Operations::GetName(type) << " is not SynchronousPermanent";
                Safir::Logging::SendSystemLog(Safir::Logging::Warning, os.str());
            }
        }
    }
    m_debug << "---------------- End list ----------------" << std::endl;
    m_debug << std::flush;
}

//-------------------------------------------------------
PersistenceHandler::~PersistenceHandler()
{
}

//-------------------------------------------------------
void PersistenceHandler::Start(bool restore)
{
    if (m_started)
    {
        m_debug << "Persistence handling already started."<< std::endl;
        return;
    }

    m_debug << "Starting Persistence handling"<< std::endl;
    //m_dobConnection.Attach();
    try
    {
        m_dobConnection.Open(L"DOPE_SUBSCRIBE", L"0", PERSISTENCE_CONTEXT, nullptr, &m_dispatcher);
        m_debug << "Opened DOB connection DOPE_SUBSCRIBE"<<std::endl;
    }
    catch (Safir::Dob::NotOpenException e)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Critical,
                                      L"PersistenceHandler failed to connect to Dob, Maybe Dope is already running?");
        throw StartupError();
    }

    if (restore)
    {
        // Normal startup
        try
        {
            m_debug << "Restoring all stored entities"<< std::endl;
            RestoreAll();
        }
        catch (const Safir::Dob::AccessDeniedException & e)
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException
                (std::wstring(L"DOSE gave me AccessDeniedException when I was trying to Set persisted data! AccessDeniedException info: ") +
                e.GetExceptionInfo(),__WFILE__,__LINE__);
        }

        StartSubscriptions();
        ReportPersistentDataReady();
    }
    else
    {
        StartSubscriptions();
    }

    // Start timer
    HandleTimeout();

    m_started = true;
    m_debug << "Persistence handling successfully started"<<std::endl;
}

//-------------------------------------------------------
void PersistenceHandler::Stop()
{
    m_started = false;
    m_writeTimer.cancel();
    m_dobConnection.Close();
}


//-------------------------------------------------------
bool
PersistenceHandler::ShouldPersist(const Safir::Dob::Typesystem::TypeId typeId)
{
    try
    {
        // Check if override property
        bool hasProperty, isInherited;
        Safir::Dob::Typesystem::Operations::HasProperty(typeId,
                                                        Safir::Dob::InjectionOverrideProperty::ClassTypeId,
                                                        hasProperty,
                                                        isInherited);
        if ( hasProperty && !isInherited)
        { //has override property
            Safir::Dob::Typesystem::ObjectPtr obj =
                    Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(typeId);
            if (Safir::Dob::InjectionOverrideProperty::GetInjection(obj) == Safir::Dob::InjectionKind::SynchronousPermanent)
            {
                return true;
            }
            else
            {
                return false;
            }
        }


        // No override property check InjectionProperty
        if (Safir::Dob::Typesystem::Operations::HasProperty(typeId, Safir::Dob::InjectionProperty::ClassTypeId))
        { //normal persistence property
            Safir::Dob::Typesystem::ObjectPtr obj =
                    Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(typeId);
            if (Safir::Dob::InjectionProperty::GetInjection(obj) == Safir::Dob::InjectionKind::SynchronousPermanent)
            {
                return true;
            }
        }

        return false;
    }
    catch (const Safir::Dob::Typesystem::NullException &)
    {
        throw Safir::Dob::Typesystem::SoftwareViolationException(std::wstring(L"Failed to get persistence status for object ") +
                                                                   Safir::Dob::Typesystem::Operations::GetName(typeId),
                                                                 __WFILE__,__LINE__);
    }
}

//-------------------------------------------------------
boost::chrono::milliseconds
PersistenceHandler::GetWritePeriod(const Safir::Dob::Typesystem::TypeId typeId)
{
    try
    {
        Safir::Dob::Typesystem::Si32::Second result = -1;

        // Check if override property
        bool hasProperty;
        bool isInherited;
        Safir::Dob::Typesystem::Operations::HasProperty(typeId,
                                                        Safir::Dob::PersistenceThrottlingOverrideProperty::ClassTypeId,
                                                        hasProperty,
                                                        isInherited);
        if (hasProperty && !isInherited)
        {
            //has override property
            Safir::Dob::Typesystem::ObjectPtr obj =
                    Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(typeId);
            result = Safir::Dob::PersistenceThrottlingOverrideProperty::GetWritePeriod(obj);
        }
        // No override property check PersistenceThrottlingProperty
        else if (Safir::Dob::Typesystem::Operations::HasProperty(typeId,
                                                                 Safir::Dob::PersistenceThrottlingProperty::ClassTypeId))
        {
            //normal persistence property
            Safir::Dob::Typesystem::ObjectPtr obj =
                    Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(typeId);
            result = Safir::Dob::PersistenceThrottlingProperty::GetWritePeriod(obj);
        }

        return boost::chrono::milliseconds(static_cast<int>(result * 1000));
    }
    catch (const Safir::Dob::Typesystem::NullException &)
    {
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Failed to get persistence throttling period for object " +
                                                                 Safir::Dob::Typesystem::Operations::GetName(typeId),
                                                                 __WFILE__,__LINE__);
    }
}


//-------------------------------------------------------
void
PersistenceHandler::StartSubscriptions()
{
    Safir::Dob::ConnectionAspectInjector inject(m_dobConnection);

    for (const auto & elem : m_persistentTypes)
    {
        // Subscribing with updates only this type not subclasses.
        // GhostDeletes == true. Happens when a ghost object is actively deleted by owner app.
        // WantsLastState == true. When a owner app dies we want the last state the app had.
        // DoesntWantSourceIsPermanent == false. we do want our own sets, in case they're from a "joined" system.
        // timestampChangeInfo == false, we dont want change info at all...
        // WantsAllStateChanges == false. This is for DOSE only. Shall be false.
        inject.SubscribeEntity(elem,              // Dob::Typesystem::TypeId      typeId
                               true,             // bool                         includeUpdates
                               false,            // bool                         includeSubclasses
                               false,            // bool                         restartSubscription
                               true,             // bool                         wantsGhostDelete
                               true,             // bool                         wantsLastState
                               false,            // bool                         doesntWantSourceIsPermanentStore
                               false,            // bool                         wantsAllStateChanges
                               false,            // bool                         timestampChangeInfo
                               this );           // Dob::EntitySubscriber* const entitySubscriber
    }
    m_debug << "Subscriptions have been set up" <<std::endl;
}

//-------------------------------------------------------
void
PersistenceHandler::Write(const Safir::Dob::EntityProxy& entityProxy, const bool update)
{
    m_debug << "Will try to persist entity " << entityProxy.GetEntityId() << std::endl;

    const char* blob = entityProxy.GetBlob();

    Safir::Dob::Typesystem::BinarySerialization bin =
        std::vector<char>(blob,blob+Safir::Dob::Typesystem::Internal::BlobOperations::GetSize(blob));

    Store(entityProxy.GetEntityId(), entityProxy.GetOwner(), bin, update);
}

//-------------------------------------------------------
void
PersistenceHandler::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    HandleEntity(entityProxy, false);
}

//-------------------------------------------------------
void
PersistenceHandler::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{
    HandleEntity(entityProxy, true);
}

//-------------------------------------------------------
void
PersistenceHandler::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy,
                                    const bool                    deletedByOwner)
{
    // Remove it from the throttling structure if it happens to be there
    m_toBeWritten.erase(entityProxy.GetEntityId());

    // only remove if removed by owner... Otherwise it is probably because the system shut down or the application died
    if( !deletedByOwner )
    {
        return;
    }

    m_debug << "Removing entity " << entityProxy.GetEntityId() << std::endl;
    Remove(entityProxy);
}

//-------------------------------------------------------
void
PersistenceHandler::HandleTimeout()
{
    const auto now = boost::chrono::steady_clock::now();

    // Don't iterate through all unless we are due for a write timeout.
    if (now > m_nextTimeout)
    {
        m_nextTimeout = boost::chrono::steady_clock::time_point::max();

        for (auto& entity : m_toBeWritten)
        {
            // Should we write this object?
            if (entity.second.second && entity.second.first < now)
            {
                // The object is dirty and its time to write it
                try
                {
                    const Safir::Dob::EntityProxy entityProxy = m_dobConnection.Read(entity.first);

                    m_debug << "Periodic write time ended for entity " << entityProxy.GetEntityId() << std::endl;

                    Write(entityProxy,true);

                    // Set next write time ...
                    entity.second.first = now + m_writePeriod[entity.first.GetTypeId()];

                    // ...and reset dirty flag
                    entity.second.second = false;
                }
                catch(const Safir::Dob::NotFoundException &)
                {
                    // This could happen if the instance has been deleted but we haven't been told yet
                    // Just fall through and erase it from our internal structure. The delete callback will
                    // take care of this.
                }
            }

            if (entity.second.first < m_nextTimeout)
            {
                m_nextTimeout = entity.second.first;
            }
        }
    }

    m_writeTimer.expires_from_now(boost::chrono::seconds(1));
    m_writeTimer.async_wait([this](const boost::system::error_code& error)
    {
        if (!error)
        {
            HandleTimeout();
        }
    });
}


//-------------------------------------------------------
void
PersistenceHandler::HandleEntity(const Safir::Dob::EntityProxy& entityProxy, const bool update)
{
    auto writePeriodIt = m_writePeriod.find(entityProxy.GetTypeId());

    if (writePeriodIt != m_writePeriod.cend())
    {
        // This entity type has a write period limit

        const auto now = boost::chrono::steady_clock::now();

        const auto writePeriod = writePeriodIt->second;

        auto toBeWrittenIt = m_toBeWritten.find(entityProxy.GetEntityId());

        if (toBeWrittenIt == m_toBeWritten.cend())
        {
            // If the entity instance doesn't exist in the map then this instance has never been written.
            // Write it right away!
            Write(entityProxy, update);

            m_toBeWritten[entityProxy.GetEntityId()] = std::make_pair(now + writePeriod,
                                                                      false);  // dirty flag
            m_nextTimeout = now; // Force timeout and a recalculation of nextTimeout
        }
        else if (m_toBeWritten[entityProxy.GetEntityId()].first < now)
        {
            // This instance hasn't been written within its write period
            // Write it right away!
            Write(entityProxy, update);

            toBeWrittenIt->second.first = now + writePeriod; // next timeout
            toBeWrittenIt->second.second = false;            // dirty flag
            m_nextTimeout = now; // Force timeout and a recalculation of nextTimeout
        }
        else
        {
            // Otherwise, mark the entity as dirty and wait until the timer strikes.
            toBeWrittenIt->second.second = true;
        }
    }
    else
    {
        Write(entityProxy, update);
    }
}


//-------------------------------------------------------
void
PersistenceHandler::ReportPersistentDataReady()
{
    m_debug << "Sending PersistentDataReady request" << std::endl;
    Safir::Dob::PersistentDataReadyPtr request = Safir::Dob::PersistentDataReady::Create();

    m_dobConnection.ServiceRequest(request,Safir::Dob::Typesystem::HandlerId(), this);

    //Not handling OverflowException since there is only one request sent from DOPE.
}

//-------------------------------------------------------
void
PersistenceHandler::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
{
    m_debug << "Got this response to the PersistenceDataReady request:" << std::endl
            << Safir::Dob::Typesystem::Serialization::ToXml(responseProxy.GetBlob()) << std::endl;

    if (!responseProxy.IsSuccess())
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                      L"Did not get a SuccessResponse to the PersistentDataReady request to Dose. Response: "
                                      + Safir::Dob::Typesystem::Serialization::ToXml(responseProxy.GetBlob()));
    }
}

//-------------------------------------------------------
void
PersistenceHandler::OnNotRequestOverflow()
{
    //Not handling OverflowException since there is only one request sent from DOPE.
}
