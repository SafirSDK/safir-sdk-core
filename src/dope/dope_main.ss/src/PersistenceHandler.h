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
#pragma once

#include "Defs.h"

#include <Safir/Dob/Connection.h>
#include <Safir/Application/Tracer.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <boost/chrono.hpp>
#include <boost/noncopyable.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

/**
 * Abstract base class for all persistence backends.
 */
class PersistenceHandler :
    public Safir::Dob::EntitySubscriber,
    public Safir::Dob::Requestor,
    private boost::noncopyable
{
public:
    /**
     * Constructor
     *
     * @param ignorePersistenceProperties: should only be set to true if implementing "no
     * persistence" backend
     */
    PersistenceHandler(boost::asio::io_service& ioService,
                       const bool ignorePersistenceProperties);

    /** Destructor */
    virtual ~PersistenceHandler();

    void Start(bool restore);
    void Stop();

    // From Safir::Dob::EntitySubscriber
    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy) override;
    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy) override;
    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy,
                                 const bool                    deletedByOwner) override;

    // From Safir::Dob::Requestor
    void OnResponse(const Safir::Dob::ResponseProxy responseProxy) override;
    void OnNotRequestOverflow() override;

protected:

    const TypeIdSet & GetPersistentTypes() const {return m_persistentTypes;}
    Safir::Dob::Connection m_dobConnection;


private:
    void ReportPersistentDataReady();
    void StartSubscriptions();

    void Write(const Safir::Dob::EntityProxy& entityProxy, const bool update);

    /**
     * Persist an object. ObjectId of the object should be used as key.
     */
    virtual void Store(const Safir::Dob::Typesystem::EntityId& entityId,
                       const Safir::Dob::Typesystem::HandlerId& handlerId,
                       Safir::Dob::Typesystem::BinarySerialization& bin,
                       const bool update) = 0;

    /**
     * Remove an object from storage.
     */
    virtual void Remove(const Safir::Dob::EntityProxy & entityProxy) = 0;

    /**
     * Remove all objects from storage.
     */
    virtual void RemoveAll() = 0;

    /**
     * Restore all objects from the storage.
     * Should also remove stuff that is no longer to be stored.
     */
    virtual void RestoreAll() = 0;

    static bool ShouldPersist(const Safir::Dob::Typesystem::TypeId typeId);

    static boost::chrono::milliseconds GetWritePeriod(const Safir::Dob::Typesystem::TypeId typeId);

    void HandleEntity(const Safir::Dob::EntityProxy & entityProxy, const bool update);

    void HandleTimeout();

    boost::asio::steady_timer               m_writeTimer;
    boost::chrono::steady_clock::time_point m_nextTimeout;

    Safir::Utilities::AsioDispatcher m_dispatcher;

    TypeIdSet m_persistentTypes;

    std::map<Safir::Dob::Typesystem::TypeId, boost::chrono::milliseconds> m_writePeriod;

    std::map<Safir::Dob::Typesystem::EntityId,
             std::pair<boost::chrono::steady_clock::time_point, bool>> m_toBeWritten;

    Safir::Application::Tracer m_debug;

    bool m_started;
};
