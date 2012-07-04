/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
#ifndef __DOPE_PERSISTENCE_HANDLER_H__
#define __DOPE_PERSISTENCE_HANDLER_H__

#include "Defs.h"

#include <Safir/Dob/Connection.h>
#include <Safir/Application/Tracer.h>
#include <Safir/Utilities/AsioDispatcher.h>
#include <boost/asio.hpp>
#include <boost/noncopyable.hpp>

/**
 * Abstract base class for all persistance backends.
 */
class PersistenceHandler :
    public Safir::Dob::EntitySubscriber,
    public Safir::Dob::Requestor,
    private boost::noncopyable
{
public:
    /** Constructor */
    explicit PersistenceHandler(boost::asio::io_service& ioService);

    /** Destructor */
    virtual ~PersistenceHandler();

    void Start(bool restore);
    void Stop();

    // From Safir::Dob::EntitySubscriber
    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy);
    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy);
    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, 
                                 const bool                    deletedByOwner);

    // From Safir::Dob::Requestor
    void OnResponse(const Safir::Dob::ResponseProxy responseProxy);
    void OnNotRequestOverflow();

protected:
    void StartSubscriptions(const bool subscribeAll);

    void ReportPersistentDataReady();

    Safir::Utilities::AsioDispatcher m_dispatcher;
    Safir::Dob::Connection  m_dobConnection;

    const TypeIdSet & GetPersistentTypes() const {return *m_persistentTypes;}

private:
    /**
     * Persist an object. ObjectId of the object should be used as key.
     */
    virtual void Store(const Safir::Dob::Typesystem::EntityId entityId,
                       const Safir::Dob::Typesystem::HandlerId handlerId,
                       Safir::Dob::Typesystem::BinarySerialization & bin,
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

    void HandleEntity(const Safir::Dob::EntityProxy & entityProxy, const bool update);

    //This is set up by the constructor of the persistance handler
    //and can be used in any way by the derived class.
    //It should be cleared after allocation though...
    //It is used by the StartSubscriptions call to know which classes to subscribe to.
    boost::shared_ptr<TypeIdSet> m_persistentTypes;

    Safir::Application::Tracer m_debug;

    bool m_started;
};
#endif
