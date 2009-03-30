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
#include "PersistenceHandler.h"
#include <Safir/Dob/PersistenceParameters.h>
#include <Safir/Dob/InjectionProperty.h>
#include <Safir/Dob/InjectionOverrideProperty.h>
#include <Safir/Dob/PersistentDataReady.h>
#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/SwReports/SwReport.h>
#include <Safir/Dob/ConnectionAspectInjector.h>
#include <Safir/Dob/Typesystem/BlobOperations.h>

//-------------------------------------------------------
PersistenceHandler::PersistenceHandler():
    m_debug(L"PersistenceHandler")
{
    m_persistentTypes.reset(new TypeIdSet());

    const Safir::Dob::Typesystem::TypeIdVector types =
        Safir::Dob::Typesystem::Operations::GetClassTree(Safir::Dob::Entity::ClassTypeId);
    m_debug << "-----These classes will be persisted--------" <<std::endl;
    for (Safir::Dob::Typesystem::TypeIdVector::const_iterator it = types.begin();
         it != types.end(); ++it)
    {
        if (ShouldPersist(*it))
        {
            m_debug << "  " << Safir::Dob::Typesystem::Operations::GetName(*it) << std::endl;
            m_persistentTypes->insert(*it);
        }
    }
    m_debug << "---------------- End list ----------------" <<std::endl;
    m_debug << std::flush;
}

//-------------------------------------------------------
PersistenceHandler::~PersistenceHandler()
{
}

//-------------------------------------------------------
void
PersistenceHandler::Start()
{
    m_debug << "Starting Persistence handling"<< std::endl;
    m_dobConnection.Attach();

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

    //we do not need the list of types any longer, so we free the memory.
    m_persistentTypes->clear();
    m_persistentTypes.reset();

    m_debug << "PersistenceHandler::Allocate successful"<<std::endl;
}

//-------------------------------------------------------
bool
PersistenceHandler::ShouldPersist(const Safir::Dob::Typesystem::TypeId typeId)
{
    try
    {
        bool hasProperty, isInherited;
        Safir::Dob::Typesystem::Operations::HasProperty(typeId, Safir::Dob::InjectionOverrideProperty::ClassTypeId, hasProperty, isInherited);
        if ( hasProperty && !isInherited)
        { //has override property
            Safir::Dob::Typesystem::ObjectPtr obj = Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(typeId);
            if (Safir::Dob::InjectionOverrideProperty::GetInjection(obj) == Safir::Dob::InjectionKind::SynchronousPermanent)
            {
                return true;
            }
        }
        if (Safir::Dob::Typesystem::Operations::HasProperty(typeId, Safir::Dob::InjectionProperty::ClassTypeId))
        { //normal persistence property
            Safir::Dob::Typesystem::ObjectPtr obj = Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(typeId);
            if (Safir::Dob::InjectionProperty::GetInjection(obj) == Safir::Dob::InjectionKind::SynchronousPermanent)
            {
                return true;
            }
        }
        return false;
    }
    catch (const Safir::Dob::Typesystem::NullException &)
    {
        throw Safir::Dob::Typesystem::SoftwareViolationException(std::wstring(L"Failed to get persistance status for object ") +
                                                                   Safir::Dob::Typesystem::Operations::GetName(typeId),
                                                                 __WFILE__,__LINE__);
    }
}


//-------------------------------------------------------
void
PersistenceHandler::StartSubscriptions()
{
    Safir::Dob::ConnectionAspectInjector inject(m_dobConnection);

    for (TypeIdSet::const_iterator it = m_persistentTypes->begin();
        it != m_persistentTypes->end(); ++it)
    {
        // Subscribing with updates only this type not subclasses.
        // GhostDeletes == true. Happens when a ghost object is actively deleted by owner app.
        // WantsLastState == true. When a owner app dies we want the last state the app had.
        // DoesntWantSourceIsPermanent == true. we dont want our own sets
        // timestampChangeInfo == false, we dont want change info at all...
        // WantsAllStateChanges == false. This is for DOSE only. Shall be false.
        inject.SubscribeEntity(*it,     // Dob::Typesystem::TypeId      typeId
                               true,    // bool                         includeUpdates
                               false,   // bool                         includeSubclasses
                               false,   // bool                         restartSubscription
                               true,    // bool                         wantsGhostDelete
                               true,    // bool                         wantsLastState
                               true,    // bool                         doesntWantSourceIsPermanentStore
                               false,   // bool                         wantsAllStateChanges
                               false,   // bool                         timestampChangeInfo
                               this );  // Dob::EntitySubscriber* const entitySubscriber
    }
    m_debug << "Subscriptions have been set up" <<std::endl;
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
PersistenceHandler::HandleEntity(const Safir::Dob::EntityProxy & entityProxy, const bool update)
{
    m_debug << "Got entity " << entityProxy.GetEntityId() << ", will try to persist it" << std::endl;
    const char * blob = entityProxy.GetBlob();
    Safir::Dob::Typesystem::BinarySerialization bin = 
    std::vector<char>(blob,blob+Safir::Dob::Typesystem::BlobOperations::GetSize(blob));

    Store(entityProxy.GetEntityId(), entityProxy.GetOwner(), bin, update);
}


//-------------------------------------------------------
void
PersistenceHandler::ReportPersistentDataReady()
{
    m_debug << "Sending PersistentDataReady request" << std::endl;
    Safir::Dob::PersistentDataReadyPtr request = Safir::Dob::PersistentDataReady::Create();

    Safir::Dob::RequestId reqId;
    reqId = m_dobConnection.ServiceRequest(request,Safir::Dob::Typesystem::HandlerId(), this);

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
        std::wostringstream ostr;
        ostr << "Did not get a SuccessResponse to the PersistentDataReady request to DOSE!" << std::endl
            << "Response as xml: " << std::endl
            << Safir::Dob::Typesystem::Serialization::ToXml(responseProxy.GetBlob());
        Safir::SwReports::SendErrorReport
            (L"Request error",
            L"PersistenceHandler::OnResponse",
            ostr.str());
    }
}

//-------------------------------------------------------
void
PersistenceHandler::OnNotRequestOverflow()
{
    //Not handling OverflowException since there is only one request sent from DOPE.
}

