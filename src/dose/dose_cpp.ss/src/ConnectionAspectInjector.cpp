/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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
#include <Safir/Dob/ConnectionAspectInjector.h>

#include <Safir/Dob/Internal/Interface.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Dob/Typesystem/Serialization.h>

namespace Safir
{
namespace Dob
{
    void ConnectionAspectInjector::InjectChanges(const Dob::EntityPtr&                entity,
                                                 const Dob::Typesystem::InstanceId&   instanceId,
                                                 const Dob::Typesystem::Int64         timestamp,
                                                 const Dob::Typesystem::HandlerId&    handlerId) const
    {
        bool success;

        Typesystem::BinarySerialization bin;
        Typesystem::Serialization::ToBinary(entity, bin);
        //TODO: serialize straight to shared memory
        DoseC_InjectEntity(GetControllerId(),
                           &bin[0],
                           instanceId.GetRawValue(),
                           instanceId.Utf8String().c_str(),
                           handlerId.GetRawValue(),
                           handlerId.Utf8String().c_str(),
                           timestamp,
                           success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    void ConnectionAspectInjector::InjectDelete(const Dob::Typesystem::EntityId&     entityId,
                                                const Dob::Typesystem::Int64         timestamp,
                                                const Dob::Typesystem::HandlerId&    handlerId) const
    {
        bool success;
        DoseC_InjectDeletedEntity(GetControllerId(),
                                  entityId.GetTypeId(),
                                  entityId.GetInstanceId().GetRawValue(),
                                  entityId.GetInstanceId().Utf8String().c_str(),
                                  handlerId.GetRawValue(),
                                  handlerId.Utf8String().c_str(),
                                  timestamp,
                                  success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

    void ConnectionAspectInjector::InitialSet(const Dob::EntityPtr&              entity,
                                              const Dob::Typesystem::InstanceId& instanceId,
                                              const Dob::Typesystem::HandlerId&  handlerId) const
    {
        bool success;

        Typesystem::BinarySerialization bin;
        Typesystem::Serialization::ToBinary(entity, bin);
        //TODO: serialize straight to shared memory
        DoseC_SetEntity(GetControllerId(),
                        &bin[0],
                        instanceId.GetRawValue(),
                        instanceId.Utf8String().c_str(),
                        handlerId.GetRawValue(),
                        handlerId.Utf8String().c_str(),
                        false,   // false => don't consider change flags
                        true,    // true => this is an initial injection
                        success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }


    void ConnectionAspectInjector::SubscribeEntity(const Dob::Typesystem::TypeId      typeId,
                                                   const bool                         includeUpdates,
                                                   const bool                         includeSubclasses,
                                                   const bool                         restartSubscription,
                                                   const bool                         wantsGhostDelete,
                                                   const bool                         wantsLastState,
                                                   const bool                         doesntWantSourceIsPermanentStore,
                                                   const bool                         wantsAllStateChanges,
                                                   const bool                         timestampChangeInfo,
                                                   Dob::EntitySubscriber* const       entitySubscriber) const
    {
        bool success;
        DoseC_InjectorSubscribeEntity(GetControllerId(),
                                      typeId,
                                      includeUpdates,
                                      includeSubclasses,
                                      restartSubscription,
                                      wantsGhostDelete,
                                      wantsLastState,
                                      doesntWantSourceIsPermanentStore,
                                      wantsAllStateChanges,
                                      timestampChangeInfo,
                                      DOSE_LANGUAGE_CPP,
                                      static_cast<Internal::ConsumerBase* const>(entitySubscriber),
                                      success);
        if (!success)
        {
            Typesystem::LibraryExceptions::Instance().Throw();
        }
    }

}
}
