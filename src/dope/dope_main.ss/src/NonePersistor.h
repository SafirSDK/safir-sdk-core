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

#include "PersistenceHandler.h"
#include <Safir/Application/Tracer.h>


/**
 * Uses file system for Persistent storage.
 */
class NonePersistor :
    public PersistenceHandler
{
public:
    /**
     * Constructor
     */
    explicit NonePersistor(boost::asio::io_service& ioService)
        : PersistenceHandler(ioService, true)
    {

    }

private:
    void Store(const Safir::Dob::Typesystem::EntityId& /*entityId*/,
               const Safir::Dob::Typesystem::HandlerId& /*handlerId*/,
               Safir::Dob::Typesystem::BinarySerialization & /*bin*/,
               const bool /*update*/) override
    {throw std::logic_error("Unexpected call to NonePersistor::Store(...)");}

    void RestoreAll() override
    {
        if (!GetPersistentTypes().empty())
        {
            throw std::logic_error("Unexpected persisted types in NonePersistor::RestoreAll()");
        }
    }

    void Remove(const Safir::Dob::EntityProxy & /*entityProxy*/) override
    {throw std::logic_error("Unexpected call to NonePersistor::Remove(...)");}

    void RemoveAll() override
    {throw std::logic_error("Unexpected call to NonePersistor::RemoveAll()");}

};
