/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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
#include <boost/filesystem/path.hpp>
#include <boost/tuple/tuple.hpp>
#include <Safir/Application/Tracer.h>


/**
 * Uses file system for Persistent storage.
 */
class FilePersistor :
    public PersistenceHandler
{
public:
    typedef boost::tuple<Safir::Dob::Typesystem::EntityId, Safir::Dob::Typesystem::HandlerId, std::wstring> EntityIdAndHandlerId;


    /**
     * Constructor
     */
    explicit FilePersistor(boost::asio::io_service& ioService);

private:
    void Store(const Safir::Dob::Typesystem::EntityId& entityId,
               const Safir::Dob::Typesystem::HandlerId& handlerId,
               Safir::Dob::Typesystem::BinarySerialization& bin,
               const bool update) override;

    void RestoreAll() override;
    void Remove(const Safir::Dob::EntityProxy & entityProxy) override;
    void RemoveAll() override;

    boost::filesystem::path GetFilePath(const EntityIdAndHandlerId& entityAndHandler) const;

    Safir::Dob::EntityPtr RestoreBinary(const boost::filesystem::path & path) const;
    Safir::Dob::EntityPtr RestoreXml(const boost::filesystem::path & path) const;

    void RemoveFile(const boost::filesystem::path& path) const;

    boost::filesystem::path m_storagePath;

    Safir::Application::Tracer m_debug;
};

