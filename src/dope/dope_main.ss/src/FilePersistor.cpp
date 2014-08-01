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
#include "FilePersistor.h"

#include <Safir/Dob/PersistenceParameters.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Logging/Log.h>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/exception.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/integer_traits.hpp>
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4702)
#endif

#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include <fstream>
#include <string>
#include <Safir/Dob/ConnectionAspectInjector.h>

namespace 
{
    const boost::filesystem::path replace_extension(boost::filesystem::path path,const std::string& ext)
    {
        return path.replace_extension(ext);
    }

}
//-------------------------------------------------------
void FilePersistor::RemoveFile(const boost::filesystem::path& path) const
{
    try
    {
        if (boost::filesystem::exists(path))
        {
            m_debug << "Removing file " << path.string().c_str() << std::endl; 
            boost::filesystem::remove(path);
        }
    }
    catch (const boost::filesystem::filesystem_error &)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                      L"Failed to remove persistence file " +
                                      Safir::Dob::Typesystem::Utilities::ToWstring(path.string())
                                      + L", maybe it is read-only?");
    }    
}


//-------------------------------------------------------
const boost::filesystem::path GetStorageDirectory()
{
    try
    {
        boost::filesystem::path path;// = boost::filesystem::path(str,boost::filesystem::native);

        path /= Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Dob::PersistenceParameters::FileStoragePath());

        if (boost::filesystem::exists(path))
        {
            if (!boost::filesystem::is_directory(path))
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException
                    (Safir::Dob::Typesystem::Utilities::ToWstring(path.string()) + L" is not a directory",__WFILE__,__LINE__);
            }
        }
        else
        {
            boost::filesystem::create_directories(path);
        }

        return path;
    }
    catch (const boost::filesystem::filesystem_error & e)
    {
        throw Safir::Dob::Typesystem::SoftwareViolationException
            (L"Failed to get hold of the directory for file persistence. Got this info from boost::filesystem::filesystem_error" +
            Safir::Dob::Typesystem::Utilities::ToWstring(e.what()),__WFILE__,__LINE__);
    }
}

//-------------------------------------------------------
const FilePersistor::EntityIdAndHandlerId
Filename2EntityIdAndHandlerId(const boost::filesystem::path & filename)
{
    if (!filename.has_filename())
    {
        throw Safir::Dob::Typesystem::IllegalValueException
            (L"Filename2EntityAndHandler: Could not decompose filename : " +
            Safir::Dob::Typesystem::Utilities::ToWstring(filename.string()),__WFILE__,__LINE__);
    }

#if defined (BOOST_FILESYSTEM_VERSION) && BOOST_FILESYSTEM_VERSION == 3
    const std::string leaf = filename.filename().string();
#else
    const std::string leaf = filename.filename();
#endif

    size_t separatorIndex = leaf.find('@');
    if (separatorIndex == std::string::npos)
    {
        throw Safir::Dob::Typesystem::IllegalValueException
            (L"Filename2EntityAndHandler: Could not decompose filename : " +
            Safir::Dob::Typesystem::Utilities::ToWstring(filename.string()),__WFILE__,__LINE__);
    }

    const std::string typeName = leaf.substr(0,separatorIndex);

    size_t secondSeparatorIndex = leaf.find('@',separatorIndex+1);

    if (secondSeparatorIndex == std::string::npos)
    {
        throw Safir::Dob::Typesystem::IllegalValueException
            (L"Filename2EntityAndHandler: Could not decompose filename : " +
            Safir::Dob::Typesystem::Utilities::ToWstring(filename.string()),__WFILE__,__LINE__);
    }
    const std::string instance = leaf.substr(separatorIndex + 1,secondSeparatorIndex - separatorIndex - 1);

    size_t extIndex = leaf.find('.',secondSeparatorIndex);

    if (extIndex == std::string::npos)
    {
        throw Safir::Dob::Typesystem::IllegalValueException
            (L"Filename2EntityAndHandler: Could not decompose filename : " +
            Safir::Dob::Typesystem::Utilities::ToWstring(filename.string()),__WFILE__,__LINE__);
    }
    const std::string handler = leaf.substr(secondSeparatorIndex + 1,extIndex - secondSeparatorIndex - 1);

    return boost::make_tuple(Safir::Dob::Typesystem::EntityId
        (Safir::Dob::Typesystem::Operations::GetTypeId(Safir::Dob::Typesystem::Utilities::ToWstring(typeName)),
        Safir::Dob::Typesystem::InstanceId( boost::lexical_cast<Safir::Dob::Typesystem::Int64>(instance))),
        Safir::Dob::Typesystem::HandlerId(boost::lexical_cast<Safir::Dob::Typesystem::Int64>(handler)),
        Safir::Dob::Typesystem::Utilities::ToWstring(typeName));
}

//-------------------------------------------------------
const boost::filesystem::path
EntityId2Filename(const FilePersistor::EntityIdAndHandlerId& entityAndHandler,
                  const std::string & extension)
{
    std::ostringstream out;
    out << Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Dob::Typesystem::Operations::GetName(entityAndHandler.get<0>().GetTypeId()))
        << "@"
        << entityAndHandler.get<0>().GetInstanceId().GetRawValue()
        << "@"
        << entityAndHandler.get<1>().GetRawValue()
        << extension;
    return out.str();
}

//-------------------------------------------------------
boost::filesystem::path
FilePersistor::GetFilePath(const FilePersistor::EntityIdAndHandlerId& entityAndHandler) const
{
    return m_storagePath / EntityId2Filename(entityAndHandler, ".bin");
}


//-------------------------------------------------------
FilePersistor::FilePersistor(boost::asio::io_service& ioService) :
    PersistenceHandler(ioService),
    m_storagePath(GetStorageDirectory()),
    m_debug(L"FilePersistor")
{
    m_debug << "Persisting to '" << m_storagePath.string().c_str() << std::endl;
}

//-------------------------------------------------------
FilePersistor::~FilePersistor()
{

}


//-------------------------------------------------------
void
FilePersistor::Store(const Safir::Dob::Typesystem::EntityId entityId,
                     const Safir::Dob::Typesystem::HandlerId handlerId,
                     Safir::Dob::Typesystem::BinarySerialization & bin,
                     const bool /*update*/)
{
    const boost::filesystem::path path = GetFilePath(boost::make_tuple(entityId,handlerId, std::wstring()));

    std::ofstream file(path.string().c_str(),std::ios::out | std::ios::binary);
    if(file.good())
    {
        if (static_cast<std::streamsize>(bin.size()) <= boost::integer_traits<std::streamsize>::const_max)
        {
            file.write(&bin[0],static_cast<std::streamsize>(bin.size()));
        }
        else
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Failed to store object "
                                          + entityId.ToString()
                                          + L") since it's serialization was larger than "
                                          + boost::lexical_cast<std::wstring>(boost::integer_traits<std::streamsize>::const_max));
        }
    }
    else
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                      L"Failed to open file "
                                      + Safir::Dob::Typesystem::Utilities::ToWstring(path.string()) 
                                      + L" for writing, cannot persist entity " 
                                      + entityId.ToString());
    }
}

//-------------------------------------------------------
void
FilePersistor::Remove(const Safir::Dob::EntityProxy & entityProxy)
{
    const boost::filesystem::path path = GetFilePath(boost::make_tuple(entityProxy.GetEntityId(),entityProxy.GetOwner(), std::wstring()));

    RemoveFile(path);
}

//-------------------------------------------------------
void
FilePersistor::RemoveAll()
{
    for (boost::filesystem::directory_iterator it (m_storagePath);
        it != boost::filesystem::directory_iterator(); ++it)
    {
        const boost::filesystem::path path = it->path();
        try
        {
            boost::filesystem::remove(path);
        }
        catch (const boost::filesystem::filesystem_error &)
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Failed to remove persistence file "
                                          + Safir::Dob::Typesystem::Utilities::ToWstring(path.string())
                                          + L", maybe it is read-only?");
        }    
    }
}

//-------------------------------------------------------
Safir::Dob::EntityPtr
FilePersistor::RestoreBinary(const boost::filesystem::path & path) const
{
    Safir::Dob::Typesystem::BinarySerialization bin;
    const size_t fileSize = static_cast<size_t>(boost::filesystem::file_size(path));
    if (fileSize == 0)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                      L"Persistence file "
                                      + Safir::Dob::Typesystem::Utilities::ToWstring(path.string())
                                      + L" is empty, removing it.");
        RemoveFile(path);
        return Safir::Dob::EntityPtr(); //NULL
    }
#if 0
    bin.resize(fileSize);
    size_t numBytesRead = 0;
    boost::filesystem::ifstream file(path, std::ios::in | std::ios::binary);

    while (file.good())
    {
        file.read(&bin[0] + numBytesRead,4096);
        numBytesRead += static_cast<size_t>(file.gcount());
    }
    if(fileSize != numBytesRead)
    {
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Stupid error in file reading, probably.",__WFILE__,__LINE__);
    }
    file.close();

    return boost::dynamic_pointer_cast<Safir::Dob::Entity>(Safir::Dob::Typesystem::Serialization::ToObject(bin));

#elif 0
    bin.resize(fileSize);

    boost::filesystem::ifstream file(path, std::ios::in | std::ios::binary);
    file.read(&bin[0],fileSize);

    return boost::dynamic_pointer_cast<Safir::Dob::Entity>(Safir::Dob::Typesystem::Serialization::ToObject(bin));
#else
    using namespace boost::interprocess;

    // Create the file mapping
    file_mapping fm(path.string().c_str(), read_only);
    // Map the file in memory
    mapped_region region(fm, read_only);
    // Get the address where the file has been mapped
    const char* addr = static_cast<const char*>(region.get_address());
    return boost::dynamic_pointer_cast<Safir::Dob::Entity>(Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(addr));
#endif
}


//-------------------------------------------------------
Safir::Dob::EntityPtr
FilePersistor::RestoreXml(const boost::filesystem::path & path) const
{
    std::wstring xml;
    const size_t fileSize = static_cast<size_t>(boost::filesystem::file_size(path));
    if (fileSize == 0)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                      L"File "
                                      + Safir::Dob::Typesystem::Utilities::ToWstring(path.string())
                                      + L" is empty, removing it.");
        RemoveFile(path);
        return Safir::Dob::EntityPtr(); //NULL
    }

    xml.resize(fileSize);

    boost::filesystem::wifstream file (path, std::ios::in);
    file.read(&xml[0], fileSize);

    if(!file.good())
    {
        throw Safir::Dob::Typesystem::SoftwareViolationException(L"Stupid error in file reading, probably", __WFILE__, __LINE__);
    }
    file.close();

    //remove .xml file
    RemoveFile(path);

    Safir::Dob::EntityPtr entityPtr;
    try
    {
        entityPtr = boost::dynamic_pointer_cast<Safir::Dob::Entity>(Safir::Dob::Typesystem::Serialization::ToObject(xml));
    }
    catch(const Safir::Dob::Typesystem::IllegalValueException &)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                      L"Could not restore entity from file "
                                      + Safir::Dob::Typesystem::Utilities::ToWstring(path.string())
                                      + L", removing it and any corresponding bin file");
        
        RemoveFile(path);

        RemoveFile(replace_extension(path,".bin")); 
    }

    return entityPtr;
}

//-------------------------------------------------------
void
FilePersistor::RestoreAll()
{

    for (boost::filesystem::directory_iterator it (m_storagePath);
        it != boost::filesystem::directory_iterator(); ++it)
    {
        const boost::filesystem::path path = it->path();
        if (!boost::filesystem::exists(path))
        {
            // The file has been removed in an earlier step
            continue;
        }

        try
        {
            const EntityIdAndHandlerId tuple = Filename2EntityIdAndHandlerId(path);
            TypeIdSet::const_iterator findIt = GetPersistentTypes().find(tuple.get<0>().GetTypeId());
            if (findIt == GetPersistentTypes().end())
            { //not persistent any more, remove it

                m_debug << "File " << path.string().c_str() << " is not persistent in this configuration, removing" << std::endl;

                Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                              L"Type " 
                                              + tuple.get<2>()
                                              + L" is not persistent in this configuration, removing the corresponding xml and bin files");

                RemoveFile(replace_extension(path,".xml"));
                RemoveFile(replace_extension(path,".bin"));
            }
            else
            {
                Safir::Dob::ConnectionAspectInjector injector(m_dobConnection);
                Safir::Dob::EntityPtr entity;
                bool store = false;
                if (path.extension() == ".xml")
                {
                    if (!boost::filesystem::exists(replace_extension(path,".bin")))
                    {
                        m_debug << "This XML file is not an overlay, it is 'alone': " << path.string().c_str() << std::endl;
                        entity = RestoreXml(path);
                        store = true;
                    }
                }
                else if (boost::filesystem::exists(replace_extension(path,".xml")))
                {
                    m_debug << "There exists an overlay for " << path.string().c_str() << std::endl;
                    entity = RestoreXml(replace_extension(path,".xml"));
                    store = true;
                }
                else
                {
                    entity = RestoreBinary(path);
                }

                if (entity == NULL)
                {
                    continue;
                }

                if(store)
                { //if it was an xml file we want to store it as binary since the xml was removed
                    Safir::Dob::Typesystem::BinarySerialization bin;
                    Safir::Dob::Typesystem::Serialization::ToBinary( entity, bin);
                    Store(tuple.get<0>(), tuple.get<1>(), bin, true);
                }

                m_debug << "Restored object " << tuple.get<0>() << " with handlerId " << tuple.get<1>() << std::endl;

                injector.InitialSet(entity, tuple.get<0>().GetInstanceId(), tuple.get<1>() );
                m_debug << "InitialSet successful"<<std::endl;
            }
        }
        catch(const Safir::Dob::Typesystem::IllegalValueException &)
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Could not restore persistent entity from file "
                                          + Safir::Dob::Typesystem::Utilities::ToWstring(path.string())
                                          + L", removing it.");
            RemoveFile(path);
        }
        catch (const boost::filesystem::filesystem_error & e)
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Filesystem operation failed on file "
                                          + Safir::Dob::Typesystem::Utilities::ToWstring(path.string())
                                          + L". Exception: "
                                          + Safir::Dob::Typesystem::Utilities::ToWstring(e.what()));
        }
    }
}
