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

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning (disable: 4251)
#pragma warning (disable: 4512)
#pragma warning (disable: 4275)
#endif

#include <boost/program_options.hpp>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/exception.hpp>
#include <boost/filesystem/fstream.hpp>

#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/SwReports/SwReport.h>
#include <Safir/Databases/Odbc/Connection.h>
#include <Safir/Databases/Odbc/Environment.h>
#include <Safir/Databases/Odbc/InputParameter.h>
#include <Safir/Databases/Odbc/Statement.h>
#include <Safir/Databases/Odbc/Columns.h>
#include <Safir/Databases/Odbc/Exception.h>

#include <Safir/Dob/PersistenceParameters.h>

#include <iostream>
enum WhatToConvert {db, files};

WhatToConvert g_whatToConvert;

void ParseCommandLine(int argc, char * argv[])
{
    namespace po = boost::program_options;
    // Declare the supported options.
    po::options_description options("Allowed options");
    options.add_options()
        ("help", "produce help message")
        ("db", "Convert the db")
        ("files", "Convert the files")
        ;

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, options), vm);
    po::notify(vm);

    if (vm.count("help"))
    {
        std::cout << "Converts persistent data in binary format to XML\n\n"
                     "If no option is given the choice between db or files\n"
                     "is determined by Safir.Dob.PersistenceParameters.Backend.\n\n"
                  << options << "\n";
        exit(1);
    }

    if (vm.count("db") && vm.count("files"))
    {
        std::cerr << "You can only convert db or files, not both at the same time" << std::endl;
        exit(1);
    }
    else if (vm.count("db"))
    {
        g_whatToConvert = db;
    }
    else if (vm.count("files"))
    {
        g_whatToConvert = files;
    }
    else
    {
        // No command line parameter given. We read the persistence
        // configuration parameter instead. 
        switch (Safir::Dob::PersistenceParameters::Backend())
        {
            case Safir::Dob::PersistenceBackend::File:
            {
                g_whatToConvert = files;
            }
            break;

            case Safir::Dob::PersistenceBackend::Odbc:
            {
                g_whatToConvert = db;
            }
            break;

            default:
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Unknown backend!",__WFILE__,__LINE__);
        }
    }
}


void ConvertDb()
{
    Safir::Databases::Odbc::Connection  readConnection;
    Safir::Databases::Odbc::Connection  updateConnection;
    Safir::Databases::Odbc::Environment environment;
    environment.Alloc();
    readConnection.Alloc(environment);
    updateConnection.Alloc(environment);

    Safir::Databases::Odbc::Statement getAllStatement;
    Safir::Databases::Odbc::Int64Column typeIdColumn;
    Safir::Databases::Odbc::Int64Column instanceColumn;
    Safir::Databases::Odbc::Int64Column handlerColumn;
    Safir::Databases::Odbc::WideStringColumn xmlDataColumn(Safir::Dob::PersistenceParameters::XmlDataColumnSize());
    Safir::Databases::Odbc::BinaryColumn binaryDataColumn(Safir::Dob::PersistenceParameters::BinaryDataColumnSize());
    Safir::Databases::Odbc::BinaryColumn binarySmallDataColumn(Safir::Dob::PersistenceParameters::BinarySmallDataColumnSize());

    Safir::Databases::Odbc::Statement updateStatement;
    Safir::Databases::Odbc::Int64Parameter updateTypeIdParam;
    Safir::Databases::Odbc::Int64Parameter updateInstanceParam;
    Safir::Databases::Odbc::LongWideStringParameter updateXmlDataParam(Safir::Dob::PersistenceParameters::XmlDataColumnSize());

    readConnection.Connect(Safir::Dob::PersistenceParameters::OdbcStorageConnectString());
    updateConnection.Connect(Safir::Dob::PersistenceParameters::OdbcStorageConnectString());

    updateStatement.Alloc(updateConnection);
    updateStatement.Prepare(L"UPDATE PersistentEntity SET xmlData=?, binarySmallData=NULL, binaryData=NULL WHERE typeId=? AND instance=?");
    updateStatement.BindLongParameter( 1, updateXmlDataParam );
    updateStatement.BindParameter( 2, updateTypeIdParam );
    updateStatement.BindParameter( 3, updateInstanceParam );

    getAllStatement.Alloc(readConnection);
    getAllStatement.Prepare( L"SELECT typeId, instance, binarySmallData, binaryData from PersistentEntity where binaryData is not null or binarySmallData is not null");
    getAllStatement.BindColumn( 1, typeIdColumn );
    getAllStatement.BindColumn( 2, instanceColumn);
    getAllStatement.BindColumn( 3, binarySmallDataColumn);
    getAllStatement.Execute();
    for (;;)
    {
        if (!getAllStatement.Fetch())
        {//we've got all rows!
            break;
        }

        Safir::Dob::Typesystem::EntityId entityId
            (typeIdColumn.GetValue(), Safir::Dob::Typesystem::InstanceId(instanceColumn.GetValue()));

        Safir::Dob::Typesystem::ObjectPtr object;

        if (!binarySmallDataColumn.IsNull())
        {
            const char * const data = reinterpret_cast<const char * const>(binarySmallDataColumn.GetValue());
            object = Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(data);
        }
        else
        {
            getAllStatement.GetData(4, binaryDataColumn);
            if (!binaryDataColumn.IsNull())
            { //some binarypersistent data set
                const char * const data = reinterpret_cast<const char * const>(binaryDataColumn.GetValue());
                object = Safir::Dob::Typesystem::ObjectFactory::Instance().CreateObject(data);
            }
        }
        if (object != NULL)
        {
            std::wstring xml = Safir::Dob::Typesystem::Serialization::ToXml(object);
            
            updateTypeIdParam.SetValue(entityId.GetTypeId());
            updateInstanceParam.SetValue(entityId.GetInstanceId().GetRawValue());
            updateXmlDataParam.SetValueAtExecution(static_cast<int>(xml.size() * sizeof (wchar_t)));
            
            updateStatement.Execute();
            unsigned short param = 0;
            if (!updateStatement.ParamData(param))
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"There should be one call to ParamData!",__WFILE__,__LINE__);
            }
            updateXmlDataParam.SetValue(&xml);
            updateStatement.PutData(updateXmlDataParam);
            if (updateStatement.ParamData(param))
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"There should only be one call to ParamData!",__WFILE__,__LINE__);
            }
            updateConnection.Commit();
        }
        else
        {
            //                          m_debug << "No data set for " << objectId <<std::endl;
        }
    }
}


//-------------------------------------------------------
boost::filesystem::path GetStorageDirectory()
{
    try
    {
        boost::filesystem::path path = boost::filesystem::path(Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Dob::PersistenceParameters::FileStoragePath()),boost::filesystem::native);

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
            boost::filesystem::create_directory(path);
        }

        return path;
    }
    catch (const boost::filesystem::filesystem_error & e)
    {
        throw Safir::Dob::Typesystem::SoftwareViolationException
            (L"Failed to get hold of the directory for file persistance. Got this info from boost::filesystem::filesystem_error" +
            Safir::Dob::Typesystem::Utilities::ToWstring(e.what()),__WFILE__,__LINE__);
    }
}

typedef std::pair<Safir::Dob::Typesystem::EntityId, Safir::Dob::Typesystem::HandlerId> EntityIdAndHandlerId;

//-------------------------------------------------------
const EntityIdAndHandlerId
Filename2EntityIdAndHandlerId(const boost::filesystem::path & filename)
{
    if (!filename.has_leaf())
    {
        throw Safir::Dob::Typesystem::IllegalValueException
            (L"Filename2EntityAndHandler: Could not decompose filename : " +
            Safir::Dob::Typesystem::Utilities::ToWstring(filename.string()),__WFILE__,__LINE__);
    }
    const std::string leaf = filename.leaf();
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

    return std::make_pair(Safir::Dob::Typesystem::EntityId
        (Safir::Dob::Typesystem::Operations::GetTypeId(Safir::Dob::Typesystem::Utilities::ToWstring(typeName)),
        Safir::Dob::Typesystem::InstanceId( boost::lexical_cast<Safir::Dob::Typesystem::Int64>(instance))),
        Safir::Dob::Typesystem::HandlerId(boost::lexical_cast<Safir::Dob::Typesystem::Int64>(handler)));
}

//-------------------------------------------------------
boost::filesystem::path
EntityId2Filename(const EntityIdAndHandlerId& entityAndHandler,
                  const std::string & extension)
{
    std::ostringstream out;
    out << Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Dob::Typesystem::Operations::GetName(entityAndHandler.first.GetTypeId()))
        << "@"
        << entityAndHandler.first.GetInstanceId().GetRawValue()
        << "@"
        << entityAndHandler.second.GetRawValue()
        << extension;
    return out.str();
}


void ConvertFiles()
{
    Safir::Dob::Typesystem::BinarySerialization bin;
    boost::filesystem::path storagePath = GetStorageDirectory();

    for (boost::filesystem::directory_iterator it (storagePath);
         it != boost::filesystem::directory_iterator(); ++it)
    {
        const EntityIdAndHandlerId id = Filename2EntityIdAndHandlerId(*it);

        if (boost::filesystem::extension(*it) == ".bin")
        {
            const size_t fileSize = static_cast<size_t>(boost::filesystem::file_size(*it));
            if (fileSize == 0)
            {
                continue;
            }
            bin.resize(fileSize);

            size_t numBytesRead = 0;
            boost::filesystem::ifstream file(*it, std::ios::in | std::ios::binary);
            while (file.good())
            {
                file.read(&bin[0] + numBytesRead,4096);
                numBytesRead += file.gcount();
            }

            if(fileSize != numBytesRead)

            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Stupid error in file reading, probably", __WFILE__, __LINE__);
            }
            file.close();

            const Safir::Dob::Typesystem::ObjectPtr object = Safir::Dob::Typesystem::Serialization::ToObject(bin);

            const std::wstring xml = Safir::Dob::Typesystem::Serialization::ToXml(object);
            boost::filesystem::wofstream xmlFile(boost::filesystem::change_extension(*it,".xml"));
            xmlFile << xml;
        }
    }
}

int main(int argc, char * argv[])
{
    try
    {
        ParseCommandLine(argc,argv);
        if (g_whatToConvert == db)
        {
            ConvertDb();
        }
        else if (g_whatToConvert == files)
        {
            ConvertFiles();
        }
    }
    catch (const std::exception & exc)
    {
        std::wcout << "Caught exception: " << exc.what() << std::endl;
    }
    return 0;
}

