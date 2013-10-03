/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#ifndef __DOTS_FILE_COLLECTION_H__
#define __DOTS_FILE_COLLECTION_H__

#include <Safir/Utilities/Internal/ConfigReader.h>
#include "dots_error_handler.h"
#include "dots_internal_defs.h"

#include <map>
#include <boost/filesystem.hpp>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    //first is file name, second is full path (including file name)
    typedef std::map<boost::filesystem::path, boost::filesystem::path> FileLocations; 

    //helper class for reading the directories containing dou and dom files
    //and for checking for illegal duplicates
    class FileCollection
    {
    public:
        FileCollection()
        {
            using namespace Safir::Dob::Typesystem::Internal;

            //Read the config files
            Safir::Utilities::Internal::ConfigReader reader;

            //loop through all sections in typesystem.ini
            for (boost::property_tree::ptree::const_iterator it = reader.Typesystem().begin();
                 it != reader.Typesystem().end(); ++it)
            {
                const bool isSection = !it->second.empty();

                if (isSection)
                {
                    boost::filesystem::path douDirectory;
                    try
                    {
                        douDirectory = it->second.get<std::string>("dou_directory");
                    }
                    catch (const std::exception&)
                    {
                        ErrorHandler::Error("Dou/Dom file parsing",
                                            "Failed to read dou_directory in section " + it->first + " of typesystem.ini",
                                            "dots_file_parser");
                        exit(1);
                    }
                    
                    if (!boost::filesystem::exists(douDirectory) || !boost::filesystem::is_directory(douDirectory))
                    {
                        ErrorHandler::Error("Dou/Dom file parsing",
                                            "dou_directory '" + douDirectory.string() + "' in section " + it->first + " of typesystem.ini does not appear to be a directory",
                                            "dots_file_parser");
                        exit(1);
                    }

                    FileLocations douFiles;
                    FileLocations domFiles;
                    ReadDirectory(douDirectory,douFiles,domFiles);
                    
                    InsertAndOverride(m_douFiles, douFiles);
                    InsertAndOverride(m_domFiles, domFiles);
                }

            }

        }

        const FileLocations& DouFiles() const {return m_douFiles;}
        const FileLocations& DomFiles() const {return m_domFiles;}
    private:
        //gets dou and dom files from a directory
        //throws xxx on duplicate file name.
        static void ReadDirectory(const boost::filesystem::path& directory, 
                                  FileLocations& douFiles,
                                  FileLocations& domFiles)
        {
            douFiles.clear();
            domFiles.clear();
            using namespace Safir::Dob::Typesystem::Internal;

            const boost::filesystem::recursive_directory_iterator end;
            for (boost::filesystem::recursive_directory_iterator dir(directory);
                 dir != end; ++dir)
            {
                const boost::filesystem::path path(*dir);
                const boost::filesystem::path extension = path.extension();
                const boost::filesystem::path filename = path.filename();
                
                if (extension == DOU_FILE_EXTENSION)
                {
                    const bool result = douFiles.insert(std::make_pair(filename,path)).second;
                    
                    if (!result)
                    {
                        SEND_SYSTEM_LOG(Critical,
                                        << "Duplicate dou file found: " << path.string().c_str());

                        throw std::logic_error("Dupicate class");
                    }
                }
                else if (extension == DOM_FILE_EXTENSION)
                {
                    const bool result = domFiles.insert(std::make_pair(filename,path)).second;
                    
                    if (!result)
                    {
                        SEND_SYSTEM_LOG(Critical,
                                        << "Duplicate dom file found: " << path.string().c_str());

                        throw std::logic_error("Dupicate property mapping");
                    }
                }

            }
        }

        static void InsertAndOverride(FileLocations& into, const FileLocations& from)
        {
            for (FileLocations::const_iterator it = from.begin();
                 it != from.end(); ++it)
            {
                const std::pair<FileLocations::iterator, bool> result = 
                    into.insert(*it);
                if (!result.second)
                {
                    lllout << "Found override dou file " << it->second.string().c_str() << std::endl;
                    //replace the old path with the new path in _into_
                    result.first->second = it->second;
                }
            }
        }


        FileLocations m_douFiles;
        FileLocations m_domFiles;
    };
}
}
}
}

#endif

