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
#include <Safir/Utilities/Internal/SystemLog.h>
#include <vector>
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


    //helper class for reading the directories containing dou and dom files
    //and for checking for illegal duplicates
    class FileCollection
    {
    public:
        static void Dirs(std::vector<boost::filesystem::path>& directories)
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
                        SEND_SYSTEM_LOG(Error, <<"Exceptions while examining dirs");
                        std::cout<<"Failed to read dou_directory in section " + it->first + " of typesystem.ini"<<std::endl;
                        exit(1);
                    }
                    
                    if (!boost::filesystem::exists(douDirectory) || !boost::filesystem::is_directory(douDirectory))
                    {
                        SEND_SYSTEM_LOG(Error, <<"Dir not found");
                        std::cout<<"dou_directory '" + douDirectory.string() + "' in section " + it->first + " of typesystem.ini does not appear to be a directory"<<std::endl;
                        exit(1);
                    }

                    directories.push_back(douDirectory);
                }
            }
        }
    };
}
}
}
}

#endif

