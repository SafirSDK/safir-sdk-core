/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
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
#ifndef __DOTS_INIT_HELPER_H__
#define __DOTS_INIT_HELPER_H__

#include <vector>
#include <boost/filesystem.hpp>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "dots_repository_keeper.h"

//check size of type definitions
BOOST_STATIC_ASSERT(sizeof(DotsC_Int32)==4);
BOOST_STATIC_ASSERT(sizeof(DotsC_Int64)==8);
BOOST_STATIC_ASSERT(sizeof(DotsC_Float32)==4);
BOOST_STATIC_ASSERT(sizeof(DotsC_Float64)==8);
BOOST_STATIC_ASSERT(sizeof(DotsC_EntityId)==sizeof(DotsC_TypeId) + sizeof(DotsC_Int64));
BOOST_STATIC_ASSERT(sizeof(DotsC_EntityId)==16);
BOOST_STATIC_ASSERT(sizeof(bool)==1);

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    class InitHelper
    {
    private:
        friend void Init();
        static boost::once_flag initFlag;
        static void Init()
        {
            std::vector<boost::filesystem::path> paths;
            size_t sharedMemorySize;
            ReadTypesystemIni(sharedMemorySize, paths);
            RepositoryKeeper::Initialize(sharedMemorySize, paths);
        }

        //Reads the typesystem.ini file.
        static void ReadTypesystemIni(size_t& sharedMemorySize, std::vector<boost::filesystem::path>& directories)
        {
            //Read the config files
            Safir::Utilities::Internal::ConfigReader reader;

            sharedMemorySize=0;
            try
            {
                sharedMemorySize=reader.Typesystem().get<size_t>("dots_shared_memory_size");
                sharedMemorySize*=(1024*1024); //megabytes to bytes
            }
            catch(...)
            {
                SEND_SYSTEM_LOG(Error, <<"Could not read dots_shared_memory_size from typesystem.ini");
                std::wcout<<"Could not read dots_shared_memory_size from typesystem.ini"<<std::endl;
                exit(1);
            }


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
                        std::cout<<"Failed to read dou_directory in section "+it->first+" of typesystem.ini"<<std::endl;
                        exit(1);
                    }

                    if (!boost::filesystem::exists(douDirectory) || !boost::filesystem::is_directory(douDirectory))
                    {
                        SEND_SYSTEM_LOG(Error, <<"Dir not found");
                        std::cout<<"dou_directory '"+douDirectory.string()+"' in section "+it->first+" of typesystem.ini does not appear to be a directory"<<std::endl;
                        exit(1);
                    }

                    directories.push_back(douDirectory);
                }
            }
        }
    };

    boost::once_flag InitHelper::initFlag=BOOST_ONCE_INIT;

    inline void Init()
    {
        boost::call_once(InitHelper::initFlag,InitHelper::Init);
    }
}
}
}
}

#endif

