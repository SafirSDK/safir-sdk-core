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
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <iostream>
#include "ConfigReaderImpl.h"
#include "Path.h"
#include "PathFinders.h"
#include <boost/algorithm/string.hpp>


namespace
{
    std::vector<Safir::Utilities::Internal::Path> GetDouSearchPath(const Safir::Utilities::Internal::ConfigReader& reader)
    {
        std::string param = reader.Typesystem().get<std::string>("dou_search_path");
        std::vector<std::string> douSearchPath;
        boost::split(douSearchPath,
                     param,
                     boost::is_any_of(","));
        std::vector<Safir::Utilities::Internal::Path> checked;
        for (std::vector<std::string>::iterator it = douSearchPath.begin();
             it != douSearchPath.end(); ++it)
        {
            Safir::Utilities::Internal::Path p(*it);
            if (p.Exists())
            {
                checked.push_back(p);
            }
        }
        return checked;
    }
}
namespace Safir
{
namespace Utilities
{
namespace Internal
{

    ConfigReader::ConfigReader()
        : m_impl(new ConfigReaderImpl())
    {
#ifdef LLUF_CONFIG_READER_USE_WINDOWS
        typedef WindowsPathFinder PathFinder;
#else
        typedef LinuxPathFinder PathFinder;
#endif
        m_impl->Read<PathFinder>();
        m_impl->ExpandEnvironmentVariables();
    }

    const boost::property_tree::ptree& ConfigReader::Locations() const
    {
        return m_impl->m_locations;
    }

    const boost::property_tree::ptree& ConfigReader::Logging() const
    {
        return m_impl->m_logging;
    }

    const boost::property_tree::ptree& ConfigReader::Typesystem() const
    {
        return m_impl->m_typesystem;
    }

    std::string Expansion::ExpandEnvironment(const std::string& str)
    {
        return Safir::Utilities::Internal::ExpandEnvironment(str);
    }

    std::string Expansion::ExpandSpecial(const std::string& str)
    {
        return Safir::Utilities::Internal::ExpandSpecial(str);
    }




    std::vector<std::pair<std::string,std::string> > ConfigHelper::GetDouDirectories(const ConfigReader& reader)
    {
        std::vector<std::pair<std::string,std::string> > directories;

        const std::vector<Path> dou_search_path = GetDouSearchPath(reader);

        // Loop through all sections in typesystem.ini
        for (boost::property_tree::ptree::const_iterator it = reader.Typesystem().begin();
             it != reader.Typesystem().end();
             ++it)
        {
            const bool isSection = !it->second.empty();

            if (isSection)
            {
                try
                {
                    directories.push_back(std::make_pair(it->first, it->second.get<std::string>("dou_directory")));
                }
                catch (boost::property_tree::ptree_bad_path&)
                {
                    bool found = false;
                    for (std::vector<Path>::const_iterator sit = dou_search_path.begin();
                         sit != dou_search_path.end(); ++sit)
                    {
                        Path p = *sit / it->first;
                        if (p.Exists())
                        {
                            directories.push_back(std::make_pair(it->first, p.str()));
                            found = true;
                        }
                    }

                    if (!found)
                    {
                        throw std::logic_error("Dou directory not found for " + it->first);
                    }
                }
            }
        }
        return directories;
    }
}
}
}
