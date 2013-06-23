/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
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
#include <boost/property_tree/ini_parser.hpp>
#include <iostream>
#include "Path.h"
#include "PathFinders.h"

    //gör i dots_kernel också...
    //dokumentera alltihopa wp och sug

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    class ConfigReader::Impl
    {
#ifdef LLUF_CONFIG_READER_USE_WINDOWS
        typedef WindowsPathsFinder PathFinder;
#else
        typedef LinuxPathsFinder PathFinder;
#endif

    public:
        Impl()
        {

        }

        void ExpandEnvironmentVariables()
        {
            ExpandEnvironmentVariables(m_locations);
            ExpandEnvironmentVariables(m_logging);
            ExpandEnvironmentVariables(m_typesystem);
        }

        void Read()
        {
            if (TryLoad(PathFinder::SystemConfigDirectory()))
            {
                return;
            }
            
            if (TryLoad(PathFinder::UserConfigDirectory()))
            {
                return;
            }

            if (TryLoad(PathFinder::SafirRuntimeConfigDirectory()))
            {
                return;
            }
            
            throw std::logic_error("Failed to load configuration");
        }

        boost::property_tree::ptree m_locations;
        boost::property_tree::ptree m_logging;
        boost::property_tree::ptree m_typesystem;

    private:
        /**
         * Returns false if it fails to find a configuration in the directory.
         * throws logic_error if the configuration cannot be loaded.
         */
        bool TryLoad(const Path& directory)
        {
            if (!directory.IsDirectory())
            {
                return false;
            }

            const Path locations = directory / "locations.ini";
            const Path logging = directory / "logging.ini";
            const Path typesystem = directory / "typesystem.ini";

            if (!locations.Exists() && !logging.Exists() && !typesystem.Exists())
            {
                return false;
            }
            else if (locations.IsFile() && logging.IsFile() && typesystem.IsFile())
            {
                boost::property_tree::read_ini(locations.str(), m_locations);
                boost::property_tree::read_ini(logging.str(), m_logging);
                boost::property_tree::read_ini(typesystem.str(), m_typesystem);
                return true;
            }
            else
            {
                throw std::logic_error("Failed to find all three ini files");
            }
        }

        static void ExpandEnvironmentVariables(boost::property_tree::ptree& ptree)
        {
            for (boost::property_tree::ptree::iterator it = ptree.begin();
                 it != ptree.end(); ++it)
            {
                const bool isSection = !it->second.empty();

                if (isSection)
                {
                    ExpandEnvironmentVariables(it->second);
                }
                else
                {
                    const std::string value = it->second.get_value<std::string>();
                    it->second.put_value(ExpandSpecial(value));
                    it->second.put_value(ExpandEnvironment(value));
                }
                

            }
        }


    };

    ConfigReader::ConfigReader()
        : m_impl(new Impl())
    {
        m_impl->Read();
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

    std::string ConfigReader::ExpandEnvironment(const std::string& str)
    {
        return Safir::Utilities::Internal::ExpandEnvironment(str);
    }
   
    std::string ConfigReader::ExpandSpecial(const std::string& str)
    {
        return Safir::Utilities::Internal::ExpandSpecial(str);
    }

}
}
}
