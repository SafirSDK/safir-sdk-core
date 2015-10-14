/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
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
#include "Util.h"

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    class ConfigReaderImpl
    {
    public:
        ConfigReaderImpl()
        {

        }

        void ExpandEnvironmentVariables()
        {
            ExpandEnvironmentVariables(m_locations);
            ExpandEnvironmentVariables(m_logging);
            ExpandEnvironmentVariables(m_typesystem);
        }

        template <class PathFinder>
        void Read()
        {
            const Path dir1 = PathFinder::SafirTestConfigOverrideDirectory();
            if (!dir1.empty())
            {
                if (TryLoad(dir1))
                {
                    return;
                }
                throw std::logic_error("Failed to load configuration. "
                                       "Looked in '"
                                       + dir1.str() + "'");
            }
            else
            {
                const Path dir2 = PathFinder::SystemConfigDirectory();
                if (TryLoad(dir2))
                {
                    return;
                }

                const Path dir3 = PathFinder::UserConfigDirectory();
                if (TryLoad(dir3))
                {
                    return;
                }
                throw std::logic_error("Failed to load configuration. "
                                       "Looked in '"
                                       + dir2.str() + "' and '"
                                       + dir3.str() + "'");
            }
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
            if (directory.empty())
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
                throw std::logic_error("Failed to find all three ini files in '" + directory.str() + "'");
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
                    std::string value = it->second.get_value<std::string>();
                    value = ExpandSpecial(value);
                    it->second.put_value(ExpandEnvironment(value));
                }


            }
        }


    };

}
}
}
