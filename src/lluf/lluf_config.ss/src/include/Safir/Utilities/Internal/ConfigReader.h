/******************************************************************************
*
* Copyright Saab AB, 2013-2015 (http://safirsdkcore.com)
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
#ifndef __SAFIR_UTILITIES_CONFIG_READER_H__
#define __SAFIR_UTILITIES_CONFIG_READER_H__

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef lluf_config_EXPORTS
#  define LLUF_CONFIG_API SAFIR_HELPER_DLL_EXPORT
#else
#  define LLUF_CONFIG_API SAFIR_HELPER_DLL_IMPORT
#endif
#define LLUF_CONFIG_LOCAL SAFIR_HELPER_DLL_LOCAL

#include <boost/property_tree/ptree.hpp>
#include <memory>
#include <vector>
#include <set>


namespace Safir
{
namespace Utilities
{
namespace Internal
{


    class ConfigReaderImpl;

    class LLUF_CONFIG_API ConfigReader
    {
    public:
        /**
         * Reads the Safir SDK Core configuration files and populates
         * the property trees for use.
         *
         * Also expands environment and special variables (see below) in the
         * property trees. There is no need to do that yourself!
         *
         * Throws various errors derived from std::exception on failure.
         * All these errors should be regarded as fatal errors!
         */
        ConfigReader();

        const boost::property_tree::ptree& Locations() const;
        const boost::property_tree::ptree& Logging() const;
        const boost::property_tree::ptree& Typesystem() const;


        /**
         * Get the directory where the currently loaded
         * configuration files are located.
         */
        std::string Path() const;

    private:

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251)
#endif

        std::shared_ptr<ConfigReaderImpl> m_impl;

#ifdef _MSC_VER
#pragma warning (pop)
#endif
    };


    class LLUF_CONFIG_API ConfigHelper
    {
    public:

        /**
         * Retreives all dou files directories specified in typesystem.ini.
         *
         * @param [in] reader ConfigReader object.
         * @return The dou file directories. The order reflects the order in the configuration file.
         *         First in pair is the module name and the second is the path.
         */
        static std::vector<std::pair<std::string,std::string> > GetDouDirectories(const ConfigReader& reader);


        /**
         * Retreives all dependencies of a dou module.
         *
         * The dependency resolution is recursive, so a dependency of a dependency will be included.
         *
         * @param [in] reader ConfigReader object.
         * @param [in] moduleName Name of the module to fetch dependecies for.
         * @return list of module names that the given module depends on.
         */
        static std::set<std::string> GetDouDependencies(const ConfigReader& reader,
                                                        const std::string& moduleName);


        /**
         * Returns the directory in which Safir SDK Tools should store their settings.
         *
         * @return Full path to tools settings directory.
         */
        static std::string GetToolsConfigDirectory();
    };
}
}
}

#endif
