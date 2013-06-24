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
#ifndef __SAFIR_UTILITIES_CONFIG_READER_H__
#define __SAFIR_UTILITIES_CONFIG_READER_H__

#include <boost/shared_ptr.hpp>
#include <boost/property_tree/ptree.hpp>

#if defined _MSC_VER
    #ifdef lluf_config_EXPORTS
        #define LLUF_CONFIG_API __declspec(dllexport)
    #else
        #define LLUF_CONFIG_API __declspec(dllimport)
        #ifndef NDEBUG
            #pragma comment( lib, "lluf_configd.lib" )
        #else
            #pragma comment( lib, "lluf_config.lib" )
        #endif
    #endif
#elif defined __GNUC__
  #define LLUF_CONFIG_API
#endif



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
         */
        ConfigReader();

        const boost::property_tree::ptree& Locations() const;
        const boost::property_tree::ptree& Logging() const;
        const boost::property_tree::ptree& Typesystem() const;


        /**
         * Expand environment variables in the string. 
         *
         * Looks for $(VARIABLE) and replaces it with the value of getenv("VARIABLE").
         * Throws std::logic_error if the variable cannot be found.
         *
         * The function will handle recursive lookup, e.g. $(NAME_$(NUMBER)) will
         * be expanded to the value of NAME_0 if NUMBER is 0.
         */
        static std::string ExpandEnvironment(const std::string& str);

        /**
         * Expand special variables int the string.
         *
         * E.g. looks for @{CSIDL_COMMON_APPDATA} and replaces it with the value of that special
         * varible (in this case usually C:\ProgramData on Windows Vista and later).
         *
         * Currently these are only a number of special folders on 
         * Windows, please look at the source code or the Safir SDK Users Guide
         * for a list of variables that are expanded (search for Special Folders).
         */
        static std::string ExpandSpecial(const std::string& str);
    private:

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251)
#endif

        boost::shared_ptr<ConfigReaderImpl> m_impl;  

#ifdef _MSC_VER
#pragma warning (pop)
#endif          
    };
}
}
}

#endif

