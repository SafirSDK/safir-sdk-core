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
#ifndef __LLUF_CONFIG_UTIL_H__
#define __LLUF_CONFIG_UTIL_H__

#include "Path.h"
#include <stdlib.h>
#include <stdexcept>
#include <boost/lexical_cast.hpp>

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    std::string GetEnv(const std::string& name)
    {
        char* env = getenv(name.c_str());
        if (env == NULL)
        {
            throw std::logic_error("Environment variable " + name + " does not appear to be set");
        }
        return env;
    }

    /** Nothrow version of getenv. Returns empty string on failure.*/
    std::string GetEnv(const std::string& name, std::nothrow_t)
    {
        char* env = getenv(name.c_str());
        if (env == NULL)
        {
            return "";
        }
        return env;
    }

#ifdef LLUF_CONFIG_READER_USE_WINDOWS
    Path GetFolderPathFromCSIDL(const int csidl)
    {
        char path[MAX_PATH];

        if(SUCCEEDED(SHGetFolderPathA(NULL, 
                                     csidl|CSIDL_FLAG_CREATE, 
                                     NULL, 
                                     0, 
                                     path))) 
        {
            return Path(path);
        }
        else
        {
            throw std::logic_error("Call to SHGetFolderPath failed!");
        }
    }
#endif



    std::string ExpandSpecial(const std::string& str)
    {
        const size_t start=str.rfind("@{");
        const size_t stop=str.find('}', start);

        if (start==std::string::npos || stop==std::string::npos)
            return str;

        const std::string var=str.substr(start+2, stop-start-2);

        Path value;
        if (var == "TEMP")
        {
#ifdef LLUF_CONFIG_READER_USE_WINDOWS
            std::string env = GetEnv("TEMP", std::nothrow);
            if (env.empty())
            {
                env = GetEnv("TMP", std::nothrow);
            }
            if (env.empty())
            {
                throw std::logic_error("Special variable TEMP could not be expanded, since neither TEMP or TMP environment variables could be found.");
            }
            value = Path(env);
#else
            value = Path("/tmp");
#endif
        }
#ifdef LLUF_CONFIG_READER_USE_WINDOWS
        else if (var == "CSIDL_APPDATA" || var == "FOLDERID_RoamingAppData")
        {
            value = GetFolderPathFromCSIDL(CSIDL_APPDATA);
        }
        else if (var == "CSIDL_LOCAL_APPDATA" || var == "FOLDERID_LocalAppData")
        {
            value = GetFolderPathFromCSIDL(CSIDL_LOCAL_APPDATA);
        }
        else if (var == "CSIDL_COMMON_APPDATA" || var == "FOLDERID_ProgramData")
        {
            value = GetFolderPathFromCSIDL(CSIDL_COMMON_APPDATA);
        }
        else if (var == "CSIDL_MYDOCUMENTS" || var == "FOLDERID_Documents")
        {
            value = GetFolderPathFromCSIDL(CSIDL_MYDOCUMENTS);
        }
        else if (var == "CSIDL_COMMON_DOCUMENTS" || var == "FOLDERID_PublicDocuments")
        {
            value = GetFolderPathFromCSIDL(CSIDL_COMMON_DOCUMENTS);
        }
#endif
        else
        {
            throw std::logic_error("Special variable " + var + " could not be found");
        }

        const std::string res=str.substr(0, start) + value.str() + str.substr(stop+1, str.size()-stop-1);
        //search for next special variable 
        return ExpandSpecial(res); 
    }



    std::string ExpandEnvironment(const std::string& str)
    {
        const size_t start=str.rfind("$(");
        const size_t stop=str.find(')', start);

        if (start==std::string::npos || stop==std::string::npos)
            return str;

        const std::string var=str.substr(start+2, stop-start-2);
        const std::string env = GetEnv(var);

        const std::string res=str.substr(0, start) + env + str.substr(stop+1, str.size()-stop-1);
        //search for next environment variable or
        //recursively expand nested variable, e.g. $(NAME_$(NUMBER))
        return ExpandEnvironment(res); 
    }


    std::string GetSafirInstanceSuffix()
    {
        try
        {
            const std::string env = GetEnv("SAFIR_INSTANCE", std::nothrow);
            
            if (env.empty())
            {
                return "_0";
            }
            else
            {
                const unsigned int inst = boost::lexical_cast<unsigned int>(env);
                return std::string("_") + boost::lexical_cast<std::string>(inst);
            }
        }
        catch(const boost::bad_lexical_cast&)
        {
            throw std::logic_error("SAFIR_INSTANCE is not set to a number");
        }
    }


}
}
}

#endif

