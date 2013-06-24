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
#ifndef __LLUF_CONFIG_UTIL_H__
#define __LLUF_CONFIG_UTIL_H__

#include "Path.h"
#include <stdlib.h>
#include <stdexcept>

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
        return std::string(env);
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
#ifdef LLUF_CONFIG_READER_USE_WINDOWS
        if (var == "CSIDL_APPDATA" || var == "FOLDERID_RoamingAppData")
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
        else
#endif
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


}
}
}

#endif

