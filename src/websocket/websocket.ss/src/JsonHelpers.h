/******************************************************************************
*
* Copyright Saab AB, 2016 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#pragma once

#include <string>
#include <vector>
#include <boost/algorithm/string.hpp>

namespace JsonHelpers
{
    inline bool IsArray(const std::string& json)
    {
        return json[0]=='[' && json[json.length()-1]==']';
    }

    inline bool IsObject(const std::string& json)
    {
        return json[0]=='{' && json[json.length()-1]=='}';
    }

    inline std::vector<std::string> SplitArrayOfObjects(const std::string& json)
    {
        std::vector<std::string> result;

        size_t uncloseBracketCount=0;
        size_t startIndex=0;
        for (size_t i=0; i<json.length(); i++)
        {
            if (json[i]=='{')
            {
                if (uncloseBracketCount==0)
                {
                    startIndex=i;
                }
                ++uncloseBracketCount;
            }
            else if (json[i]=='}')
            {
                --uncloseBracketCount;
                if (uncloseBracketCount==0)
                {
                    result.emplace_back(json.substr(startIndex, i-startIndex+1));
                }
            }
        }

        return result;
    }
}
