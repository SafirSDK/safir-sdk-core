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

class RequestErrorException : public std::exception
{
public:
    static const int ParseError         = -32700;
    static const int InvalidRequest     = -32600;
    static const int MethodNotFound     = -32601;
    static const int InvalidParams      = -32602;
    static const int InternalError      = -32602;
    static const int ServerError        = -32000;
    static const int SafirException     = -1;

    RequestErrorException(const std::string& msg, int code)
        :m_message(msg)
        ,m_code(code)
    {
    }

    const std::string& Message() const {return m_message;}
    int Code() const {return m_code;}

    virtual const char* what () const throw () {return m_message.c_str();}

private:
    std::string m_message;
    int m_code;
};

