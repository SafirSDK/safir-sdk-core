/******************************************************************************
*
* Copyright Saab AB, 2016, 2023 (http://safirsdkcore.com)
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

namespace JsonRpcErrorCodes
{
    //JSON-RPC predefined error codes
    static const int ParseError         = -32700;
    static const int InvalidRequest     = -32600;
    static const int MethodNotFound     = -32601;
    static const int InvalidParams      = -32602;
    static const int InternalError      = -32603;
    static const int ServerError        = -32000;

    //Safir defined error codes
    static const int SafirNotOpen               = 100;
    static const int SafirOverflow              = 101;
    static const int SafirAccessDenied          = 102;
    static const int SafirGhostExists           = 103;
    static const int SafirNotFound              = 104;
    static const int SafirIllegalValue          = 105;
    static const int SafirSoftwareViolation     = 106;
    static const int SafirReadOnly              = 107;
    static const int SafirUnexpectedException   = 108;
    static const int SafirLowMemoryException    = 109;

    static std::string CodeToString(int code)
    {
        switch (code)
        {
        case ParseError:
            return "Parse error";
        case InvalidRequest:
            return "Invalid request";
        case MethodNotFound:
            return "Method not found";
        case InvalidParams:
            return "Invalid params";
        case InternalError:
            return "Internal error";
        case ServerError:
            return "Server error";
        case SafirNotOpen:
            return "Not open";
        case SafirOverflow:
            return "Overflow";
        case SafirAccessDenied:
            return "Access denied";
        case SafirGhostExists:
            return "Ghost exists";
        case SafirNotFound:
            return "Not found";
        case SafirIllegalValue:
            return "Illegal value";
        case SafirSoftwareViolation:
            return "Software violation";
        case SafirReadOnly:
            return "Read only";
        case SafirUnexpectedException:
            return "Unexpected Safir exception";
        case SafirLowMemoryException:
            return "Low memory";
        default:
            return "Unexpected error";
        }
    }
}

class RequestErrorException : public std::exception
{
public:

    RequestErrorException(int code)
        :m_code(code)
        ,m_message(JsonRpcErrorCodes::CodeToString(code))
        ,m_data("")
    {
    }

    RequestErrorException(int code, const std::string& data)
        :m_code(code)
        ,m_message(JsonRpcErrorCodes::CodeToString(code))
        ,m_data(data)
    {
    }

    RequestErrorException(int code, const std::string& message, const std::string& data)
        :m_code(code)
        ,m_message(message)
        ,m_data(data)
    {
    }

    const char* what () const throw() override {return m_message.c_str();}

    int Code() const {return m_code;}
    const std::string& Message() const {return m_message;}
    const std::string& Data() const {return m_data;}

private:
    int m_code;
    std::string m_message;
    std::string m_data;
};
