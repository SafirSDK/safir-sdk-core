/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
* 
* Created by: Lars Hagström / stlrha
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

#include "dots_exception_parser.h"


namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    ExceptionParser::ExceptionParser()
    {
        //insert predefined exceptions base classes
        m_exceptions.insert(DobException("FundamentalException",""));
        m_exceptions.insert(DobException("Exception",""));
        
        //insert predefined derived exceptions
        m_exceptions.insert(DobException("Safir.Dob.Typesystem.IllegalValueException","FundamentalException"));
        m_exceptions.insert(DobException("Safir.Dob.Typesystem.IncompatibleTypesException","FundamentalException"));
        m_exceptions.insert(DobException("Safir.Dob.Typesystem.ConfigurationErrorException","FundamentalException"));
        m_exceptions.insert(DobException("Safir.Dob.Typesystem.SoftwareViolationException","FundamentalException"));
        m_exceptions.insert(DobException("Safir.Dob.Typesystem.NullException","FundamentalException"));
        m_exceptions.insert(DobException("Safir.Dob.Typesystem.ReadOnlyException","FundamentalException"));
    }

    bool ExceptionParser::ValidElement(const std::string & element)
    {
        if (m_parseStack.empty())
        {
            return element == XmlElements::EXCEPTION;
        }
        else if (m_parseStack.back() == XmlElements::EXCEPTION)
        {
            return element == XmlElements::SUMMARY ||
                element == XmlElements::NAME ||
                element == XmlElements::BASE_CLASS;
        }
        else
        {
            return false;
        }

    }

    void ExceptionParser::Reset()
    {
        m_currentName.clear();
        m_currentBaseClass.clear();
    }


    bool ExceptionParser::StartElement(const std::string& element)
    {
        if (!ValidElement(element))
        {
            IllegalElement(element.c_str());
            return false;
        }
        m_parseStack.push_back(element);

        return true;
    }

    bool ExceptionParser::Content(const std::string& str)
    {
        if (m_parseStack.back() == XmlElements::NAME)
        {
            if (str.empty())
            {
                return false;
            }
            m_currentName = str;
        } 
        else if (m_parseStack.back() == XmlElements::BASE_CLASS)
        {
            if (str.empty())
            {
                return false;
            }
            m_currentBaseClass = str;
        }
        return true;
    }


    bool ExceptionParser::EndElement(const std::string& element)
    {
        if (m_parseStack.back() != element)
        {
            ElementMismatch(element.c_str());
            return false;
        }
        m_parseStack.pop_back();

        if (element == XmlElements::EXCEPTION)
        {
            m_exceptions.insert(DobException(m_currentName,m_currentBaseClass));
        }

        return true;
    }
 
}
}
}
}
