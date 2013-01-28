/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / joot
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
#ifndef __DOTS_TYPE_PARSER_ERROR_H__
#define __DOTS_TYPE_PARSER_ERROR_H__

#if defined _MSC_VER
    #if defined DOTS_PARSER_EXPORTS
        #define DOTS_API __declspec(dllexport)
    #else
        #define DOTS_API __declspec(dllimport)
        #define SAFIR_LIBRARY_NAME "dots_parser"
        #include <Safir/Utilities/Internal/AutoLink.h>
    #endif
#elif defined __GNUC__
    #define DOTS_API
    #define __cdecl
#endif

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Exception used to report errors in dou- and dom- files.
     */
    class DOTS_API ParseError : public std::exception
    {
    public:
        ParseError(const std::string& label, const std::string& description, const std::string& file) :
            m_label(label),
            m_description(description),
            m_file(file) {}

        ~ParseError() throw() {}

        const std::string& Label() const throw() {return m_label;}
        const std::string& Description() const throw() {return m_description;}
        const std::string& File() const throw() {return m_file;}

        virtual const char* what () const throw (){return m_description.c_str();}

    private:
        std::string m_label;
        std::string m_description;
        std::string m_file;
    };
}
}
}
} //end namespace Safir::Dob::Typesystem::Internal

#endif
