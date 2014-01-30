/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#ifndef __DOTS_INTERNAL_PARSE_ERROR_H__
#define __DOTS_INTERNAL_PARSE_ERROR_H__

#include <sstream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4275)
#pragma warning (disable: 4251)
#endif

    /**
     * @brief Exception used to report errors in dou- and dom- files.
     */
    class ParseError : public std::exception
    {
    public:

        /**
         * Constructor - Creates a ParseError object.
         *
         * @param label [in] - Short description of the error.
         * @param description [in] - Internaled description of the error.
         * @param file [in] - Name of the dou- or dom-file that caused the error.
         * @param errorId [in] - Id of this specific error. Can be used by test code and developvers to locate from where this error was thrown.
         */
        ParseError(const std::string& label, const std::string& description, const std::string& file, int errorId)
            :m_label(label)
            ,m_description(description)
            ,m_file(file)
            ,m_what()
            ,m_errorId(errorId)
        {
        }

        /**
         * Destructor.
         */
        ~ParseError() throw() {}

        /**
         * Get short error description.
         *
         * @return Error label.
         */
        const std::string& Label() const throw() {return m_label;}

        /**
         * Get Internaled error description.
         *
         * @return Error description.
         */
        const std::string& Description() const throw() {return m_description;}

        /**
         * Get file where error occured.
         *
         * @return Complete path to a dou- or dom- file.
         */
        const std::string& File() const throw() {return m_file;}

        /**
         * Get an error identifier that can be used to find out exactly where this error was generated.
         * Mostly intended for test and development.
         *
         * @return Error id.
         */
        int ErrorId() const {return m_errorId;}

        /**
         * Get error informtation on the form "Label; Description; File; ErrorId".
         * Inherited from std:exception.
         *
         * @return Error information.
         */
        virtual const char* what () const throw ()
        {
            if (m_what.empty())
            {
                std::ostringstream os;
                os<<m_label<<"; "<<m_description<<"; "<<m_file<<"; ErrorCode="<<m_errorId;
                m_what=os.str();  //composed error info
            }
            return m_what.c_str();
        }

    private:
        std::string m_label;
        std::string m_description;
        std::string m_file;
        mutable std::string m_what;
        int m_errorId;
    };

#ifdef _MSC_VER
#pragma warning (pop)
#endif

}
}
}
} //end namespace Safir::Dob::Typesystem::Internal

#endif
