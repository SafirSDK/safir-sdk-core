/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://www.safirsdk.com)
* 
* Created by: Joel Ottosson / stjoot
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

#ifndef _dots_file_parser_h
#define _dots_file_parser_h
#include <expat.h>
#include "dots_internal_defs.h"
#include "dots_temporary_descriptions.h"
#include <boost/filesystem.hpp>
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Parses and validates xml-files.
     */
    class FileParser
    {
    public:
        FileParser();
        ~FileParser();
        bool ParseFiles();
        const DobClasses & ResultClasses() const;
        const DobProperties & ResultProperties() const;
        const DobEnumerations & ResultEnums() const;
        const DobExceptions& ResultExceptions() const;
        size_t ParameterSize();

        //DEBUG
        static void GetFileNameAndLineNumber(boost::filesystem::path & filename, int& line);

        void DumpClasses();
        void DumpProperties();

    private:
        /*
        bool ParseDouFiles();
        bool ParseDomFiles();
        bool ParseDouDir(const boost::filesystem::path & dirName);
        bool ParseDomDir(const boost::filesystem::path & dirName);        */
        bool ParseFile(const boost::filesystem::path & filename);

        bool UniqueTypes();

        bool FinalizeProperties();
        bool FinalizePropertyMappings();
        bool FinalizeClasses();

        //helper methods for FinalizeClasses
        bool SetBaseClasses();
        bool SetClassSizes();
        unsigned int SizeOfClass(int i);
        bool ResolveParameterReferences();

        int ParameterIndexInternal(const std::string& name);
        void ParseParameter(const std::string text, std::string& name, size_t& index);

        bool GetIndexOfType(TypeId tid, MemberType& mt, int& index);

        void DumpClass(DobClass& c);

        //the size of the parameters in the properties must be calculated here, since
        //we need the type sizes from the classes.
        size_t m_propertyParametersSize;

    };
}
}
}
}
#endif
