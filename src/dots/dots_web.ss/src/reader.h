/******************************************************************************
*
* Copyright Saab AB, 2005-2010 (http://www.safirsdk.com)
* 
* Created by: Mikael Wennerberg / stmiwn
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
#ifndef _dots_web_reader_h
#define _dots_web_reader_h
#include <Safir/Dob/Typesystem/Operations.h>

namespace DotsWeb
{

    class Reader
    {

    public:
        Reader(boost::filesystem::path templateFile);
        ~Reader();

        int Run();


    private:
        void Write(FILE* outStream, Safir::Dob::Typesystem::TypeIdVector vect);

        boost::filesystem::path m_inFile;
        boost::filesystem::path m_outFile;

    };
}

#endif // _dots_web_reader_h
