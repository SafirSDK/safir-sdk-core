/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/Response.h>
#include <Safir/Dob/Parametrization.h>
#include <Safir/Dob/Item.h>
#include <Safir/Dob/Struct.h>
#include <Safir/Dob/Typesystem/Properties.h>
#include <Safir/Dob/Typesystem/Exceptions.h>

#include <iostream>
#include <vector>
#include <string.h>
#include <stdio.h>
#include <algorithm>

#include "reader.h"

using namespace Safir::Dob::Typesystem;

namespace DotsWeb
{

    Reader::Reader(boost::filesystem::path templateFile)
    {
        m_inFile = templateFile;
        m_outFile = templateFile.parent_path();
        m_outFile/="index.html";        
    }

    Reader::~Reader()
    {
    }

    int Reader::Run()
    {
        FILE* outStream = fopen(m_outFile.string().c_str(), "w");
        FILE* inStream = fopen(m_inFile.string().c_str(), "r");

        if (inStream == NULL)
        {
            std::wcout << "Failed to open file: " << m_inFile.string().c_str() << std::endl;
            return -1;
        }


        const char * const part1 = "<li>Entities";
        const char * const part2 = "<li>Messages";
        const char * const part3 = "<li>Services"; 
        const char * const part4 = "<li>Responses"; 
        const char * const part5 = "<li>Parameters"; 
        const char * const part6 = "<li>Items"; 
        const char * const part7 = "<li>Structs"; 
        const char * const part8 = "<li>Properties"; 
        const char * const part9 = "<li>Exceptions"; 
        const char * const part10 = "<li>Enumerations"; 
        const char * const part11 = "<li>Property mappings"; 
        const char * const part12 = "<li>All"; 
        const int buff_size = 128;
        char line[128];

        // find 
        while( fgets( line, buff_size, inStream ) != NULL)
        {
            fwrite(line, sizeof( char ), strlen(line), outStream);

            if (strncmp(part1, line, strlen(part1)) == 0)
            {
                Write(outStream, Operations::GetClassTree(Safir::Dob::Entity::ClassTypeId));
            }
            else if (strncmp(part2, line, strlen(part2)) == 0)
            {
                Write(outStream, Operations::GetClassTree(Safir::Dob::Message::ClassTypeId));
            }
            else if (strncmp(part3, line, strlen(part3)) == 0)
            {
                Write(outStream, Operations::GetClassTree(Safir::Dob::Service::ClassTypeId));
            }
            else if (strncmp(part4, line, strlen(part4)) == 0)
            {
                Write(outStream, Operations::GetClassTree(Safir::Dob::Response::ClassTypeId));
            }
            else if (strncmp(part5, line, strlen(part5)) == 0)
            {
                Write(outStream, Operations::GetClassTree(Safir::Dob::Parametrization::ClassTypeId));
            }
            else if (strncmp(part6, line, strlen(part6)) == 0)
            {
                Write(outStream, Operations::GetClassTree(Safir::Dob::Item::ClassTypeId));
            }
            else if (strncmp(part7, line, strlen(part7)) == 0)
            {
                Write(outStream, Operations::GetClassTree(Safir::Dob::Struct::ClassTypeId));
            }
            else if (strncmp(part8, line, strlen(part8)) == 0)
            {
                Safir::Dob::Typesystem::TypeIdVector vect = Operations::GetAllTypeIds();
                Safir::Dob::Typesystem::TypeIdVector v;
                TypeIdVector::iterator iter = vect.begin();
                
                while (iter != vect.end())
                {
                    if (Operations::IsProperty(*iter))
                    {
                        v.push_back(*iter);
                    }
                    ++iter;
                }   
                Write(outStream, v);
            }
            else if (strncmp(part9, line, strlen(part9)) == 0)
            {                
               Safir::Dob::Typesystem::TypeIdVector vect = Operations::GetAllTypeIds();
                Safir::Dob::Typesystem::TypeIdVector v;
                TypeIdVector::iterator iter = vect.begin();
                
                while (iter != vect.end())
                {
                    if (Operations::IsException(*iter))
                    {
                        v.push_back(*iter);
                    }
                    ++iter;
                }   
                Write(outStream, v);
            }
            else if (strncmp(part10, line, strlen(part10)) == 0)
            {
                Safir::Dob::Typesystem::TypeIdVector vect = Operations::GetAllTypeIds();
                Safir::Dob::Typesystem::TypeIdVector v;
                TypeIdVector::iterator iter = vect.begin();

                while (iter != vect.end())
                {
                    if (Operations::IsEnumeration(*iter))
                    {
                        v.push_back(*iter);
                    }
                    ++iter;
                }   
                Write(outStream, v);
            }
            else if (strncmp(part11, line, strlen(part11)) == 0)
            {
                
            }
            else if (strncmp(part12, line, strlen(part12)) == 0)
            {
                Write(outStream, Operations::GetAllTypeIds());
            }
        }

        fclose(inStream);
        fclose(outStream);
        return 0;
    }

    void Reader::Write(FILE* outStream, Safir::Dob::Typesystem::TypeIdVector vect)
    {
        std::vector<std::wstring> v;

        TypeIdVector::iterator iter = vect.begin();

        while (iter != vect.end())
        {
            v.push_back(Operations::GetName(*iter));
            ++iter;
        }   

        sort(v.begin(), v.end());

        fputs("<ul>",outStream);

        std::vector<std::wstring>::iterator iter2 = v.begin();
        while (iter2 != v.end())
        {
            fputs("<li>", outStream);

            fputs("<A HREF=\"show?", outStream);
            fputws((*iter2).c_str(), outStream);
            fputs("\"> ", outStream);
            fputws((*iter2).c_str(), outStream);
            fputs("</A><br>", outStream);


            fputs("</li>\n", outStream);
            ++iter2;
        }   

        fputs("</ul>\n", outStream);

    }

}
