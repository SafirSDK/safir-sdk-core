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

#if 0
#include "dots_param.h"
#include "dots_internal_defs.h"
#include <expat/expat.h>
#include <windows.h>
#include <string>
#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    typedef enum
    {
        MemoryPool_Param_EL,
        Group_Param_El,
        NoBlocks_Param_El,
        Size_Param_El
    } PoolElements;

    static PoolElements lastStart;
    static adty::BlockGroup group;
    static adty::BlockGroupVector gv;

    static void XMLCALL MemParamStartElement(void * /*userData*/, const char *name, const char ** /*atts*/)
    {
        std::string s=name;
        if (s=="MemoryPool")
            lastStart=MemoryPool_Param_EL;
        else if(s=="Group")
            lastStart=Group_Param_El;
        else if(s=="NoBlocks")
            lastStart=NoBlocks_Param_El;
        else if(s=="Size")
            lastStart=Size_Param_El;
    }

    static void XMLCALL MemParamEndElement(void * /*userData*/, const char * /*name*/)
    {
    }

    static void XMLCALL MemParamCharacters(void * /*userData*/, const XML_Char *s, int len)
    {
        //Trim string
        int startpos=0;
        int endpos=len-1;
        while((unsigned)s[startpos]<33 && startpos<len) startpos++;
        while((unsigned)s[endpos]<33 && endpos>=0) endpos--;

        //check content
        if (startpos>endpos)
            return;

        std::string str="";
        for (int i=startpos; i<=endpos; i++)
                str+=s[i];

        if (lastStart==NoBlocks_Param_El)
        {
            group.NumberOfBlocks=atoi(str.c_str());
        }
        else if (lastStart==Size_Param_El)
        {
            group.Size=atoi(str.c_str());
            gv.push_back(group);
        }
    }

    adty::BlockGroupVector Param::MemoryPoolParameters()
    {
        gv.clear();

        //traverse directory
        std::string filename=getenv(RUNTIME_ENV);
        filename+=DATA_FILE;

        FILE* stream=fopen(filename.c_str(), "r");
        if (stream==NULL)
        {
            std::wcout<<"Failed to open file: "<<filename<<std::endl;
            return gv;
        }

        char buf[BUFSIZ];
        XML_Parser parser = XML_ParserCreate(NULL);
        bool done = false;
        int depth = 0;
        XML_SetUserData(parser, &depth);
        XML_SetElementHandler(parser, MemParamStartElement, MemParamEndElement);
        XML_SetCharacterDataHandler(parser, MemParamCharacters);

        while (!done)
        {
            size_t len = fread(buf, 1, sizeof(buf), stream);
            done = len < sizeof(buf);
            if (XML_Parse(parser, buf, static_cast<int>(len), done) == XML_STATUS_ERROR)
            {
                std::wcout<<"XML parse error: "<<XML_ErrorString(XML_GetErrorCode(parser))<<", line: "<<XML_GetCurrentLineNumber(parser)<<std::endl;
                return gv;
            }
        }

        XML_ParserFree(parser);
        return gv;
    }

}
}
}
}
#endif
