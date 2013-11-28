/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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

#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <iostream>
#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4702)
#endif

void ListAllTypes();
void ShowType(const char* name);
void ShowClass(DotsC_TypeId tid);
void ShowEnum(DotsC_TypeId tid);
void ShowProperty(DotsC_TypeId tid);
void ShowException(DotsC_TypeId tid);

int main(int /*argc*/, char* /*argv*/[])
{
    std::wcout<<"Checking configuration..."<<std::endl;

    try
    {        
        std::wcout<<"Number of types: "<<DotsC_NumberOfTypeIds()<<std::endl;
    }
    catch (const std::exception & exc)
    {
        std::wcout << "Failed with exception description: " << exc.what() << std::endl;
        return 1;
    }
    catch (...)
    {
        std::wcout << "Failed with ... exception." << std::endl;
        return 1;
    }

    //ListAllTypes();

    std::wcout<<"Success!"<<std::endl;

    return 0;
}

void ListAllTypes()
{
    DotsC_TypeId tid[3000];
    int size;
    DotsC_GetAllTypeIds(tid, 3000, size);
    if (size==0)
    {
        std::wcout<<"No types found!"<<std::endl;
    }
    for (int i=0; i<size; ++i)
    {
        const char* n=DotsC_GetTypeName(tid[i]);
        ShowType(n);
        std::wcout<<"- - - - -"<<std::endl;
        //std::wcout<<n<<" ["<<tid[i]<<"]"<<std::endl;
    }
}

void ShowType(const char* name)
{
    DotsC_TypeId tid=DotsC_TypeIdFromName(name);
    if (DotsC_IsClass(tid))
    {
        ShowClass(tid);
    }
    else if (DotsC_IsEnumeration(tid))
    {
        ShowEnum(tid);
    }
    else if (DotsC_IsProperty(tid))
    {
        ShowProperty(tid);
    }
    else if (DotsC_IsException(tid))
    {
        ShowException(tid);
    }
}

void ShowClass(DotsC_TypeId tid)
{
    std::wcout<<"Class: "<<DotsC_GetTypeName(tid)<<" ["<<tid<<"]"<<std::endl;

    int numMem=DotsC_GetNumberOfMembers(tid);
    if (numMem>0)
    {
        std::wcout<<"  Members"<<std::endl;
    }
    for (int i=0; i<numMem; ++i)
    {
        const char* name;
        DotsC_MemberType mt;
        DotsC_TypeId complexTid;
        int strLen;
        bool isArray;
        int arrSize;
        DotsC_GetMemberInfo(tid, i, mt, name, complexTid, strLen, isArray, arrSize);
        std::wcout<<"    "<<i<<". "<<DotsC_GetMemberTypeName(tid, i)<<" "<<name<<std::endl;
    }

    int numParams=DotsC_GetNumberOfParameters(tid);
    if (numParams>0)
    {
        std::wcout<<"  Parameters"<<std::endl;
    }
    for (int i=0; i<numParams; ++i)
    {
        const char* n=DotsC_GetParameterName(tid, i);
        std::wcout<<"    "<<i<<". "<<n<<std::endl;
    }
}

void ShowEnum(DotsC_TypeId tid)
{
    std::wcout<<"Enum: "<<DotsC_GetTypeName(tid)<<" ["<<tid<<"]"<<std::endl;

    int numVal=DotsC_GetNumberOfEnumerationValues(tid);
    for (int i=0; i<numVal; ++i)
    {
        const char* n=DotsC_GetEnumerationValueName(tid, i);
        std::wcout<<"  Value "<<i<<": "<<n<<std::endl;
    }
}

void ShowProperty(DotsC_TypeId tid)
{
    std::wcout<<"Property: "<<DotsC_GetTypeName(tid)<<" ["<<tid<<"]"<<std::endl;
}

void ShowException(DotsC_TypeId tid)
{
    std::wcout<<"Exception: "<<DotsC_GetTypeName(tid)<<" ["<<tid<<"]"<<std::endl;
}
