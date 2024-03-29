/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / stlrha
*
******************************************************************************/
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <fstream>
#include <iostream>

#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

std::vector<char> xml;

void access()
{
    char* blob = NULL;
    DotsC_BytePointerDeleter deleter;
    DotsC_XmlToBlob(blob,deleter,&xml[0]);
    std::vector<char> xml2(100000);
    DotsC_Int32 resultsize = 0;
    DotsC_BlobToXml(&xml2[0],blob,100000,resultsize);
    deleter(blob);
}

int main(int argc, char* argv[])
{
    //read xml
    if (argc != 2)
    {
        std::wcout << "expect arg" << std::endl;
        return 1;
    }

    std::ifstream obj(argv[1]);
    for (;;)
    {
        int c = obj.get();
        if (!obj.good())
        {
            break;
        }
        xml.push_back(static_cast<char>(c));
    }
    if (xml.empty())
    {
        std::wcout << "Empty object!" << std::endl;
        return 1;
    }
    xml.push_back(0);

    //let dots_kernel load in peace.
    DotsC_NumberOfTypeIds();

    boost::thread_group tg;
    for (int i = 0; i < 50; ++i)
    {
        tg.create_thread([]{access();});
    }
    tg.join_all();

    return 0;
}


