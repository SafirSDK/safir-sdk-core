/******************************************************************************
*
* Copyright Saab Systems AB, 2012 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
*
******************************************************************************/
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include <fstream>
#include <iostream>

std::vector<char> xml;

void access()
{
    char* blob = NULL;
    DotsC_BytePointerDeleter deleter;
    DotsC_XmlToBlob(blob,deleter,&xml[0]);
    char xml2[1000000];
    DotsC_Int32 resultsize = 0;
    DotsC_BetterBlobToXml(xml2,blob,1000000,resultsize);
    deleter(blob);
}

int main(int argc, char* argv[])
{
    //read xml
    std::ifstream obj("obj.xml");
    for (;;)
    {
        int c = obj.get();
        if (!obj.good())
        {
            break;
        }
        xml.push_back(c);
    }
    xml.push_back(0);

    //let dots_kernel load in peace.
    DotsC_NumberOfTypeIds();
    boost::this_thread::sleep(boost::posix_time::seconds(5));

    boost::thread_group tg;
    for (int i = 0; i < 50; ++i)
    {
        tg.create_thread(boost::bind(access));
    }
    tg.join_all();

    return 0;
}


