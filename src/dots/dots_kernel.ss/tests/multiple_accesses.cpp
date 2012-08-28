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

void access()
{
    DotsC_NumberOfTypeIds();
}

int main(int argc, char* argv[])
{
    if (argc == 2 && std::string(argv[1]) == "sleep")
    {
        DotsC_NumberOfTypeIds();
        boost::this_thread::sleep(boost::posix_time::seconds(5));
    }
    boost::thread_group tg;
    for (int i = 0; i < 50; ++i)
    {
        tg.create_thread(boost::bind(access));
    }
    tg.join_all();
    return 0;
}


